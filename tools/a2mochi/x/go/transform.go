//go:build slow

package gox

import (
	"fmt"
	"go/ast"
	"go/token"
	"strconv"
	"strings"

	mast "mochi/ast"
)

// Transform converts a parsed Go Program into a Mochi AST node.
func Transform(p *Program) (*mast.Node, error) {
	if p == nil {
		return nil, fmt.Errorf("nil program")
	}

	root := &mast.Node{Kind: "program"}

	for _, decl := range p.File.Decls {
		switch d := decl.(type) {
		case *ast.GenDecl:
			switch d.Tok {
			case token.VAR:
				for _, sp := range d.Specs {
					vs, ok := sp.(*ast.ValueSpec)
					if !ok || len(vs.Names) != 1 {
						continue
					}
					name := vs.Names[0].Name
					var val *mast.Node
					switch len(vs.Values) {
					case 1:
						val = transformExpr(vs.Values[0])
					case 0:
						if vs.Type != nil {
							val = transformType(vs.Type)
						} else {
							val = defaultValue(nil)
						}
					default:
						continue
					}
					root.Children = append(root.Children, &mast.Node{
						Kind:     "var",
						Value:    name,
						Children: []*mast.Node{val},
					})
				}
			case token.TYPE:
				for _, sp := range d.Specs {
					ts, ok := sp.(*ast.TypeSpec)
					if !ok {
						continue
					}
					st, ok := ts.Type.(*ast.StructType)
					if !ok {
						continue
					}
					tn := &mast.Node{Kind: "type", Value: ts.Name.Name}
					for _, f := range st.Fields.List {
						for _, n := range f.Names {
							fld := &mast.Node{Kind: "field", Value: fieldName(n.Name, f)}
							if f.Type != nil {
								fld.Children = []*mast.Node{transformType(f.Type)}
							}
							tn.Children = append(tn.Children, fld)
						}
					}
					root.Children = append(root.Children, tn)
				}
			}
		case *ast.FuncDecl:
			if d.Body == nil {
				continue
			}
			if d.Name.Name == "main" {
				for _, st := range d.Body.List {
					root.Children = append(root.Children, transformStmt(st)...)
				}
				continue
			}
			fn := &mast.Node{Kind: "fun", Value: d.Name.Name}
			for _, p := range d.Type.Params.List {
				for _, n := range p.Names {
					param := &mast.Node{Kind: "param", Value: n.Name}
					if p.Type != nil {
						param.Children = []*mast.Node{transformType(p.Type)}
					}
					fn.Children = append(fn.Children, param)
				}
			}
			if d.Type.Results != nil && len(d.Type.Results.List) == 1 {
				fn.Children = append(fn.Children, transformType(d.Type.Results.List[0].Type))
			}
			fn.Children = append(fn.Children, transformBlock(d.Body))
			root.Children = append(root.Children, fn)
		}
	}

	return root, nil
}

func transformStmt(st ast.Stmt) []*mast.Node {
	switch s := st.(type) {
	case *ast.ExprStmt:
		if call, ok := s.X.(*ast.CallExpr); ok {
			if n := transformCall(call); n != nil {
				return []*mast.Node{n}
			}
			if n := transformExpr(call); n != nil && n.Kind != "unknown" {
				return []*mast.Node{n}
			}
		}
	case *ast.AssignStmt:
		if len(s.Lhs) == 1 && len(s.Rhs) == 1 {
			lhs := transformExpr(s.Lhs[0])
			rhs := transformExpr(s.Rhs[0])
			if s.Tok == token.DEFINE {
				if id, ok := s.Lhs[0].(*ast.Ident); ok {
					return []*mast.Node{{
						Kind:     "let",
						Value:    id.Name,
						Children: []*mast.Node{rhs},
					}}
				}
			}
			return []*mast.Node{{
				Kind:     "assign",
				Children: []*mast.Node{lhs, rhs},
			}}
		}
	case *ast.DeclStmt:
		if gd, ok := s.Decl.(*ast.GenDecl); ok && gd.Tok == token.VAR {
			var nodes []*mast.Node
			for _, sp := range gd.Specs {
				vs, ok := sp.(*ast.ValueSpec)
				if !ok || len(vs.Names) != 1 || len(vs.Values) != 1 {
					continue
				}
				name := vs.Names[0].Name
				val := transformExpr(vs.Values[0])
				nodes = append(nodes, &mast.Node{Kind: "let", Value: name, Children: []*mast.Node{val}})
			}
			if len(nodes) > 0 {
				return nodes
			}
		}
	case *ast.ForStmt:
		body := transformBlock(s.Body)
		if s.Init == nil && s.Post == nil {
			return []*mast.Node{{
				Kind:     "while",
				Children: []*mast.Node{transformExpr(s.Cond), body},
			}}
		}
		if as, ok := s.Init.(*ast.AssignStmt); ok &&
			len(as.Lhs) == 1 && len(as.Rhs) == 1 &&
			as.Tok == token.DEFINE {
			if inc, ok := s.Post.(*ast.IncDecStmt); ok && inc.Tok == token.INC {
				if id, ok := as.Lhs[0].(*ast.Ident); ok {
					varName := id.Name
					if xid, ok := inc.X.(*ast.Ident); ok && xid.Name == varName {
						if be, ok := s.Cond.(*ast.BinaryExpr); ok {
							if lhs, ok := be.X.(*ast.Ident); ok && lhs.Name == varName {
								start := transformExpr(as.Rhs[0])
								end := transformExpr(be.Y)
								rng := &mast.Node{Kind: "range", Children: []*mast.Node{start, end}}
								return []*mast.Node{{
									Kind:     "for",
									Value:    varName,
									Children: []*mast.Node{rng, body},
								}}
							}
						}
					}
				}
			}
		}
		return []*mast.Node{{
			Kind:     "while",
			Children: []*mast.Node{transformExpr(s.Cond), body},
		}}
	case *ast.RangeStmt:
		varName := "_"
		if id, ok := s.Value.(*ast.Ident); ok {
			varName = id.Name
		}
		iterExpr := s.X
		if call, ok := s.X.(*ast.CallExpr); ok {
			if fn, ok := call.Fun.(*ast.FuncLit); ok {
				if m := tryMapKeysIIFE(fn); m != nil {
					iterExpr = m
				}
			}
		}
		iter := transformExpr(iterExpr)
		body := transformBlock(s.Body)
		return []*mast.Node{{
			Kind:  "for",
			Value: varName,
			Children: []*mast.Node{
				{Kind: "in", Children: []*mast.Node{iter}},
				body,
			},
		}}
	case *ast.BranchStmt:
		switch s.Tok {
		case token.BREAK:
			return []*mast.Node{{Kind: "break"}}
		case token.CONTINUE:
			return []*mast.Node{{Kind: "continue"}}
		}
	case *ast.IfStmt:
		cond := transformExpr(s.Cond)
		thenBlk := transformBlock(s.Body)
		var elseBlk *mast.Node
		if s.Else != nil {
			if eb, ok := s.Else.(*ast.BlockStmt); ok {
				elseBlk = transformBlock(eb)
			}
		}
		n := &mast.Node{Kind: "if", Children: []*mast.Node{cond, thenBlk}}
		if elseBlk != nil {
			n.Children = append(n.Children, elseBlk)
		}
		return []*mast.Node{n}
	case *ast.ReturnStmt:
		if len(s.Results) == 1 {
			return []*mast.Node{{
				Kind:     "return",
				Children: []*mast.Node{transformExpr(s.Results[0])},
			}}
		}
		return []*mast.Node{{Kind: "return"}}
	}
	return nil
}

func transformBlock(b *ast.BlockStmt) *mast.Node {
	if b == nil {
		return &mast.Node{Kind: "block"}
	}
	blk := &mast.Node{Kind: "block"}
	for _, st := range b.List {
		blk.Children = append(blk.Children, transformStmt(st)...)
	}
	return blk
}

func transformCall(c *ast.CallExpr) *mast.Node {
	switch fn := c.Fun.(type) {
	case *ast.SelectorExpr:
		if id, ok := fn.X.(*ast.Ident); ok && id.Name == "fmt" {
			switch fn.Sel.Name {
			case "Println":
				n := &mast.Node{Kind: "call", Value: "print"}
				for _, arg := range c.Args {
					n.Children = append(n.Children, transformExpr(arg))
				}
				return n
			case "Sprint":
				n := &mast.Node{Kind: "call", Value: "str"}
				for _, arg := range c.Args {
					n.Children = append(n.Children, transformExpr(arg))
				}
				return n
			}
		}
		if id, ok := fn.X.(*ast.Ident); ok && id.Name == "strings" {
			if fn.Sel.Name == "Contains" {
				return &mast.Node{Kind: "binary", Value: "in", Children: []*mast.Node{
					transformExpr(c.Args[1]),
					transformExpr(c.Args[0]),
				}}
			}
		}
	case *ast.Ident:
		switch fn.Name {
		case "len", "append", "sum", "min", "max":
			n := &mast.Node{Kind: "call", Value: fn.Name}
			for _, arg := range c.Args {
				n.Children = append(n.Children, transformExpr(arg))
			}
			return n
		case "string":
			return nil
		}
		n := &mast.Node{Kind: "call", Value: fn.Name}
		for _, arg := range c.Args {
			n.Children = append(n.Children, transformExpr(arg))
		}
		return n
	}
	return nil
}

// tryIIFE attempts to unwrap immediately-invoked function expressions that
// return a simple expression or if/else chain. It returns nil if the call does
// not match a supported pattern.
func tryIIFE(c *ast.CallExpr) *mast.Node {
	if len(c.Args) != 0 {
		return nil
	}
	fn, ok := c.Fun.(*ast.FuncLit)
	if !ok {
		return nil
	}
	if fn.Type != nil && fn.Type.Params != nil && len(fn.Type.Params.List) > 0 {
		return nil
	}
	if len(fn.Body.List) == 1 {
		switch st := fn.Body.List[0].(type) {
		case *ast.ReturnStmt:
			if len(st.Results) == 1 {
				return transformExpr(st.Results[0])
			}
		case *ast.IfStmt:
			if n := transformIfExpr(st); n != nil {
				return n
			}
		}
	}
	if len(fn.Body.List) == 2 {
		// match `n, _ := strconv.Atoi(arg); return n`
		if as, ok := fn.Body.List[0].(*ast.AssignStmt); ok && as.Tok == token.DEFINE && len(as.Lhs) == 2 && len(as.Rhs) == 1 {
			if id, ok := as.Lhs[0].(*ast.Ident); ok {
				if ret, ok := fn.Body.List[1].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
					if rid, ok := ret.Results[0].(*ast.Ident); ok && rid.Name == id.Name {
						if call, ok := as.Rhs[0].(*ast.CallExpr); ok {
							if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
								if pkg, ok := sel.X.(*ast.Ident); ok && pkg.Name == "strconv" && sel.Sel.Name == "Atoi" && len(call.Args) == 1 {
									return &mast.Node{Kind: "cast", Children: []*mast.Node{transformExpr(call.Args[0]), &mast.Node{Kind: "type", Value: "int"}}}
								}
							}
						}
					}
				}
			}
		}
		if _, ok := fn.Body.List[0].(*ast.RangeStmt); ok {
			if n := tryListMembership(fn); n != nil {
				return n
			}
		}
		if n := tryMapMembership(fn); n != nil {
			return n
		}
	}

	// match formatting wrapper for avg_builtin
	if len(fn.Body.List) == 3 {
		if as, ok := fn.Body.List[0].(*ast.AssignStmt); ok && as.Tok == token.DEFINE && len(as.Lhs) == 1 && len(as.Rhs) == 1 {
			if call, ok := as.Rhs[0].(*ast.CallExpr); ok {
				if id, ok := call.Fun.(*ast.Ident); ok && id.Name == "float64" && len(call.Args) == 1 {
					if inner, ok := call.Args[0].(*ast.CallExpr); ok {
						if n := tryIIFE(inner); n != nil {
							if _, ok := fn.Body.List[1].(*ast.IfStmt); ok {
								if _, ok := fn.Body.List[2].(*ast.ReturnStmt); ok {
									return n
								}
							}
						}
					}
				}
			}
		}
	}

	if n := tryJSONMarshal(fn); n != nil {
		return n
	}
	if n := tryJSONPrint(fn); n != nil {
		return n
	}
	if n := trySum(fn); n != nil {
		return n
	}
	if n := tryMinMax(fn); n != nil {
		return n
	}
	if n := tryAvg(fn); n != nil {
		return n
	}
	return nil
}

// transformIfExpr converts nested if statements that return expressions into a
// Mochi if_expr node.
func transformIfExpr(ifs *ast.IfStmt) *mast.Node {
	cond := transformExpr(ifs.Cond)
	thenNode := returnExprFromBlock(ifs.Body)
	if thenNode == nil {
		return nil
	}
	var elseNode *mast.Node
	switch el := ifs.Else.(type) {
	case *ast.BlockStmt:
		elseNode = returnExprFromBlock(el)
	case *ast.IfStmt:
		elseNode = transformIfExpr(el)
	}
	if elseNode == nil {
		return nil
	}
	return &mast.Node{Kind: "if_expr", Children: []*mast.Node{cond, thenNode, elseNode}}
}

func returnExprFromBlock(b *ast.BlockStmt) *mast.Node {
	if b == nil || len(b.List) != 1 {
		return nil
	}
	if ret, ok := b.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
		return transformExpr(ret.Results[0])
	}
	return nil
}

// tryListMembership matches loops that test for value membership in a slice.
func tryListMembership(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) != 2 {
		return nil
	}
	rng, ok := fn.Body.List[0].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	ifs, ok := rng.Body.List[0].(*ast.IfStmt)
	if !ok || len(ifs.Body.List) != 1 {
		return nil
	}
	retTrue, ok := ifs.Body.List[0].(*ast.ReturnStmt)
	if !ok || len(retTrue.Results) != 1 {
		return nil
	}
	if id, ok := retTrue.Results[0].(*ast.Ident); !ok || id.Name != "true" {
		return nil
	}
	retFalse, ok := fn.Body.List[1].(*ast.ReturnStmt)
	if !ok || len(retFalse.Results) != 1 {
		return nil
	}
	if id, ok := retFalse.Results[0].(*ast.Ident); !ok || id.Name != "false" {
		return nil
	}
	be, ok := ifs.Cond.(*ast.BinaryExpr)
	if !ok || be.Op != token.EQL {
		return nil
	}
	valIdent, ok := rng.Value.(*ast.Ident)
	if !ok {
		return nil
	}
	lhs, ok := be.X.(*ast.Ident)
	if !ok || lhs.Name != valIdent.Name {
		return nil
	}
	return &mast.Node{Kind: "binary", Value: "in", Children: []*mast.Node{
		transformExpr(be.Y),
		transformExpr(rng.X),
	}}
}

// tryMapMembership matches `_, ok := m[key]; return ok` patterns.
func tryMapMembership(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) != 2 {
		return nil
	}
	as, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || len(as.Lhs) != 2 || len(as.Rhs) != 1 {
		return nil
	}
	if as.Tok != token.DEFINE && as.Tok != token.ASSIGN {
		return nil
	}
	okIdent, ok := as.Lhs[1].(*ast.Ident)
	if !ok || okIdent.Name != "ok" {
		return nil
	}
	idx, ok := as.Rhs[0].(*ast.IndexExpr)
	if !ok {
		return nil
	}
	ret, ok := fn.Body.List[1].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	rident, ok := ret.Results[0].(*ast.Ident)
	if !ok || rident.Name != okIdent.Name {
		return nil
	}
	return &mast.Node{Kind: "binary", Value: "in", Children: []*mast.Node{
		transformExpr(idx.Index),
		transformExpr(idx.X),
	}}
}

// tryJSONMarshal unwraps `json.Marshal` helper functions used to pretty print
// values. It expects the function body to marshal an expression to JSON and
// ultimately return the resulting string. The returned Mochi node is the
// marshalled expression itself.
func tryJSONMarshal(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) < 2 {
		return nil
	}
	as, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || as.Tok != token.DEFINE || len(as.Lhs) != 2 || len(as.Rhs) != 1 {
		return nil
	}
	bIdent, ok := as.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	call, ok := as.Rhs[0].(*ast.CallExpr)
	if !ok {
		return nil
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return nil
	}
	pkg, ok := sel.X.(*ast.Ident)
	if !ok || pkg.Name != "json" || sel.Sel.Name != "Marshal" || len(call.Args) != 1 {
		return nil
	}
	ret, ok := fn.Body.List[len(fn.Body.List)-1].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	switch rv := ret.Results[0].(type) {
	case *ast.CallExpr:
		if id, ok := rv.Fun.(*ast.Ident); !ok || id.Name != "string" || len(rv.Args) != 1 {
			return nil
		}
		if arg, ok := rv.Args[0].(*ast.Ident); !ok || arg.Name != bIdent.Name {
			return nil
		}
	case *ast.Ident:
		found := false
		for _, st := range fn.Body.List[1 : len(fn.Body.List)-1] {
			if as2, ok := st.(*ast.AssignStmt); ok && as2.Tok == token.DEFINE && len(as2.Lhs) == 1 && len(as2.Rhs) == 1 {
				if id2, ok := as2.Lhs[0].(*ast.Ident); ok && id2.Name == rv.Name {
					if call2, ok := as2.Rhs[0].(*ast.CallExpr); ok {
						if fid, ok := call2.Fun.(*ast.Ident); ok && fid.Name == "string" && len(call2.Args) == 1 {
							if b, ok := call2.Args[0].(*ast.Ident); ok && b.Name == bIdent.Name {
								found = true
							}
						}
					}
				}
			}
		}
		if !found {
			return nil
		}
	default:
		return nil
	}
	return transformExpr(call.Args[0])
}

// tryJSONPrint matches a function that marshals a value to JSON and prints it.
func tryJSONPrint(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) != 2 {
		return nil
	}
	as, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || as.Tok != token.DEFINE || len(as.Lhs) != 2 || len(as.Rhs) != 1 {
		return nil
	}
	bIdent, ok := as.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	call, ok := as.Rhs[0].(*ast.CallExpr)
	if !ok {
		return nil
	}
	sel, ok := call.Fun.(*ast.SelectorExpr)
	if !ok {
		return nil
	}
	pkg, ok := sel.X.(*ast.Ident)
	if !ok || pkg.Name != "json" || (sel.Sel.Name != "MarshalIndent" && sel.Sel.Name != "Marshal") || len(call.Args) == 0 {
		return nil
	}
	if stmt, ok := fn.Body.List[1].(*ast.ExprStmt); ok {
		if pcall, ok := stmt.X.(*ast.CallExpr); ok {
			if sel2, ok := pcall.Fun.(*ast.SelectorExpr); ok {
				if pkg2, ok := sel2.X.(*ast.Ident); ok && pkg2.Name == "fmt" && sel2.Sel.Name == "Println" && len(pcall.Args) == 1 {
					if inner, ok := pcall.Args[0].(*ast.CallExpr); ok {
						if id, ok := inner.Fun.(*ast.Ident); ok && id.Name == "string" && len(inner.Args) == 1 {
							if arg, ok := inner.Args[0].(*ast.Ident); ok && arg.Name == bIdent.Name {
								return &mast.Node{Kind: "call", Value: "json", Children: []*mast.Node{transformExpr(call.Args[0])}}
							}
						}
					}
				}
			}
		}
	}
	return nil
}

// trySum matches `s := 0; for _, v := range xs { s += v }; return s`.
func trySum(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) != 3 {
		return nil
	}
	init, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || init.Tok != token.DEFINE || len(init.Lhs) != 1 || len(init.Rhs) != 1 {
		return nil
	}
	accIdent, ok := init.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	if lit, ok := init.Rhs[0].(*ast.BasicLit); !ok || lit.Value != "0" {
		return nil
	}
	rng, ok := fn.Body.List[1].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	add, ok := rng.Body.List[0].(*ast.AssignStmt)
	if !ok || add.Tok != token.ADD_ASSIGN || len(add.Lhs) != 1 || len(add.Rhs) != 1 {
		return nil
	}
	if id, ok := add.Lhs[0].(*ast.Ident); !ok || id.Name != accIdent.Name {
		return nil
	}
	ret, ok := fn.Body.List[2].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	if id, ok := ret.Results[0].(*ast.Ident); !ok || id.Name != accIdent.Name {
		return nil
	}
	return &mast.Node{Kind: "call", Value: "sum", Children: []*mast.Node{transformExpr(rng.X)}}
}

// tryMinMax matches loops computing the minimum or maximum of a slice.
func tryMinMax(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) < 3 {
		return nil
	}
	idx := 0
	// optional len check
	if ifs, ok := fn.Body.List[0].(*ast.IfStmt); ok {
		if be, ok := ifs.Cond.(*ast.BinaryExpr); ok && be.Op == token.EQL {
			if call, ok := be.X.(*ast.CallExpr); ok {
				if id, ok := call.Fun.(*ast.Ident); ok && id.Name == "len" && len(call.Args) == 1 {
					if lit, ok := be.Y.(*ast.BasicLit); ok && lit.Value == "0" {
						if len(ifs.Body.List) == 1 {
							if ret0, ok := ifs.Body.List[0].(*ast.ReturnStmt); ok && len(ret0.Results) == 1 {
								if lit0, ok := ret0.Results[0].(*ast.BasicLit); ok && lit0.Value == "0" {
									idx = 1
								}
							}
						}
					}
				}
			}
		}
	}
	if len(fn.Body.List[idx:]) != 3 {
		return nil
	}
	as, ok := fn.Body.List[idx].(*ast.AssignStmt)
	if !ok || as.Tok != token.DEFINE || len(as.Lhs) != 1 || len(as.Rhs) != 1 {
		return nil
	}
	accIdent, ok := as.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	firstIdx, ok := as.Rhs[0].(*ast.IndexExpr)
	if !ok {
		return nil
	}
	base := firstIdx.X
	rng, ok := fn.Body.List[idx+1].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	sliceExpr, ok := rng.X.(*ast.SliceExpr)
	if !ok || sliceExpr.Low == nil {
		return nil
	}
	if !astEqual(sliceExpr.X, base) {
		return nil
	}
	if lit, ok := sliceExpr.Low.(*ast.BasicLit); !ok || lit.Value != "1" {
		return nil
	}
	ifSt, ok := rng.Body.List[0].(*ast.IfStmt)
	if !ok || len(ifSt.Body.List) != 1 {
		return nil
	}
	assign, ok := ifSt.Body.List[0].(*ast.AssignStmt)
	if !ok || assign.Tok != token.ASSIGN || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return nil
	}
	if id, ok := assign.Lhs[0].(*ast.Ident); !ok || id.Name != accIdent.Name {
		return nil
	}
	valIdent, ok := rng.Value.(*ast.Ident)
	if !ok {
		return nil
	}
	if id, ok := assign.Rhs[0].(*ast.Ident); !ok || id.Name != valIdent.Name {
		return nil
	}
	cond, ok := ifSt.Cond.(*ast.BinaryExpr)
	if !ok {
		return nil
	}
	if lx, ok := cond.X.(*ast.Ident); !ok || lx.Name != valIdent.Name {
		return nil
	}
	if ly, ok := cond.Y.(*ast.Ident); !ok || ly.Name != accIdent.Name {
		return nil
	}
	ret, ok := fn.Body.List[idx+2].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	if rid, ok := ret.Results[0].(*ast.Ident); !ok || rid.Name != accIdent.Name {
		return nil
	}
	switch cond.Op {
	case token.LSS:
		return &mast.Node{Kind: "call", Value: "min", Children: []*mast.Node{transformExpr(base)}}
	case token.GTR:
		return &mast.Node{Kind: "call", Value: "max", Children: []*mast.Node{transformExpr(base)}}
	}
	return nil
}

// tryAvg matches `sum := 0; for _, v := range xs { sum += v }; return float64(sum) / float64(len(xs))`.
func tryAvg(fn *ast.FuncLit) *mast.Node {
	if len(fn.Body.List) != 3 {
		return nil
	}
	init, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || init.Tok != token.DEFINE || len(init.Lhs) != 1 || len(init.Rhs) != 1 {
		return nil
	}
	accIdent, ok := init.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	if lit, ok := init.Rhs[0].(*ast.BasicLit); !ok || lit.Value != "0" {
		return nil
	}
	rng, ok := fn.Body.List[1].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	add, ok := rng.Body.List[0].(*ast.AssignStmt)
	if !ok || add.Tok != token.ADD_ASSIGN || len(add.Lhs) != 1 || len(add.Rhs) != 1 {
		return nil
	}
	if id, ok := add.Lhs[0].(*ast.Ident); !ok || id.Name != accIdent.Name {
		return nil
	}
	ret, ok := fn.Body.List[2].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	be, ok := ret.Results[0].(*ast.BinaryExpr)
	if !ok || be.Op != token.QUO {
		return nil
	}
	// left operand must be float64(sum)
	leftCall, ok := be.X.(*ast.CallExpr)
	if !ok || len(leftCall.Args) != 1 {
		return nil
	}
	if lf, ok := leftCall.Fun.(*ast.Ident); !ok || lf.Name != "float64" {
		return nil
	}
	if arg, ok := leftCall.Args[0].(*ast.Ident); !ok || arg.Name != accIdent.Name {
		return nil
	}
	// right operand must be float64(len(xs))
	rightCall, ok := be.Y.(*ast.CallExpr)
	if !ok || len(rightCall.Args) != 1 {
		return nil
	}
	if rf, ok := rightCall.Fun.(*ast.Ident); !ok || rf.Name != "float64" {
		return nil
	}
	inner, ok := rightCall.Args[0].(*ast.CallExpr)
	if !ok || len(inner.Args) != 1 {
		return nil
	}
	if id, ok := inner.Fun.(*ast.Ident); !ok || id.Name != "len" {
		return nil
	}
	expr := transformExpr(inner.Args[0])
	if expr == nil {
		return nil
	}
	return &mast.Node{Kind: "call", Value: "avg", Children: []*mast.Node{expr}}
}

// tryMapKeysIIFE detects an immediately invoked function that builds and
// returns a slice of map keys sorted deterministically. It returns the map
// expression if the pattern matches.
func tryMapKeysIIFE(fn *ast.FuncLit) ast.Expr {
	if len(fn.Body.List) < 3 {
		return nil
	}
	// first statement: keys := make(...)
	init, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || init.Tok != token.DEFINE || len(init.Lhs) != 1 {
		return nil
	}
	keysIdent, ok := init.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	// second statement: for k := range m { keys = append(keys, k) }
	rng, ok := fn.Body.List[1].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	assign, ok := rng.Body.List[0].(*ast.AssignStmt)
	if !ok || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return nil
	}
	if id, ok := assign.Lhs[0].(*ast.Ident); !ok || id.Name != keysIdent.Name {
		return nil
	}
	call, ok := assign.Rhs[0].(*ast.CallExpr)
	if !ok || len(call.Args) != 2 {
		return nil
	}
	if fid, ok := call.Fun.(*ast.Ident); !ok || fid.Name != "append" {
		return nil
	}
	if a1, ok := call.Args[0].(*ast.Ident); !ok || a1.Name != keysIdent.Name {
		return nil
	}
	if a2, ok := call.Args[1].(*ast.Ident); !ok || rng.Key == nil {
		return nil
	} else if kid, ok2 := rng.Key.(*ast.Ident); !ok2 || a2.Name != kid.Name {
		return nil
	}
	// final statement: return keys
	ret, ok := fn.Body.List[len(fn.Body.List)-1].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	if rid, ok := ret.Results[0].(*ast.Ident); !ok || rid.Name != keysIdent.Name {
		return nil
	}
	return rng.X
}

// tryExists matches patterns like len(func(){ res:=[]T{}; for _,v:=range xs { if v == k { res = append(res,v) } }; return res }()) > 0
func tryExists(be *ast.BinaryExpr) *mast.Node {
	if be.Op != token.GTR || !isZeroLit(be.Y) {
		return nil
	}
	call, ok := be.X.(*ast.CallExpr)
	if !ok || len(call.Args) != 1 {
		return nil
	}
	if id, ok := call.Fun.(*ast.Ident); !ok || id.Name != "len" {
		return nil
	}
	inner, ok := call.Args[0].(*ast.CallExpr)
	if !ok || len(inner.Args) != 0 {
		return nil
	}
	fn, ok := inner.Fun.(*ast.FuncLit)
	if !ok || len(fn.Body.List) != 3 {
		return nil
	}
	init, ok := fn.Body.List[0].(*ast.AssignStmt)
	if !ok || init.Tok != token.DEFINE || len(init.Lhs) != 1 || len(init.Rhs) != 1 {
		return nil
	}
	resIdent, ok := init.Lhs[0].(*ast.Ident)
	if !ok {
		return nil
	}
	_, ok = init.Rhs[0].(*ast.CompositeLit)
	if !ok {
		return nil
	}
	rng, ok := fn.Body.List[1].(*ast.RangeStmt)
	if !ok || rng.Body == nil || len(rng.Body.List) != 1 {
		return nil
	}
	ifs, ok := rng.Body.List[0].(*ast.IfStmt)
	if !ok || len(ifs.Body.List) != 1 {
		return nil
	}
	assign, ok := ifs.Body.List[0].(*ast.AssignStmt)
	if !ok || assign.Tok != token.ASSIGN || len(assign.Lhs) != 1 || len(assign.Rhs) != 1 {
		return nil
	}
	if rid, ok := assign.Lhs[0].(*ast.Ident); !ok || rid.Name != resIdent.Name {
		return nil
	}
	if call2, ok := assign.Rhs[0].(*ast.CallExpr); ok {
		if fn, ok := call2.Fun.(*ast.Ident); !ok || fn.Name != "append" || len(call2.Args) != 2 {
			return nil
		}
	} else {
		return nil
	}
	ret, ok := fn.Body.List[2].(*ast.ReturnStmt)
	if !ok || len(ret.Results) != 1 {
		return nil
	}
	if id, ok := ret.Results[0].(*ast.Ident); !ok || id.Name != resIdent.Name {
		return nil
	}
	cond, ok := ifs.Cond.(*ast.BinaryExpr)
	if !ok || cond.Op != token.EQL {
		return nil
	}
	valIdent, ok := rng.Value.(*ast.Ident)
	if !ok {
		return nil
	}
	lhs, ok := cond.X.(*ast.Ident)
	if !ok || lhs.Name != valIdent.Name {
		return nil
	}
	return &mast.Node{Kind: "binary", Value: "in", Children: []*mast.Node{transformExpr(cond.Y), transformExpr(rng.X)}}
}

// astEqual reports whether two expressions refer to the same identifier.
func astEqual(a, b ast.Expr) bool {
	ida, ok := a.(*ast.Ident)
	idb, ok2 := b.(*ast.Ident)
	return ok && ok2 && ida.Name == idb.Name
}

func isIntLit(e ast.Expr) bool {
	if bl, ok := e.(*ast.BasicLit); ok {
		return bl.Kind == token.INT
	}
	return false
}

func isZeroLit(e ast.Expr) bool {
	if bl, ok := e.(*ast.BasicLit); ok && bl.Kind == token.INT {
		return bl.Value == "0"
	}
	return false
}

func transformExpr(e ast.Expr) *mast.Node {
	switch v := e.(type) {
	case *ast.BasicLit:
		switch v.Kind {
		case token.INT:
			iv, _ := strconv.Atoi(v.Value)
			return &mast.Node{Kind: "int", Value: iv}
		case token.FLOAT:
			fv, _ := strconv.ParseFloat(v.Value, 64)
			return &mast.Node{Kind: "float", Value: fv}
		case token.STRING:
			s, _ := strconv.Unquote(v.Value)
			return &mast.Node{Kind: "string", Value: s}
		case token.CHAR:
			r, _ := strconv.Unquote(v.Value)
			if len(r) > 0 {
				return &mast.Node{Kind: "int", Value: int(rune(r[0]))}
			}
			return &mast.Node{Kind: "int", Value: 0}
		}
	case *ast.Ident:
		return &mast.Node{Kind: "selector", Value: v.Name}
	case *ast.SelectorExpr:
		return &mast.Node{Kind: "selector", Value: strings.ToLower(v.Sel.Name), Children: []*mast.Node{transformExpr(v.X)}}
	case *ast.BinaryExpr:
		if n := tryExists(v); n != nil {
			return n
		}
		if v.Op == token.QUO && isIntLit(v.X) && isIntLit(v.Y) {
			div := &mast.Node{Kind: "binary", Value: v.Op.String(), Children: []*mast.Node{
				transformExpr(v.X), transformExpr(v.Y),
			}}
			return &mast.Node{Kind: "cast", Children: []*mast.Node{div, &mast.Node{Kind: "type", Value: "int"}}}
		}
		return &mast.Node{Kind: "binary", Value: v.Op.String(), Children: []*mast.Node{
			transformExpr(v.X), transformExpr(v.Y),
		}}
	case *ast.UnaryExpr:
		switch v.Op {
		case token.SUB:
			return &mast.Node{Kind: "unary", Value: "-", Children: []*mast.Node{transformExpr(v.X)}}
		case token.NOT:
			return &mast.Node{Kind: "unary", Value: "!", Children: []*mast.Node{transformExpr(v.X)}}
		}
	case *ast.CallExpr:
		if n := tryIIFE(v); n != nil {
			return n
		}
		if n := transformCall(v); n != nil {
			return n
		}
		if id, ok := v.Fun.(*ast.Ident); ok && id.Name == "string" && len(v.Args) == 1 {
			arg := v.Args[0]
			// string([]rune(s)[i]) or string([]rune(s)[start:end])
			switch a := arg.(type) {
			case *ast.IndexExpr:
				if sel, ok := a.X.(*ast.CallExpr); ok {
					if arr, ok := sel.Fun.(*ast.ArrayType); ok {
						if elt, ok := arr.Elt.(*ast.Ident); ok && elt.Name == "rune" && len(sel.Args) == 1 {
							src := transformExpr(sel.Args[0])
							if src != nil {
								idx := transformExpr(a.Index)
								one := &mast.Node{Kind: "int", Value: 1}
								end := &mast.Node{Kind: "binary", Value: "+", Children: []*mast.Node{idx, one}}
								return &mast.Node{Kind: "call", Value: "substring", Children: []*mast.Node{src, idx, end}}
							}
						}
					}
				}
			case *ast.SliceExpr:
				if sel, ok := a.X.(*ast.CallExpr); ok {
					if arr, ok := sel.Fun.(*ast.ArrayType); ok {
						if elt, ok := arr.Elt.(*ast.Ident); ok && elt.Name == "rune" && len(sel.Args) == 1 {
							src := transformExpr(sel.Args[0])
							if src != nil {
								start := transformExpr(a.Low)
								end := transformExpr(a.High)
								return &mast.Node{Kind: "call", Value: "substring", Children: []*mast.Node{src, start, end}}
							}
						}
					}
				}
			}
		}
		if fn, ok := v.Fun.(*ast.FuncLit); ok {
			if len(fn.Body.List) == 1 {
				if ifs, ok := fn.Body.List[0].(*ast.IfStmt); ok {
					if len(ifs.Body.List) == 1 {
						if ret, ok := ifs.Body.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
							if bl, ok := ret.Results[0].(*ast.BasicLit); ok && bl.Value == "1" {
								if ifs.Else != nil {
									if eb, ok := ifs.Else.(*ast.BlockStmt); ok && len(eb.List) == 1 {
										if ret0, ok := eb.List[0].(*ast.ReturnStmt); ok && len(ret0.Results) == 1 {
											if bl0, ok := ret0.Results[0].(*ast.BasicLit); ok && bl0.Value == "0" {
												return transformExpr(ifs.Cond)
											}
										}
									}
								}
							}
						}
					}
				}
			} else if len(fn.Body.List) == 2 {
				if ifs, ok := fn.Body.List[0].(*ast.IfStmt); ok {
					if len(ifs.Body.List) == 1 {
						if ret, ok := ifs.Body.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
							if bl, ok := ret.Results[0].(*ast.BasicLit); ok && bl.Value == "1" {
								if ret0, ok := fn.Body.List[1].(*ast.ReturnStmt); ok && len(ret0.Results) == 1 {
									if bl0, ok := ret0.Results[0].(*ast.BasicLit); ok && bl0.Value == "0" {
										return transformExpr(ifs.Cond)
									}
								}
							}
						}
					}
				}
			}
		}
		n := &mast.Node{Kind: "call"}
		n.Children = append(n.Children, transformExpr(v.Fun))
		for _, arg := range v.Args {
			n.Children = append(n.Children, transformExpr(arg))
		}
		return n
	case *ast.FuncLit:
		if len(v.Body.List) == 1 {
			if ret, ok := v.Body.List[0].(*ast.ReturnStmt); ok && len(ret.Results) == 1 {
				// partial application: return fnName(const, param)
				if call, ok := ret.Results[0].(*ast.CallExpr); ok {
					if id, ok := call.Fun.(*ast.Ident); ok {
						if v.Type != nil && v.Type.Params != nil && len(v.Type.Params.List) == 1 && len(call.Args) == 2 {
							if argIdent, ok := call.Args[1].(*ast.Ident); ok && len(v.Type.Params.List[0].Names) == 1 && argIdent.Name == v.Type.Params.List[0].Names[0].Name {
								return &mast.Node{Kind: "call", Value: id.Name, Children: []*mast.Node{transformExpr(call.Args[0])}}
							}
						}
					}
				}
				fn := &mast.Node{Kind: "funexpr"}
				if v.Type != nil {
					if v.Type.Params != nil {
						for _, p := range v.Type.Params.List {
							for _, name := range p.Names {
								param := &mast.Node{Kind: "param", Value: name.Name}
								if p.Type != nil {
									param.Children = []*mast.Node{transformType(p.Type)}
								}
								fn.Children = append(fn.Children, param)
							}
						}
					}
					if v.Type.Results != nil && len(v.Type.Results.List) == 1 {
						fn.Children = append(fn.Children, transformType(v.Type.Results.List[0].Type))
					}
				}
				fn.Children = append(fn.Children, transformExpr(ret.Results[0]))
				return fn
			}
		}
		return &mast.Node{Kind: "unknown"}
	case *ast.ParenExpr:
		return transformExpr(v.X)
	case *ast.CompositeLit:
		if v.Type == nil {
			m := &mast.Node{Kind: "map"}
			for _, e := range v.Elts {
				if kv, ok := e.(*ast.KeyValueExpr); ok {
					key := ""
					if id, ok := kv.Key.(*ast.Ident); ok {
						key = strings.ToLower(id.Name)
					} else if bl, ok := kv.Key.(*ast.BasicLit); ok {
						key, _ = strconv.Unquote(bl.Value)
					}
					m.Children = append(m.Children, &mast.Node{
						Kind:     "entry",
						Children: []*mast.Node{{Kind: "string", Value: key}, transformExpr(kv.Value)},
					})
				}
			}
			return m
		}
		switch t := v.Type.(type) {
		case *ast.ArrayType:
			n := &mast.Node{Kind: "list"}
			for _, e := range v.Elts {
				n.Children = append(n.Children, transformExpr(e))
			}
			return n
		case *ast.MapType:
			n := &mast.Node{Kind: "map"}
			for _, e := range v.Elts {
				if kv, ok := e.(*ast.KeyValueExpr); ok {
					n.Children = append(n.Children, &mast.Node{
						Kind:     "entry",
						Children: []*mast.Node{transformExpr(kv.Key), transformExpr(kv.Value)},
					})
				}
			}
			return n
		case *ast.Ident, *ast.StructType:
			m := &mast.Node{Kind: "map"}
			for _, e := range v.Elts {
				if kv, ok := e.(*ast.KeyValueExpr); ok {
					key := ""
					if id, ok := kv.Key.(*ast.Ident); ok {
						key = strings.ToLower(id.Name)
					} else if bl, ok := kv.Key.(*ast.BasicLit); ok {
						key, _ = strconv.Unquote(bl.Value)
					}
					m.Children = append(m.Children, &mast.Node{
						Kind:     "entry",
						Children: []*mast.Node{{Kind: "string", Value: key}, transformExpr(kv.Value)},
					})
				}
			}
			return &mast.Node{Kind: "cast", Children: []*mast.Node{m, transformType(t)}}
		}
	case *ast.IndexExpr:
		return &mast.Node{Kind: "index", Children: []*mast.Node{transformExpr(v.X), transformExpr(v.Index)}}
	case *ast.SliceExpr:
		n := &mast.Node{Kind: "index", Children: []*mast.Node{transformExpr(v.X)}}
		start := &mast.Node{Kind: "start"}
		if v.Low != nil {
			start.Children = []*mast.Node{transformExpr(v.Low)}
		}
		end := &mast.Node{Kind: "end"}
		if v.High != nil {
			end.Children = []*mast.Node{transformExpr(v.High)}
		}
		n.Children = append(n.Children, start, end)
		return n
	}
	return &mast.Node{Kind: "unknown"}
}

// defaultValue returns a simple default value node for the given Go type.
func defaultValue(t ast.Expr) *mast.Node {
	if id, ok := t.(*ast.Ident); ok {
		switch id.Name {
		case "int":
			return &mast.Node{Kind: "int", Value: 0}
		case "string":
			return &mast.Node{Kind: "string", Value: ""}
		case "bool":
			return &mast.Node{Kind: "bool", Value: false}
		case "float64":
			return &mast.Node{Kind: "float", Value: 0.0}
		}
	}
	return &mast.Node{Kind: "nil"}
}

func transformType(t ast.Expr) *mast.Node {
	switch v := t.(type) {
	case *ast.Ident:
		return &mast.Node{Kind: "type", Value: v.Name}
	case *ast.FuncType:
		n := &mast.Node{Kind: "typefun"}
		if v.Params != nil {
			for _, p := range v.Params.List {
				n.Children = append(n.Children, transformType(p.Type))
			}
		}
		if v.Results != nil && len(v.Results.List) == 1 {
			n.Children = append(n.Children, transformType(v.Results.List[0].Type))
		}
		return n
	case *ast.ArrayType:
		if id, ok := v.Elt.(*ast.Ident); ok {
			return &mast.Node{Kind: "type", Value: "list<" + id.Name + ">"}
		}
	}
	return &mast.Node{Kind: "type", Value: "any"}
}

func fieldName(name string, f *ast.Field) string {
	if f == nil || f.Tag == nil {
		return strings.ToLower(name)
	}
	tag, err := strconv.Unquote(f.Tag.Value)
	if err == nil {
		for _, part := range strings.Split(tag, " ") {
			if strings.HasPrefix(part, "json:\"") {
				v := strings.TrimSuffix(strings.TrimPrefix(part, "json:\""), "\"")
				if idx := strings.IndexByte(v, ','); idx >= 0 {
					v = v[:idx]
				}
				if v != "" && v != "-" {
					return v
				}
			}
		}
	}
	return strings.ToLower(name)
}
