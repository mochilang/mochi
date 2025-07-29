package gox

import (
	"fmt"
	"go/ast"
	"go/token"
	"strconv"

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
			if d.Tok != token.VAR {
				continue
			}
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
		iter := transformExpr(s.X)
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
		}
	case *ast.Ident:
		return &mast.Node{Kind: "selector", Value: v.Name}
	case *ast.BinaryExpr:
		return &mast.Node{Kind: "binary", Value: v.Op.String(), Children: []*mast.Node{
			transformExpr(v.X), transformExpr(v.Y),
		}}
	case *ast.UnaryExpr:
		if v.Op == token.SUB {
			return &mast.Node{Kind: "unary", Value: "-", Children: []*mast.Node{transformExpr(v.X)}}
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
		switch v.Type.(type) {
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
	}
	return &mast.Node{Kind: "type", Value: "any"}
}
