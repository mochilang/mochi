package ast

import (
	"fmt"
	"strings"

	"mochi/parser"
)

func FromProgram(p *parser.Program) *Node {
	root := &Node{Kind: "program"}
	if p.Package != "" {
		root.Value = p.Package
	}
	for _, stmt := range p.Statements {
		root.Children = append(root.Children, FromStatement(stmt))
	}
	return root
}

func FromStatement(s *parser.Statement) *Node {
	switch {
	case s.Let != nil:
		n := &Node{Kind: "let", Value: s.Let.Name}
		if s.Let.Type != nil {
			n.Children = append(n.Children, FromTypeRef(s.Let.Type))
		}
		if s.Let.Value != nil {
			n.Children = append(n.Children, FromExpr(s.Let.Value))
		}
		return n

	case s.Var != nil:
		n := &Node{Kind: "var", Value: s.Var.Name}
		if s.Var.Type != nil {
			n.Children = append(n.Children, FromTypeRef(s.Var.Type))
		}
		if s.Var.Value != nil {
			n.Children = append(n.Children, FromExpr(s.Var.Value))
		}
		return n

	case s.Assign != nil:
		if len(s.Assign.Index) == 0 {
			return &Node{
				Kind:  "assign",
				Value: s.Assign.Name,
				Children: []*Node{
					FromExpr(s.Assign.Value),
				},
			}
		}
		// Build target expression with indexes
		target := &parser.PostfixExpr{Target: &parser.Primary{Selector: &parser.SelectorExpr{Root: s.Assign.Name}}}
		for _, idx := range s.Assign.Index {
			target.Ops = append(target.Ops, &parser.PostfixOp{Index: idx})
		}
		return &Node{
			Kind:     "assign",
			Children: []*Node{FromPostfixExpr(target), FromExpr(s.Assign.Value)},
		}

	case s.Fun != nil:
		n := &Node{Kind: "fun", Value: s.Fun.Name}
		for _, param := range s.Fun.Params {
			pn := &Node{Kind: "param", Value: param.Name}
			if param.Type != nil {
				pn.Children = append(pn.Children, FromTypeRef(param.Type))
			}
			n.Children = append(n.Children, pn)
		}
		if s.Fun.Return != nil {
			n.Children = append(n.Children, FromTypeRef(s.Fun.Return))
		}
		n.Children = append(n.Children, mapStatements(s.Fun.Body)...)
		if s.Fun.Export {
			return &Node{Kind: "export", Children: []*Node{n}}
		}
		return n

	case s.Return != nil:
		return &Node{Kind: "return", Children: []*Node{FromExpr(s.Return.Value)}}

	case s.Break != nil:
		return &Node{Kind: "break"}

	case s.Continue != nil:
		return &Node{Kind: "continue"}

	case s.Update != nil:
		n := &Node{Kind: "update", Value: s.Update.Target}
		mapExpr := &parser.Expr{Binary: &parser.BinaryExpr{Left: &parser.Unary{Value: &parser.PostfixExpr{Target: &parser.Primary{Map: s.Update.Set}}}}}
		n.Children = append(n.Children, &Node{Kind: "set", Children: []*Node{FromExpr(mapExpr)}})
		if s.Update.Where != nil {
			n.Children = append(n.Children, &Node{Kind: "where", Children: []*Node{FromExpr(s.Update.Where)}})
		}
		return n

	case s.Expr != nil:
		return FromExpr(s.Expr.Expr)

	case s.If != nil:
		return fromIfStmt(s.If)

	case s.While != nil:
		return fromWhileStmt(s.While)

	case s.For != nil:
		return fromForStmt(s.For)

	case s.Agent != nil:
		n := &Node{Kind: "agent", Value: s.Agent.Name}
		for _, block := range s.Agent.Body {
			switch {
			case block.Let != nil:
				n.Children = append(n.Children, FromStatement(&parser.Statement{Let: block.Let}))
			case block.Assign != nil:
				n.Children = append(n.Children, FromStatement(&parser.Statement{Assign: block.Assign}))
			case block.On != nil:
				n.Children = append(n.Children, fromOnHandler(block.On))
			case block.Intent != nil:
				n.Children = append(n.Children, fromIntent(block.Intent))
			}
		}
		return n

	case s.On != nil:
		return fromOnHandler(s.On)

	case s.Stream != nil:
		n := &Node{Kind: "stream", Value: s.Stream.Name}
		for _, f := range s.Stream.Fields {
			n.Children = append(n.Children, fromStreamField(f))
		}
		return n

	case s.Model != nil:
		n := &Node{Kind: "model", Value: s.Model.Name}
		for _, f := range s.Model.Fields {
			n.Children = append(n.Children, &Node{
				Kind:     f.Name,
				Children: []*Node{FromExpr(f.Value)},
			})
		}
		return n

	case s.Type != nil:
		n := &Node{Kind: "type", Value: s.Type.Name}
		for _, m := range s.Type.Members {
			if m.Field != nil {
				n.Children = append(n.Children, &Node{
					Kind:     "field",
					Value:    m.Field.Name,
					Children: []*Node{FromTypeRef(m.Field.Type)},
				})
			} else if m.Method != nil {
				n.Children = append(n.Children, FromStatement(&parser.Statement{Fun: m.Method}))
			}
		}
		return n

	case s.Test != nil:
		n := &Node{Kind: "test", Value: s.Test.Name}
		n.Children = append(n.Children, mapStatements(s.Test.Body)...)
		return n

	case s.Expect != nil:
		return &Node{Kind: "expect", Children: []*Node{FromExpr(s.Expect.Value)}}

	default:
		return &Node{Kind: "unknown"}
	}
}

// --- Control Flow Helpers ---

func fromIfStmt(stmt *parser.IfStmt) *Node {
	n := &Node{Kind: "if", Children: []*Node{FromExpr(stmt.Cond)}}

	thenBlock := &Node{Kind: "block", Children: mapStatements(stmt.Then)}
	n.Children = append(n.Children, thenBlock)

	if stmt.ElseIf != nil {
		n.Children = append(n.Children, fromIfStmt(stmt.ElseIf))
	} else if stmt.Else != nil {
		elseBlock := &Node{Kind: "block", Children: mapStatements(stmt.Else)}
		n.Children = append(n.Children, elseBlock)
	}
	return n
}

func fromIfExpr(expr *parser.IfExpr) *Node {
	n := &Node{Kind: "if_expr", Children: []*Node{FromExpr(expr.Cond), FromExpr(expr.Then)}}
	if expr.ElseIf != nil {
		n.Children = append(n.Children, fromIfExpr(expr.ElseIf))
	} else if expr.Else != nil {
		n.Children = append(n.Children, FromExpr(expr.Else))
	}
	return n
}

func fromWhileStmt(stmt *parser.WhileStmt) *Node {
	n := &Node{Kind: "while", Children: []*Node{FromExpr(stmt.Cond)}}
	n.Children = append(n.Children, &Node{Kind: "block", Children: mapStatements(stmt.Body)})
	return n
}

func fromForStmt(f *parser.ForStmt) *Node {
	n := &Node{Kind: "for", Value: f.Name}

	if f.RangeEnd != nil {
		// Range loop: for i in start..end
		n.Children = append(n.Children, &Node{
			Kind:     "range",
			Children: []*Node{FromExpr(f.Source), FromExpr(f.RangeEnd)},
		})
	} else {
		// Collection loop: for x in expr
		n.Children = append(n.Children, &Node{
			Kind:     "in",
			Children: []*Node{FromExpr(f.Source)},
		})
	}

	n.Children = append(n.Children, &Node{
		Kind:     "block",
		Children: mapStatements(f.Body),
	})
	return n
}

// --- DSL Helpers ---

func fromOnHandler(h *parser.OnHandler) *Node {
	return &Node{
		Kind:     "on",
		Value:    h.Stream,
		Children: mapStatements(h.Body),
	}
}

func fromIntent(i *parser.IntentDecl) *Node {
	n := &Node{Kind: "intent", Value: i.Name}
	for _, param := range i.Params {
		pn := &Node{Kind: "param", Value: param.Name}
		if param.Type != nil {
			pn.Children = append(pn.Children, FromTypeRef(param.Type))
		}
		n.Children = append(n.Children, pn)
	}
	if i.Return != nil {
		n.Children = append(n.Children, FromTypeRef(i.Return))
	}
	n.Children = append(n.Children, mapStatements(i.Body)...)
	return n
}

func fromStreamField(f *parser.StreamField) *Node {
	if f == nil {
		return &Node{Kind: "field", Value: "unknown"}
	}
	return &Node{Kind: "field", Value: f.Name + ":" + typeRefString(f.Type)}
}

func mapStatements(stmts []*parser.Statement) []*Node {
	var out []*Node
	for _, s := range stmts {
		out = append(out, FromStatement(s))
	}
	return out
}

func isUnderscoreExpr(e *parser.Expr) bool {
	if e == nil {
		return false
	}
	if len(e.Binary.Right) != 0 {
		return false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}

// --- Expression Conversion ---

func FromExpr(e *parser.Expr) *Node {
	n := FromUnary(e.Binary.Left)
	for _, op := range e.Binary.Right {
		val := op.Op
		if op.All {
			val += "_all"
		}
		n = &Node{Kind: "binary", Value: val, Children: []*Node{n, FromPostfixExpr(op.Right)}}
	}
	return n
}

func FromUnary(u *parser.Unary) *Node {
	n := FromPostfixExpr(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		op := u.Ops[i]
		if op == "-" {
			switch n.Kind {
			case "int":
				if v, ok := n.Value.(int); ok {
					n.Value = -v
					continue
				}
			case "float":
				if v, ok := n.Value.(float64); ok {
					n.Value = -v
					continue
				}
			}
		}
		n = &Node{Kind: "unary", Value: op, Children: []*Node{n}}
	}
	return n
}

func FromPostfixExpr(p *parser.PostfixExpr) *Node {
	n := FromPrimary(p.Target)
	for _, op := range p.Ops {
		if idx := op.Index; idx != nil {
			idxNode := &Node{Kind: "index", Children: []*Node{n}}
			if idx.Colon == nil {
				if idx.Start != nil {
					idxNode.Children = append(idxNode.Children, FromExpr(idx.Start))
				}
			} else {
				if idx.Start != nil {
					idxNode.Children = append(idxNode.Children, &Node{Kind: "start", Children: []*Node{FromExpr(idx.Start)}})
				}
				if idx.End != nil {
					idxNode.Children = append(idxNode.Children, &Node{Kind: "end", Children: []*Node{FromExpr(idx.End)}})
				}
			}
			n = idxNode
		} else if call := op.Call; call != nil {
			callNode := &Node{Kind: "call", Children: []*Node{n}}
			for _, a := range call.Args {
				callNode.Children = append(callNode.Children, FromExpr(a))
			}
			n = callNode
		} else if cast := op.Cast; cast != nil {
			n = &Node{Kind: "cast", Children: []*Node{n, FromTypeRef(cast.Type)}}
		} else if field := op.Field; field != nil {
			n = &Node{Kind: "selector", Value: field.Name, Children: []*Node{n}}
		}
	}
	return n
}

func FromPrimary(p *parser.Primary) *Node {
	switch {
	case p.FunExpr != nil:
		n := &Node{Kind: "funexpr"}
		for _, param := range p.FunExpr.Params {
			pn := &Node{Kind: "param", Value: param.Name}
			if param.Type != nil {
				pn.Children = append(pn.Children, FromTypeRef(param.Type))
			}
			n.Children = append(n.Children, pn)
		}
		if p.FunExpr.Return != nil {
			n.Children = append(n.Children, FromTypeRef(p.FunExpr.Return))
		}
		if p.FunExpr.ExprBody != nil {
			n.Children = append(n.Children, FromExpr(p.FunExpr.ExprBody))
		} else if len(p.FunExpr.BlockBody) > 0 {
			block := &Node{Kind: "block", Children: mapStatements(p.FunExpr.BlockBody)}
			n.Children = append(n.Children, block)
		}
		return n

	case p.Call != nil:
		n := &Node{Kind: "call", Value: p.Call.Func}
		for _, arg := range p.Call.Args {
			n.Children = append(n.Children, FromExpr(arg))
		}
		return n

	case p.Selector != nil:
		root := &Node{Kind: "selector", Value: p.Selector.Root}
		for _, field := range p.Selector.Tail {
			root = &Node{Kind: "selector", Value: field, Children: []*Node{root}}
		}
		return root

	case p.Struct != nil:
		n := &Node{Kind: "struct", Value: p.Struct.Name}
		for _, field := range p.Struct.Fields {
			n.Children = append(n.Children, &Node{
				Kind:     "field",
				Value:    field.Name,
				Children: []*Node{FromExpr(field.Value)},
			})
		}
		return n

	case p.List != nil:
		n := &Node{Kind: "list"}
		for _, el := range p.List.Elems {
			n.Children = append(n.Children, FromExpr(el))
		}
		return n

	case p.Map != nil:
		n := &Node{Kind: "map"}
		for _, entry := range p.Map.Items {
			n.Children = append(n.Children, &Node{
				Kind: "entry",
				Children: []*Node{
					FromExpr(entry.Key),
					FromExpr(entry.Value),
				},
			})
		}
		return n

	case p.Query != nil:
		n := &Node{Kind: "query", Value: p.Query.Var}
		n.Children = append(n.Children, &Node{Kind: "source", Children: []*Node{FromExpr(p.Query.Source)}})
		for _, f := range p.Query.Froms {
			fn := &Node{Kind: "from", Value: f.Var}
			fn.Children = append(fn.Children, &Node{Kind: "source", Children: []*Node{FromExpr(f.Src)}})
			n.Children = append(n.Children, fn)
		}
		for _, j := range p.Query.Joins {
			kind := "join"
			if j.Side != nil {
				switch *j.Side {
				case "left":
					kind = "left_join"
				case "right":
					kind = "right_join"
				case "outer":
					kind = "outer_join"
				}
			}
			jn := &Node{Kind: kind, Value: j.Var}
			jn.Children = append(jn.Children, &Node{Kind: "source", Children: []*Node{FromExpr(j.Src)}})
			if j.On != nil {
				jn.Children = append(jn.Children, &Node{Kind: "on", Children: []*Node{FromExpr(j.On)}})
			}
			n.Children = append(n.Children, jn)
		}
		if p.Query.Where != nil {
			n.Children = append(n.Children, &Node{Kind: "where", Children: []*Node{FromExpr(p.Query.Where)}})
		}
		if p.Query.Group != nil {
			n.Children = append(n.Children, &Node{
				Kind: "group_by",
				Children: []*Node{
					FromExpr(p.Query.Group.Exprs[0]),
					&Node{Kind: "into", Value: p.Query.Group.Name},
				},
			})
		}
		if p.Query.Sort != nil {
			n.Children = append(n.Children, &Node{Kind: "sort", Children: []*Node{FromExpr(p.Query.Sort)}})
		}
		if p.Query.Skip != nil {
			n.Children = append(n.Children, &Node{Kind: "skip", Children: []*Node{FromExpr(p.Query.Skip)}})
		}
		if p.Query.Take != nil {
			n.Children = append(n.Children, &Node{Kind: "take", Children: []*Node{FromExpr(p.Query.Take)}})
		}
		n.Children = append(n.Children, &Node{Kind: "select", Children: []*Node{FromExpr(p.Query.Select)}})
		return n

	case p.Match != nil:
		n := &Node{Kind: "match"}
		n.Children = append(n.Children, FromExpr(p.Match.Target))
		for _, c := range p.Match.Cases {
			cn := &Node{Kind: "case"}
			if !isUnderscoreExpr(c.Pattern) {
				cn.Children = append(cn.Children, FromExpr(c.Pattern))
			} else {
				cn.Children = append(cn.Children, &Node{Kind: "_"})
			}
			cn.Children = append(cn.Children, FromExpr(c.Result))
			n.Children = append(n.Children, cn)
		}
		return n

	case p.If != nil:
		return fromIfExpr(p.If)

	case p.Generate != nil:
		n := &Node{Kind: "generate_text"}
		for _, f := range p.Generate.Fields {
			n.Children = append(n.Children, &Node{
				Kind:     f.Name,
				Children: []*Node{FromExpr(f.Value)},
			})
		}
		return n

	case p.Fetch != nil:
		n := &Node{Kind: "fetch"}
		n.Children = append(n.Children, FromExpr(p.Fetch.URL))
		if p.Fetch.With != nil {
			n.Children = append(n.Children, FromExpr(p.Fetch.With))
		}
		return n

	case p.Load != nil:
		n := &Node{Kind: "load"}
		if p.Load.Path != nil {
			n.Children = append(n.Children, &Node{Kind: "string", Value: *p.Load.Path})
		}
		if p.Load.Type != nil {
			n.Children = append(n.Children, FromTypeRef(p.Load.Type))
		}
		if p.Load.With != nil {
			n.Children = append(n.Children, FromExpr(p.Load.With))
		}
		return n

	case p.Save != nil:
		n := &Node{Kind: "save"}
		n.Children = append(n.Children, FromExpr(p.Save.Src))
		if p.Save.Path != nil {
			n.Children = append(n.Children, &Node{Kind: "string", Value: *p.Save.Path})
		}
		if p.Save.With != nil {
			n.Children = append(n.Children, FromExpr(p.Save.With))
		}
		return n

	case p.Lit != nil:
		switch {
		case p.Lit.Float != nil:
			return &Node{Kind: "float", Value: *p.Lit.Float}
		case p.Lit.Int != nil:
			return &Node{Kind: "int", Value: *p.Lit.Int}
		case p.Lit.Str != nil:
			return &Node{Kind: "string", Value: *p.Lit.Str}
		case p.Lit.Bool != nil:
			return &Node{Kind: "bool", Value: bool(*p.Lit.Bool)}
		}

	case p.Group != nil:
		return &Node{Kind: "group", Children: []*Node{FromExpr(p.Group)}}
	}

	return &Node{Kind: "unknown"}
}

// --- Type Ref ---

func FromTypeRef(t *parser.TypeRef) *Node {
	if t == nil {
		return nil
	}
	if t.Fun != nil {
		n := &Node{Kind: "typefun"}
		for _, param := range t.Fun.Params {
			n.Children = append(n.Children, FromTypeRef(param))
		}
		if t.Fun.Return != nil {
			n.Children = append(n.Children, FromTypeRef(t.Fun.Return))
		}
		return n
	}
	if t.Generic != nil {
		n := &Node{Kind: "type", Value: t.Generic.Name}
		for _, arg := range t.Generic.Args {
			n.Children = append(n.Children, FromTypeRef(arg))
		}
		return n
	}
	if t.Struct != nil {
		n := &Node{Kind: "type"}
		s := &Node{Kind: "struct"}
		for _, f := range t.Struct.Fields {
			s.Children = append(s.Children, &Node{Kind: "field", Value: f.Name, Children: []*Node{FromTypeRef(f.Type)}})
		}
		n.Children = append(n.Children, s)
		return n
	}
	if t.Simple != nil {
		return &Node{Kind: "type", Value: *t.Simple}
	}
	return &Node{Kind: "type", Value: "unknown"}
}

func typeRefString(t *parser.TypeRef) string {
	if t == nil {
		return ""
	}
	if t.Simple != nil {
		return *t.Simple
	}
	if t.Generic != nil {
		parts := make([]string, len(t.Generic.Args))
		for i, a := range t.Generic.Args {
			parts[i] = typeRefString(a)
		}
		return fmt.Sprintf("%s<%s>", t.Generic.Name, strings.Join(parts, ","))
	}
	if t.Struct != nil {
		parts := make([]string, len(t.Struct.Fields))
		for i, f := range t.Struct.Fields {
			parts[i] = fmt.Sprintf("%s:%s", f.Name, typeRefString(f.Type))
		}
		return fmt.Sprintf("{%s}", strings.Join(parts, ","))
	}
	if t.Fun != nil {
		parts := make([]string, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			parts[i] = typeRefString(p)
		}
		s := fmt.Sprintf("fun(%s)", strings.Join(parts, ","))
		if t.Fun.Return != nil {
			s += ":" + typeRefString(t.Fun.Return)
		}
		return s
	}
	return ""
}
