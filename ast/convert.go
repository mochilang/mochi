package ast

import "mochi/parser"

func FromProgram(p *parser.Program) *Node {
	root := &Node{Kind: "program"}
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

	case s.Assign != nil:
		return &Node{
			Kind:  "assign",
			Value: s.Assign.Name,
			Children: []*Node{
				FromExpr(s.Assign.Value),
			},
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
		for _, st := range s.Fun.Body {
			n.Children = append(n.Children, FromStatement(st))
		}
		return n

	case s.Return != nil:
		return &Node{Kind: "return", Children: []*Node{FromExpr(s.Return.Value)}}

	case s.Expr != nil:
		return FromExpr(s.Expr.Expr)

	case s.If != nil:
		return fromIfStmt(s.If)

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

	case s.Test != nil:
		n := &Node{Kind: "test", Value: s.Test.Name}
		for _, stmt := range s.Test.Body {
			n.Children = append(n.Children, FromStatement(stmt))
		}
		return n

	case s.Expect != nil:
		return &Node{Kind: "expect", Children: []*Node{FromExpr(s.Expect.Value)}}

	default:
		return &Node{Kind: "unknown"}
	}
}

func fromIfStmt(stmt *parser.IfStmt) *Node {
	n := &Node{Kind: "if"}
	n.Children = append(n.Children, FromExpr(stmt.Cond))
	thenBlock := &Node{Kind: "block"}
	for _, stmt := range stmt.Then {
		thenBlock.Children = append(thenBlock.Children, FromStatement(stmt))
	}
	n.Children = append(n.Children, thenBlock)

	if stmt.ElseIf != nil {
		n.Children = append(n.Children, fromIfStmt(stmt.ElseIf))
	} else if stmt.Else != nil {
		elseBlock := &Node{Kind: "block"}
		for _, stmt := range stmt.Else {
			elseBlock.Children = append(elseBlock.Children, FromStatement(stmt))
		}
		n.Children = append(n.Children, elseBlock)
	}

	return n
}

func fromForStmt(f *parser.ForStmt) *Node {
	return &Node{
		Kind:  "for",
		Value: f.Name,
		Children: []*Node{
			{Kind: "range", Children: []*Node{
				FromExpr(f.Start),
				FromExpr(f.End),
			}},
			{Kind: "block", Children: mapStatements(f.Body)},
		},
	}
}

func fromOnHandler(h *parser.OnHandler) *Node {
	n := &Node{Kind: "on", Value: h.Stream}
	for _, stmt := range h.Body {
		n.Children = append(n.Children, FromStatement(stmt))
	}
	return n
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
	for _, stmt := range i.Body {
		n.Children = append(n.Children, FromStatement(stmt))
	}
	return n
}

func fromStreamField(f *parser.StreamField) *Node {
	if f.Simple != nil {
		return &Node{Kind: "field", Value: f.Simple.Name + ":" + f.Simple.Type}
	}
	if f.Nested != nil {
		n := &Node{Kind: "field", Value: f.Nested.Name + ":" + f.Nested.Type}
		for _, sub := range f.Nested.Body.Fields {
			n.Children = append(n.Children, fromStreamField(sub))
		}
		return n
	}
	return &Node{Kind: "field"}
}

func mapStatements(stmts []*parser.Statement) []*Node {
	var out []*Node
	for _, s := range stmts {
		out = append(out, FromStatement(s))
	}
	return out
}

func FromExpr(e *parser.Expr) *Node {
	return FromEquality(e.Equality)
}

func FromEquality(e *parser.Equality) *Node {
	n := FromComparison(e.Left)
	for _, op := range e.Right {
		n = &Node{Kind: "binary", Value: op.Op, Children: []*Node{n, FromComparison(op.Right)}}
	}
	return n
}

func FromComparison(c *parser.Comparison) *Node {
	n := FromTerm(c.Left)
	for _, op := range c.Right {
		n = &Node{Kind: "binary", Value: op.Op, Children: []*Node{n, FromTerm(op.Right)}}
	}
	return n
}

func FromTerm(t *parser.Term) *Node {
	n := FromFactor(t.Left)
	for _, op := range t.Right {
		n = &Node{Kind: "binary", Value: op.Op, Children: []*Node{n, FromFactor(op.Right)}}
	}
	return n
}

func FromFactor(f *parser.Factor) *Node {
	n := FromUnary(f.Left)
	for _, op := range f.Right {
		n = &Node{Kind: "binary", Value: op.Op, Children: []*Node{n, FromUnary(op.Right)}}
	}
	return n
}

func FromUnary(u *parser.Unary) *Node {
	n := FromPostfixExpr(u.Value)
	for i := len(u.Ops) - 1; i >= 0; i-- {
		n = &Node{Kind: "unary", Value: u.Ops[i], Children: []*Node{n}}
	}
	return n
}

func FromPostfixExpr(p *parser.PostfixExpr) *Node {
	n := FromPrimary(p.Target)
	for _, op := range p.Index {
		idx := &Node{Kind: "index", Children: []*Node{n}}
		if op.Colon == nil {
			if op.Start != nil {
				idx.Children = append(idx.Children, FromExpr(op.Start))
			}
		} else {
			if op.Start != nil {
				idx.Children = append(idx.Children, &Node{Kind: "start", Children: []*Node{FromExpr(op.Start)}})
			}
			if op.End != nil {
				idx.Children = append(idx.Children, &Node{Kind: "end", Children: []*Node{FromExpr(op.End)}})
			}
		}
		n = idx
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
			block := &Node{Kind: "block"}
			for _, stmt := range p.FunExpr.BlockBody {
				block.Children = append(block.Children, FromStatement(stmt))
			}
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
		n := &Node{Kind: "selector", Value: p.Selector.Root}
		for _, field := range p.Selector.Tail {
			n = &Node{Kind: "selector", Value: field, Children: []*Node{n}}
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

	case p.Lit != nil:
		switch {
		case p.Lit.Float != nil:
			return &Node{Kind: "float", Value: *p.Lit.Float}
		case p.Lit.Int != nil:
			return &Node{Kind: "int", Value: *p.Lit.Int}
		case p.Lit.Str != nil:
			return &Node{Kind: "string", Value: *p.Lit.Str}
		case p.Lit.Bool != nil:
			return &Node{Kind: "bool", Value: *p.Lit.Bool}
		}

	case p.Group != nil:
		return &Node{Kind: "group", Children: []*Node{FromExpr(p.Group)}}
	}

	return &Node{Kind: "unknown"}
}

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
	if t.Simple != nil {
		return &Node{Kind: "type", Value: *t.Simple}
	}
	return &Node{Kind: "type", Value: "unknown"}
}
