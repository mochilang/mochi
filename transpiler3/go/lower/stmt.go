package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/go/gotree"
)

// lowerBlock walks an aotir.Block's statement list into a
// gotree.BlockStmt.
func (l *lowerer) lowerBlock(b *aotir.Block) (*gotree.BlockStmt, error) {
	out := &gotree.BlockStmt{}
	for _, s := range b.Statements {
		gs, err := l.lowerStmt(s)
		if err != nil {
			return nil, err
		}
		out.List = append(out.List, gs)
	}
	return out, nil
}

func (l *lowerer) lowerStmt(s aotir.Stmt) (gotree.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)
	case *aotir.LetStmt:
		return l.lowerLetStmt(s)
	case *aotir.AssignStmt:
		return l.lowerAssignStmt(s)
	case *aotir.IfStmt:
		return l.lowerIfStmt(s)
	case *aotir.WhileStmt:
		return l.lowerWhileStmt(s)
	case *aotir.ForRangeStmt:
		return l.lowerForRangeStmt(s)
	case *aotir.ForEachStmt:
		return l.lowerForEachStmt(s)
	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(s)
	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(s)
	case *aotir.BreakStmt:
		return &gotree.BranchStmt{Tok: "break"}, nil
	case *aotir.ContinueStmt:
		return &gotree.BranchStmt{Tok: "continue"}, nil
	case *aotir.ReturnStmt:
		return l.lowerReturnStmt(s)
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: does not handle stmt %T", s)
	}
}

// lowerCallStmt handles the print runtime shims emitted by the
// shared aotir lowerer (mochi_print_str, mochi_print_i64,
// mochi_print_f64, mochi_print_bool). Each lowers to a single
// fmt.Println(arg) call; the runtime shim names go away.
func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (gotree.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("transpiler3/go/lower: %s takes one arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		l.addImport("fmt")
		return &gotree.ExprStmt{X: &gotree.CallExpr{
			Fun:  &gotree.SelectorExpr{X: &gotree.Ident{Name: "fmt"}, Sel: "Println"},
			Args: []gotree.Expr{arg},
		}}, nil
	default:
		return nil, fmt.Errorf("transpiler3/go/lower: Phase 2 does not handle call %q", s.Func)
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (gotree.Stmt, error) {
	typeText, err := l.letTypeText(s)
	if err != nil {
		return nil, fmt.Errorf("transpiler3/go/lower: let %s: %w", s.Name, err)
	}
	init, err := l.lowerExpr(s.Init)
	if err != nil {
		return nil, fmt.Errorf("transpiler3/go/lower: let %s init: %w", s.Name, err)
	}
	name := mangleIdent(s.Name)
	if s.Mutable {
		// `var name Type = init` keeps the binding mutable so
		// subsequent AssignStmts can update it; gofmt collapses
		// to the appropriate form.
		return &gotree.DeclStmt{Decl: &gotree.GenDecl{
			Tok: "var",
			Specs: []gotree.Spec{&gotree.ValueSpec{
				Names:  []string{name},
				Type:   &gotree.Ident{Name: typeText},
				Values: []gotree.Expr{init},
			}},
		}}, nil
	}
	// Immutable let: use `name := init`. The type annotation is
	// implicit; gotree-level typing is handled by gofmt and the
	// Go compiler.
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: name}},
		Tok: ":=",
		Rhs: []gotree.Expr{init},
	}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) (gotree.Stmt, error) {
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: mangleIdent(s.Name)}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (gotree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	thenBlk, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	out := &gotree.IfStmt{Cond: cond, Body: thenBlk}
	if s.Else != nil {
		elseBlk, err := l.lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		out.Else = elseBlk
	}
	return out, nil
}

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (gotree.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &gotree.ForStmt{Cond: cond, Body: body}, nil
}

// lowerForRangeStmt lowers Mochi `for i in a..b { ... }` to
// the canonical Go `for i := a; i < b; i++ { ... }` form.
func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (gotree.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	name := mangleIdent(s.Var)
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	init := &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.Ident{Name: name}},
		Tok: ":=",
		Rhs: []gotree.Expr{start},
	}
	cond := &gotree.BinaryExpr{X: &gotree.Ident{Name: name}, Op: "<", Y: end}
	post := &gotree.IncDecStmt{X: &gotree.Ident{Name: name}, Tok: "++"}
	return &gotree.ForStmt{Init: init, Cond: cond, Post: post, Body: body}, nil
}

// letTypeText returns the Go type-expression text for a LetStmt's
// binding type, dispatching on VarType so compound types (lists
// and maps today, records and sums later) can carry their element
// / field metadata through the type renderer.
func (l *lowerer) letTypeText(s *aotir.LetStmt) (string, error) {
	switch s.VarType {
	case aotir.TypeList:
		if s.ElemType == aotir.TypeRecord {
			if s.ElemRecordName == "" {
				return "", fmt.Errorf("list<record> missing ElemRecordName")
			}
			return "[]" + s.ElemRecordName, nil
		}
		return l.lowerListType(s.ElemType)
	case aotir.TypeMap:
		return l.lowerMapType(s.KeyType, s.ValueType)
	case aotir.TypeSet:
		return l.lowerSetType(s.ElemType)
	case aotir.TypeRecord:
		if s.RecordName == "" {
			return "", fmt.Errorf("record let missing RecordName")
		}
		return s.RecordName, nil
	}
	return l.lowerType(s.VarType)
}

// lowerForEachStmt lowers Mochi `for x in xs { ... }` to
// `for _, x := range xs { ... }`. The key (index) slot is
// discarded with `_`.
func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (gotree.Stmt, error) {
	xs, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &gotree.RangeStmt{
		Key:   &gotree.Ident{Name: "_"},
		Value: &gotree.Ident{Name: mangleIdent(s.Var)},
		Tok:   ":=",
		X:     xs,
		Body:  body,
	}, nil
}

// lowerListSetStmt lowers Mochi `xs[i] = v` to `xs[int(i)] = v`.
func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (gotree.Stmt, error) {
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.IndexExpr{
			X:     &gotree.Ident{Name: mangleIdent(s.Name)},
			Index: narrowToInt(idx),
		}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

// lowerMapPutStmt lowers Mochi `m[k] = v` to the same Go form.
func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (gotree.Stmt, error) {
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.AssignStmt{
		Lhs: []gotree.Expr{&gotree.IndexExpr{
			X:     &gotree.Ident{Name: mangleIdent(s.Name)},
			Index: key,
		}},
		Tok: "=",
		Rhs: []gotree.Expr{val},
	}, nil
}

func (l *lowerer) lowerReturnStmt(s *aotir.ReturnStmt) (gotree.Stmt, error) {
	if s.Value == nil {
		return &gotree.ReturnStmt{}, nil
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &gotree.ReturnStmt{Results: []gotree.Expr{val}}, nil
}
