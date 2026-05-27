package lower

import (
	"fmt"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerStmt translates an aotir.Stmt to a javasrc.Stmt.
// Returns (nil, nil) for statements that produce no output (e.g. no-ops).
func lowerStmt(s aotir.Stmt) (javasrc.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return lowerCallStmt(s)

	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &javasrc.ReturnStmt{}, nil
		}
		v, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &javasrc.ReturnStmt{Value: v}, nil

	case *aotir.LetStmt:
		return lowerLetStmt(s)

	case *aotir.AssignStmt:
		val, err := lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &javasrc.AssignStmt{
			Target: &javasrc.NameExpr{Name: s.Name},
			Value:  val,
		}, nil

	case *aotir.IfStmt:
		return lowerIfStmt(s)

	case *aotir.WhileStmt:
		return lowerWhileStmt(s)

	case *aotir.ForRangeStmt:
		return lowerForRangeStmt(s)

	case *aotir.BreakStmt:
		return &javasrc.BreakStmt{}, nil

	case *aotir.ContinueStmt:
		return &javasrc.ContinueStmt{}, nil

	case *aotir.TryCatchStmt:
		return lowerTryCatchStmt(s)

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported stmt %T", s)
	}
}

func lowerCallStmt(s *aotir.CallStmt) (javasrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("jvm/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := lowerExpr(s.Args[0])
		if err != nil {
			return nil, err
		}
		call := &javasrc.StaticCallExpr{
			Class:  "dev.mochi.runtime.io.IO",
			Method: "println",
			Args:   []javasrc.Expr{arg},
		}
		return &javasrc.ExprStmt{X: call}, nil
	default:
		return nil, fmt.Errorf("jvm/lower: unsupported builtin %q", s.Func)
	}
}

func lowerLetStmt(s *aotir.LetStmt) (javasrc.Stmt, error) {
	t, err := lowerType(s.VarType)
	if err != nil {
		return nil, err
	}
	var init javasrc.Expr
	if s.Init != nil {
		init, err = lowerExpr(s.Init)
		if err != nil {
			return nil, err
		}
	}
	return &javasrc.VarDeclStmt{
		Final: !s.Mutable,
		Type:  &t,
		Name:  s.Name,
		Init:  init,
	}, nil
}

func lowerIfStmt(s *aotir.IfStmt) (javasrc.Stmt, error) {
	cond, err := lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var elseBlock *javasrc.Block
	if s.Else != nil {
		eb, err := lowerBlock(s.Else)
		if err != nil {
			return nil, err
		}
		elseBlock = &eb
	}
	return &javasrc.IfStmt{
		Cond: cond,
		Then: then,
		Else: elseBlock,
	}, nil
}

func lowerWhileStmt(s *aotir.WhileStmt) (javasrc.Stmt, error) {
	cond, err := lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &javasrc.WhileStmt{
		Cond: cond,
		Body: body,
	}, nil
}

func lowerForRangeStmt(s *aotir.ForRangeStmt) (javasrc.Stmt, error) {
	start, err := lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}

	longType := javasrc.TypeLong
	init := &javasrc.VarDeclStmt{
		Type: &longType,
		Name: s.Var,
		Init: start,
	}
	cond := &javasrc.BinaryExpr{
		Left:  &javasrc.NameExpr{Name: s.Var},
		Op:    "<",
		Right: end,
	}
	update := &javasrc.ExprStmt{
		X: &javasrc.UnaryExpr{
			Op:      "++",
			Operand: &javasrc.NameExpr{Name: s.Var},
			Postfix: true,
		},
	}
	return &javasrc.ForStmt{
		Init:   init,
		Cond:   cond,
		Update: update,
		Body:   body,
	}, nil
}

func lowerTryCatchStmt(s *aotir.TryCatchStmt) (javasrc.Stmt, error) {
	tryBody, err := lowerBlock(s.TryBody)
	if err != nil {
		return nil, err
	}
	catchBody, err := lowerBlock(s.CatchBody)
	if err != nil {
		return nil, err
	}
	return &javasrc.TryCatchStmt{
		Body:      tryBody,
		CatchVar:  s.CatchVar,
		CatchTyp:  javasrc.TypeRef{Name: "dev.mochi.runtime.error.MochiPanicException"},
		CatchBody: catchBody,
	}, nil
}

// lowerBlock converts an aotir.Block to a javasrc.Block.
func lowerBlock(b *aotir.Block) (javasrc.Block, error) {
	if b == nil {
		return javasrc.Block{}, nil
	}
	var stmts []javasrc.Stmt
	for _, s := range b.Statements {
		js, err := lowerStmt(s)
		if err != nil {
			return javasrc.Block{}, err
		}
		if js != nil {
			stmts = append(stmts, js)
		}
	}
	return javasrc.Block{Stmts: stmts}, nil
}
