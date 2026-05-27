package lower

import (
	"fmt"
	"strings"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerStmt translates an aotir.Stmt to a javasrc.Stmt.
// Returns (nil, nil) for statements that produce no output (e.g. no-ops).
func (l *lowerer) lowerStmt(s aotir.Stmt) (javasrc.Stmt, error) {
	switch s := s.(type) {
	case *aotir.CallStmt:
		return l.lowerCallStmt(s)

	case *aotir.ReturnStmt:
		if s.Value == nil {
			return &javasrc.ReturnStmt{}, nil
		}
		v, err := l.lowerExpr(s.Value)
		if err != nil {
			return nil, err
		}
		return &javasrc.ReturnStmt{Value: v}, nil

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

	case *aotir.BreakStmt:
		return &javasrc.BreakStmt{}, nil

	case *aotir.ContinueStmt:
		return &javasrc.ContinueStmt{}, nil

	case *aotir.TryCatchStmt:
		return l.lowerTryCatchStmt(s)

	case *aotir.ListSetStmt:
		return l.lowerListSetStmt(s)

	case *aotir.MapPutStmt:
		return l.lowerMapPutStmt(s)

	default:
		return nil, fmt.Errorf("jvm/lower: unsupported stmt %T", s)
	}
}

func (l *lowerer) lowerCallStmt(s *aotir.CallStmt) (javasrc.Stmt, error) {
	switch s.Func {
	case "mochi_print_str", "mochi_print_i64", "mochi_print_f64", "mochi_print_bool":
		if len(s.Args) != 1 {
			return nil, fmt.Errorf("jvm/lower: %s wants 1 arg, got %d", s.Func, len(s.Args))
		}
		arg, err := l.lowerExpr(s.Args[0])
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
		// User-defined function called as a statement (discarding the return value).
		if !strings.HasPrefix(s.Func, "mochi_") {
			args, err := l.lowerExprs(s.Args)
			if err != nil {
				return nil, err
			}
			call := &javasrc.StaticCallExpr{
				Class:  l.className,
				Method: s.Func,
				Args:   args,
			}
			return &javasrc.ExprStmt{X: call}, nil
		}
		return nil, fmt.Errorf("jvm/lower: unsupported builtin %q", s.Func)
	}
}

func (l *lowerer) lowerLetStmt(s *aotir.LetStmt) (javasrc.Stmt, error) {
	var javaType javasrc.TypeRef
	var err error

	// Choose parameterised collection types for let/var collection bindings.
	switch s.VarType {
	case aotir.TypeList:
		if s.Mutable {
			javaType = lowerMutableListType(s.ElemType)
		} else {
			javaType = lowerListType(s.ElemType)
		}
	case aotir.TypeMap:
		javaType = lowerMapType(s.KeyType, s.ValueType)
	case aotir.TypeSet:
		javaType = lowerSetType(s.ElemType)
	default:
		javaType, err = lowerType(s.VarType)
		if err != nil {
			return nil, err
		}
	}

	var init javasrc.Expr
	if s.Init != nil {
		init, err = l.lowerExpr(s.Init)
		if err != nil {
			return nil, err
		}
		// For immutable (let) list bindings backed by a mutable ArrayList, wrap
		// in java.util.Collections.unmodifiableList so Phase 3 let semantics hold.
		// Actually we keep ArrayList for simplicity -- Mochi let just means no
		// rebinding, not that the container is structurally immutable at runtime.
	}
	return &javasrc.VarDeclStmt{
		Final: !s.Mutable,
		Type:  &javaType,
		Name:  s.Name,
		Init:  init,
	}, nil
}

func (l *lowerer) lowerAssignStmt(s *aotir.AssignStmt) (javasrc.Stmt, error) {
	// Pattern: xs = append(xs, v) -- lower to xs.add(v) for in-place mutation.
	if ap, ok := s.Value.(*aotir.AppendExpr); ok {
		recv := &javasrc.NameExpr{Name: s.Name}
		val, err := l.lowerExpr(ap.Value)
		if err != nil {
			return nil, err
		}
		call := &javasrc.CallExpr{
			Receiver: recv,
			Method:   "add",
			Args:     []javasrc.Expr{val},
		}
		return &javasrc.ExprStmt{X: call}, nil
	}
	// Pattern: s = add(s, x) (set add) -- lower to s.add(x) for in-place mutation.
	if sa, ok := s.Value.(*aotir.SetAddExpr); ok {
		recv := &javasrc.NameExpr{Name: s.Name}
		elem, err := l.lowerExpr(sa.Elem)
		if err != nil {
			return nil, err
		}
		call := &javasrc.CallExpr{
			Receiver: recv,
			Method:   "add",
			Args:     []javasrc.Expr{elem},
		}
		return &javasrc.ExprStmt{X: call}, nil
	}
	// General assignment.
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	return &javasrc.AssignStmt{
		Target: &javasrc.NameExpr{Name: s.Name},
		Value:  val,
	}, nil
}

func (l *lowerer) lowerIfStmt(s *aotir.IfStmt) (javasrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	then, err := l.lowerBlock(s.Then)
	if err != nil {
		return nil, err
	}
	var elseBlock *javasrc.Block
	if s.Else != nil {
		eb, err := l.lowerBlock(s.Else)
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

func (l *lowerer) lowerWhileStmt(s *aotir.WhileStmt) (javasrc.Stmt, error) {
	cond, err := l.lowerExpr(s.Cond)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	return &javasrc.WhileStmt{
		Cond: cond,
		Body: body,
	}, nil
}

func (l *lowerer) lowerForRangeStmt(s *aotir.ForRangeStmt) (javasrc.Stmt, error) {
	start, err := l.lowerExpr(s.Start)
	if err != nil {
		return nil, err
	}
	end, err := l.lowerExpr(s.End)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
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

// lowerForEachStmt lowers `for x in xs { ... }` to a Java enhanced for loop.
func (l *lowerer) lowerForEachStmt(s *aotir.ForEachStmt) (javasrc.Stmt, error) {
	list, err := l.lowerExpr(s.List)
	if err != nil {
		return nil, err
	}
	body, err := l.lowerBlock(s.Body)
	if err != nil {
		return nil, err
	}
	// Use the boxed element type so Java can iterate over List<Long> etc.
	elemType := boxedType(s.ElemType)
	return &javasrc.ForEachStmt{
		ElemType: elemType,
		ElemName: s.Var,
		Iter:     list,
		Body:     body,
	}, nil
}

func (l *lowerer) lowerTryCatchStmt(s *aotir.TryCatchStmt) (javasrc.Stmt, error) {
	tryBody, err := l.lowerBlock(s.TryBody)
	if err != nil {
		return nil, err
	}
	catchBody, err := l.lowerBlock(s.CatchBody)
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

// lowerListSetStmt lowers xs[i] = v to xs.set((int) i, v).
func (l *lowerer) lowerListSetStmt(s *aotir.ListSetStmt) (javasrc.Stmt, error) {
	recv := &javasrc.NameExpr{Name: s.Name}
	idx, err := l.lowerExpr(s.Index)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	intIdx := &javasrc.CastExpr{Type: javasrc.TypeRef{Name: "int"}, X: idx}
	call := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "set",
		Args:     []javasrc.Expr{intIdx, val},
	}
	return &javasrc.ExprStmt{X: call}, nil
}

// lowerMapPutStmt lowers m[k] = v to m.put(k, v).
func (l *lowerer) lowerMapPutStmt(s *aotir.MapPutStmt) (javasrc.Stmt, error) {
	recv := &javasrc.NameExpr{Name: s.Name}
	key, err := l.lowerExpr(s.Key)
	if err != nil {
		return nil, err
	}
	val, err := l.lowerExpr(s.Value)
	if err != nil {
		return nil, err
	}
	call := &javasrc.CallExpr{
		Receiver: recv,
		Method:   "put",
		Args:     []javasrc.Expr{key, val},
	}
	return &javasrc.ExprStmt{X: call}, nil
}

// lowerBlock converts an aotir.Block to a javasrc.Block.
func (l *lowerer) lowerBlock(b *aotir.Block) (javasrc.Block, error) {
	if b == nil {
		return javasrc.Block{}, nil
	}
	var stmts []javasrc.Stmt
	for _, s := range b.Statements {
		js, err := l.lowerStmt(s)
		if err != nil {
			return javasrc.Block{}, err
		}
		if js != nil {
			stmts = append(stmts, js)
		}
	}
	return javasrc.Block{Stmts: stmts}, nil
}
