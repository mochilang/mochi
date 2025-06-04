package types

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/parser"
)

// --- Type System ---

type Type interface {
	String() string
}

type IntType struct{}

func (IntType) String() string { return "int" }

type FloatType struct{}

func (FloatType) String() string { return "float" }

type StringType struct{}

func (StringType) String() string { return "string" }

type BoolType struct{}

func (BoolType) String() string { return "bool" }

type VoidType struct{}

func (VoidType) String() string { return "void" }

type ListType struct {
	Elem Type
}

func (t ListType) String() string { return "[" + t.Elem.String() + "]" }

type MapType struct {
	Key   Type
	Value Type
}

func (t MapType) String() string {
	return fmt.Sprintf("{%s: %s}", t.Key.String(), t.Value.String())
}

type AnyType struct{}

func (AnyType) String() string { return "any" }

type TypeVar struct {
	Name string
}

func (t *TypeVar) String() string { return t.Name }

type FuncType struct {
	Params []Type
	Return Type
}

func (f FuncType) String() string {
	s := "fun("
	for i, p := range f.Params {
		if i > 0 {
			s += ", "
		}
		s += p.String()
	}
	s += ")"
	if f.Return != nil && f.Return.String() != "void" {
		s += ": " + f.Return.String()
	}
	return s
}

type AnyListType struct{}

func (AnyListType) String() string { return "[_]" }
func (AnyListType) Equal(t Type) bool {
	_, ok := t.(ListType)
	return ok
}

type BuiltinFuncType struct{}

func (BuiltinFuncType) String() string { return "fun(...): void" }

type Subst map[string]Type

// unify attempts to determine if type a can be unified with type b.
// If a substitution map is provided, it will be updated to resolve type variables.
// If subst == nil, unification checks structural equality.
// unify attempts to determine if type a can be unified with type b.
// If a substitution map is provided, it will be updated to resolve type variables.
// If subst == nil, unification checks structural equality.
func unify(a, b Type, subst Subst) bool {
	switch at := a.(type) {

	case AnyType:
		return true

	case *TypeVar:
		if subst != nil {
			if val, ok := subst[at.Name]; ok {
				return unify(val, b, subst)
			}
			subst[at.Name] = b
			return true
		}
		if bt, ok := b.(*TypeVar); ok {
			return at.Name == bt.Name
		}
		return false

	case ListType:
		switch bt := b.(type) {
		case ListType:
			return unify(at.Elem, bt.Elem, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case MapType:
		switch bt := b.(type) {
		case MapType:
			return unify(at.Key, bt.Key, subst) &&
				unify(at.Value, bt.Value, subst)
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(at, val, subst)
				}
				subst[bt.Name] = at
				return true
			}
			return false
		default:
			return false
		}

	case FuncType:
		bt, ok := b.(FuncType)
		if !ok || len(at.Params) != len(bt.Params) {
			return false
		}
		for i := range at.Params {
			if !unify(at.Params[i], bt.Params[i], subst) {
				return false
			}
		}
		return unify(at.Return, bt.Return, subst)

	case IntType:
		_, ok := b.(IntType)
		return ok

	case FloatType:
		_, ok := b.(FloatType)
		return ok

	case StringType:
		_, ok := b.(StringType)
		return ok

	case BoolType:
		_, ok := b.(BoolType)
		return ok

	case VoidType:
		_, ok := b.(VoidType)
		return ok

	default:
		// If a didn't match, maybe b is AnyType or a TypeVar
		switch bt := b.(type) {
		case AnyType:
			return true
		case *TypeVar:
			if subst != nil {
				if val, ok := subst[bt.Name]; ok {
					return unify(a, val, subst)
				}
				subst[bt.Name] = a
				return true
			}
			if atv, ok := a.(*TypeVar); ok {
				return atv.Name == bt.Name
			}
			return false
		default:
			return false
		}
	}
}

// --- Entry Point ---

func Check(prog *parser.Program, env *Env) []error {
	env.SetVar("print", BuiltinFuncType{}, false)
	env.SetVar("len", FuncType{
		Params: []Type{AnyType{}}, // loosely typed
		Return: IntType{},
	}, false)
	env.SetVar("now", FuncType{
		Params: []Type{},
		Return: IntType{},
	}, false)
	env.SetVar("json", FuncType{
		Params: []Type{AnyType{}},
		Return: VoidType{},
	}, false)

	var errs []error
	for _, stmt := range prog.Statements {
		if err := checkStmt(stmt, env, VoidType{}); err != nil {
			errs = append(errs, err)
		}
	}
	return errs
}

// --- Helpers ---

func checkStmt(s *parser.Statement, env *Env, expectedReturn Type) error {
	switch {
	case s.Let != nil:
		name := s.Let.Name
		var typ Type
		if s.Let.Type != nil {
			typ = resolveTypeRef(s.Let.Type)
			if s.Let.Value != nil {
				exprType, err := checkExprWithExpected(s.Let.Value, env, typ)
				if err != nil {
					return err
				}
				if !unify(typ, exprType, nil) {
					return errTypeMismatch(s.Let.Pos, typ, exprType)
				}
			}
		} else if s.Let.Value != nil {
			var err error
			typ, err = checkExprWithExpected(s.Let.Value, env, nil)
			if err != nil {
				return err
			}
		} else {
			return errLetMissingTypeOrValue(s.Let.Pos)
		}
		env.SetVar(name, typ, false)
		return nil

	case s.Var != nil:
		name := s.Var.Name
		var typ Type
		if s.Var.Type != nil {
			typ = resolveTypeRef(s.Var.Type)
			if s.Var.Value != nil {
				exprType, err := checkExprWithExpected(s.Var.Value, env, typ)
				if err != nil {
					return err
				}
				if !unify(typ, exprType, nil) {
					return errTypeMismatch(s.Var.Pos, typ, exprType)
				}
			}
		} else if s.Var.Value != nil {
			var err error
			typ, err = checkExprWithExpected(s.Var.Value, env, nil)
			if err != nil {
				return err
			}
		} else {
			return errLetMissingTypeOrValue(s.Var.Pos)
		}
		env.SetVar(name, typ, true)
		return nil

	case s.Assign != nil:
		rhsType, err := checkExprWithExpected(s.Assign.Value, env, nil)
		if err != nil {
			return err
		}
		lhsType, err := env.GetVar(s.Assign.Name)
		if err != nil {
			return errAssignUndeclared(s.Assign.Pos, s.Assign.Name)
		}
		mutable, err := env.IsMutable(s.Assign.Name)
		if err != nil {
			return errAssignUndeclared(s.Assign.Pos, s.Assign.Name)
		}
		if !mutable {
			return errAssignImmutableVar(s.Assign.Pos, s.Assign.Name)
		}
		if !unify(lhsType, rhsType, nil) {
			return errCannotAssign(s.Assign.Pos, rhsType, s.Assign.Name, lhsType)
		}
		return nil

	case s.For != nil:
		// Check the loop expression (either a collection or a range start)
		sourceType, err := checkExprWithExpected(s.For.Source, env, nil)
		if err != nil {
			return err
		}

		var elemType Type

		if s.For.RangeEnd != nil {
			// It's a range loop: `for i in start..end`
			endType, err := checkExprWithExpected(s.For.RangeEnd, env, nil)
			if err != nil {
				return err
			}
			if !unify(sourceType, IntType{}, nil) || !unify(endType, IntType{}, nil) {
				return errRangeRequiresInts(s.For.Pos)
			}
			elemType = IntType{} // range loop always yields int
		} else {
			// It's a collection loop: `for x in collection`
			switch t := sourceType.(type) {
			case ListType:
				elemType = t.Elem
			case MapType:
				elemType = t.Key // loop iterates over keys
			case AnyType:
				elemType = AnyType{}
			default:
				return errCannotIterate(s.For.Pos, sourceType)
			}
		}

		// Create new scope for the loop variable
		child := NewEnv(env)
		child.SetVar(s.For.Name, elemType, true)

		// Check loop body
		for _, stmt := range s.For.Body {
			if err := checkStmt(stmt, child, expectedReturn); err != nil {
				return err
			}
		}
		return nil

	case s.Fun != nil:
		name := s.Fun.Name
		params := []Type{}
		for _, p := range s.Fun.Params {
			if p.Type == nil {
				return errParamMissingType(s.Fun.Pos, p.Name)
			}
			params = append(params, resolveTypeRef(p.Type))
		}
		var ret Type = VoidType{}
		if s.Fun.Return != nil {
			ret = resolveTypeRef(s.Fun.Return)
		}
		env.SetVar(name, FuncType{Params: params, Return: ret}, false)

		child := NewEnv(env)
		for i, p := range s.Fun.Params {
			child.SetVar(p.Name, params[i], true)
		}
		for _, stmt := range s.Fun.Body {
			if err := checkStmt(stmt, child, ret); err != nil {
				return err
			}
		}
		return nil

	case s.Expr != nil:
		_, err := checkExprWithExpected(s.Expr.Expr, env, nil)
		return err

	case s.Return != nil:
		actual, err := checkExprWithExpected(s.Return.Value, env, expectedReturn)
		if err != nil {
			return err
		}
		if !unify(actual, expectedReturn, nil) {
			return errReturnMismatch(s.Return.Pos, expectedReturn, actual)
		}
		return nil

	case s.Test != nil:
		child := NewEnv(env)
		for _, stmt := range s.Test.Body {
			if err := checkStmt(stmt, child, expectedReturn); err != nil {
				return err
			}
		}
		return nil

	case s.Expect != nil:
		t, err := checkExprWithExpected(s.Expect.Value, env, BoolType{})
		if err != nil {
			return err
		}
		if !unify(t, BoolType{}, nil) {
			return errExpectBoolean(s.Expect.Pos)
		}
		return nil
	}
	return nil
}

func resolveTypeRef(t *parser.TypeRef) Type {
	if t.Fun != nil {
		params := make([]Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p)
		}
		var ret Type = VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return)
		}
		return FuncType{Params: params, Return: ret}
	}

	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return ListType{Elem: resolveTypeRef(args[0])}
			}
		case "map":
			if len(args) == 2 {
				return MapType{
					Key:   resolveTypeRef(args[0]),
					Value: resolveTypeRef(args[1]),
				}
			}
		}
		// Fallback: unknown generic type
		return AnyType{}
	}

	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return IntType{}
		case "float":
			return FloatType{}
		case "string":
			return StringType{}
		case "bool":
			return BoolType{}
		default:
			return AnyType{}
		}
	}

	return AnyType{}
}

func checkExpr(e *parser.Expr, env *Env) (Type, error) {
	return checkExprWithExpected(e, env, nil)
}

func checkExprWithExpected(e *parser.Expr, env *Env, expected Type) (Type, error) {
	actual, err := checkBinaryExpr(e.Binary, env)
	if err != nil {
		return nil, err
	}
	if expected != nil && !unify(actual, expected, nil) {
		return nil, errTypeMismatch(e.Pos, expected, actual)
	}
	return actual, nil
}
func checkBinaryExpr(b *parser.BinaryExpr, env *Env) (Type, error) {
	left, err := checkUnary(b.Left, env, nil)
	if err != nil {
		return nil, err
	}

	for _, op := range b.Right {
		right, err := checkPostfix(op.Right, env, nil)
		if err != nil {
			return nil, err
		}

		switch op.Op {
		case "+", "-", "*", "/", "%":
			// Arithmetic: int, float, or string + string
			switch {
			case unify(left, IntType{}, nil) && unify(right, IntType{}, nil):
				left = IntType{}

			case unify(left, FloatType{}, nil) && unify(right, FloatType{}, nil):
				left = FloatType{}

			case op.Op == "+" && unify(left, StringType{}, nil) && unify(right, StringType{}, nil):
				left = StringType{}

			default:
				return nil, errOperatorMismatch(op.Pos, op.Op, left, right)
			}

		case "==", "!=", "<", "<=", ">", ">=":
			// comparison: any comparable types
			if !unify(left, right, nil) {
				return nil, errIncompatibleComparison(lexer.Position{}) // you can add op.Pos to BinaryOp
			}
			left = BoolType{}

		default:
			return nil, errUnsupportedOperator(op.Pos, op.Op)
		}
	}

	return left, nil
}

/*
func checkExprWithExpected(e *parser.Expr, env *Env, expected Type) (Type, error) {
	return checkEquality(e.Equality, env, expected)
}

func checkEquality(e *parser.Equality, env *Env, expected Type) (Type, error) {
	left, err := checkComparison(e.Left, env, nil)
	if err != nil {
		return nil, err
	}
	for _, op := range e.Right {
		right, err := checkComparison(op.Right, env, nil)
		if err != nil {
			return nil, err
		}
		if !left.Equal(right) {
			return nil, errIncompatibleEquality(op.Pos)
		}
		left = BoolType{}
	}
	return left, nil
}

func checkComparison(c *parser.Comparison, env *Env, expected Type) (Type, error) {
	left, err := checkTerm(c.Left, env, nil)
	if err != nil {
		return nil, err
	}
	for _, op := range c.Right {
		right, err := checkTerm(op.Right, env, nil)
		if err != nil {
			return nil, err
		}
		if !left.Equal(right) {
			return nil, errIncompatibleComparison(op.Pos)
		}
		left = BoolType{}
	}
	return left, nil
}

func checkTerm(t *parser.Term, env *Env, expected Type) (Type, error) {
	left, err := checkFactor(t.Left, env, nil)
	if err != nil {
		return nil, err
	}
	for _, op := range t.Right {
		right, err := checkFactor(op.Right, env, nil)
		if err != nil {
			return nil, err
		}
		if !left.Equal(right) {
			return nil, errTypeMismatch(op.Pos, left, right)
		}
	}
	return left, nil
}

func checkFactor(f *parser.Factor, env *Env, expected Type) (Type, error) {
	left, err := checkUnary(f.Left, env, nil)
	if err != nil {
		return nil, err
	}
	for _, op := range f.Right {
		right, err := checkUnary(op.Right, env, nil)
		if err != nil {
			return nil, err
		}
		if !left.Equal(right) {
			return nil, errTypeMismatch(op.Pos, left, right)
		}
	}
	return left, nil
}
*/

func checkUnary(u *parser.Unary, env *Env, expected Type) (Type, error) {
	return checkPostfix(u.Value, env, expected)
}

func checkPostfix(p *parser.PostfixExpr, env *Env, expected Type) (Type, error) {
	typ, err := checkPrimary(p.Target, env, expected)
	if err != nil {
		return nil, err
	}

	for _, idx := range p.Index {
		switch t := typ.(type) {
		case ListType:
			if idx.Colon == nil {
				// list[i]
				if idx.Start == nil {
					return nil, errMissingIndex(idx.Pos)
				}
				startType, err := checkExpr(idx.Start, env)
				if err != nil {
					return nil, err
				}
				if !unify(startType, IntType{}, nil) {
					return nil, errIndexNotInteger(idx.Pos)
				}
				typ = t.Elem
			} else {
				// list[i:j], list[:j], list[i:], list[:]
				if idx.Start != nil {
					startType, err := checkExpr(idx.Start, env)
					if err != nil {
						return nil, err
					}
					if !unify(startType, IntType{}, nil) {
						return nil, errIndexNotInteger(idx.Pos)
					}
				}
				if idx.End != nil {
					endType, err := checkExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
					if !unify(endType, IntType{}, nil) {
						return nil, errIndexNotInteger(idx.Pos)
					}
				}
				typ = t // list slice returns same list type
			}

		case MapType:
			if idx.Colon != nil {
				return nil, errInvalidMapSlice(idx.Pos)
			}
			if idx.Start == nil {
				return nil, errMissingIndex(idx.Pos)
			}
			keyType, err := checkExpr(idx.Start, env)
			if err != nil {
				return nil, err
			}
			if !unify(keyType, t.Key, nil) {
				return nil, errIndexTypeMismatch(idx.Pos, t.Key, keyType)
			}
			typ = t.Value

		default:
			return nil, errNotIndexable(p.Target.Pos, typ)
		}
	}

	return typ, nil
}

func checkPrimary(p *parser.Primary, env *Env, expected Type) (Type, error) {
	switch {
	case p.Lit != nil:
		switch {
		case p.Lit.Int != nil:
			return IntType{}, nil
		case p.Lit.Float != nil:
			return FloatType{}, nil
		case p.Lit.Str != nil:
			return StringType{}, nil
		case p.Lit.Bool != nil:
			return BoolType{}, nil
		}

	case p.Selector != nil:
		typ, err := env.GetVar(p.Selector.Root)
		if err != nil {
			return nil, errUnknownVariable(p.Pos, p.Selector.Root)
		}
		// TODO: implement proper struct/stream field access checking
		return typ, nil

	case p.Call != nil:
		fnType, err := env.GetVar(p.Call.Func)
		if err != nil {
			return nil, errUnknownFunction(p.Pos, p.Call.Func)
		}

		switch ft := fnType.(type) {
		case FuncType:
			argCount := len(p.Call.Args)
			paramCount := len(ft.Params)

			if argCount > paramCount {
				return nil, errTooManyArgs(p.Pos, paramCount, argCount)
			}

			for i := 0; i < argCount; i++ {
				argType, err := checkExprWithExpected(p.Call.Args[i], env, ft.Params[i])
				if err != nil {
					return nil, err
				}
				if !unify(argType, ft.Params[i], nil) {
					return nil, errArgTypeMismatch(p.Pos, i, ft.Params[i], argType)
				}
			}

			if argCount == paramCount {
				return ft.Return, nil
			}
			return curryFuncType(ft.Params[argCount:], ft.Return), nil

		case BuiltinFuncType:
			for _, arg := range p.Call.Args {
				if _, err := checkExpr(arg, env); err != nil {
					return nil, err
				}
			}
			return VoidType{}, nil

		default:
			return nil, errNotFunction(p.Pos, p.Call.Func)
		}

	case p.List != nil:
		var elemType Type = nil
		for _, elem := range p.List.Elems {
			t, err := checkExpr(elem, env)
			if err != nil {
				return nil, err
			}
			if elemType == nil {
				elemType = t
			} else if !unify(elemType, t, nil) {
				elemType = AnyType{} // fallback if mixed types
			}
		}
		if elemType == nil {
			elemType = AnyType{}
		}
		return ListType{Elem: elemType}, nil

	case p.Map != nil:
		var keyT, valT Type
		for _, item := range p.Map.Items {
			kt, err := checkExpr(item.Key, env)
			if err != nil {
				return nil, err
			}
			vt, err := checkExpr(item.Value, env)
			if err != nil {
				return nil, err
			}
			if keyT == nil {
				keyT = kt
			} else if !unify(keyT, kt, nil) {
				keyT = AnyType{}
			}
			if valT == nil {
				valT = vt
			} else if !unify(valT, vt, nil) {
				valT = AnyType{}
			}
		}
		if keyT == nil {
			keyT = AnyType{}
		}
		if valT == nil {
			valT = AnyType{}
		}
		return MapType{Key: keyT, Value: valT}, nil

	case p.FunExpr != nil:
		return checkFunExpr(p.FunExpr, env, expected, p.Pos)

	case p.Group != nil:
		return checkExprWithExpected(p.Group, env, expected)
	}

	return nil, errInvalidPrimary(p.Pos)
}

func checkFunExpr(f *parser.FunExpr, env *Env, expected Type, pos lexer.Position) (Type, error) {
	var expectedFunc *FuncType
	if ft, ok := expected.(FuncType); ok {
		expectedFunc = &ft
	}

	paramTypes := make([]Type, len(f.Params))
	for i, p := range f.Params {
		if p.Type == nil {
			return nil, errParamMissingType(pos, p.Name)
		}
		paramTypes[i] = resolveTypeRef(p.Type)
	}

	var declaredRet Type
	if f.Return != nil {
		declaredRet = resolveTypeRef(f.Return)
	} else if expectedFunc != nil {
		declaredRet = expectedFunc.Return
	} else {
		declaredRet = &TypeVar{Name: "R"}
	}

	child := NewEnv(env)
	for i, p := range f.Params {
		child.SetVar(p.Name, paramTypes[i], true)
	}

	subst := Subst{}
	var actualRet Type
	var err error

	if f.ExprBody != nil {
		actualRet, err = checkExpr(f.ExprBody, child)
		if err != nil {
			return nil, err
		}
	} else {
		// Block body
		for _, stmt := range f.BlockBody {
			if err := checkStmt(stmt, child, declaredRet); err != nil {
				return nil, err
			}
		}
		actualRet = declaredRet
	}

	if !unify(declaredRet, actualRet, subst) {
		return nil, errTypeMismatch(pos, declaredRet, actualRet)
	}

	// Final substitution: resolve any type variable that was inferred
	if tv, ok := declaredRet.(*TypeVar); ok {
		if resolved, ok := subst[tv.Name]; ok {
			declaredRet = resolved
		}
	}

	return FuncType{Params: paramTypes, Return: declaredRet}, nil
}

func curryFuncType(params []Type, ret Type) Type {
	if len(params) == 0 {
		return ret
	}
	return FuncType{
		Params: []Type{params[0]},
		Return: curryFuncType(params[1:], ret),
	}
}
