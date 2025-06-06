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

// Int64Type specifically represents 64-bit integers. It unifies with IntType
// for most operations but allows the Go compiler to emit int64 values where
// precision matters (e.g. now()).
type Int64Type struct{}

func (Int64Type) String() string { return "int64" }

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

type StructType struct {
	Name        string
	Fields      map[string]Type
	Methods     map[string]*parser.FunStmt
	MethodTypes map[string]FuncType
}

func (t StructType) String() string { return t.Name }

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

	case StructType:
		bt, ok := b.(StructType)
		if !ok || at.Name != bt.Name {
			return false
		}
		if len(at.Fields) != len(bt.Fields) {
			return false
		}
		for k, v := range at.Fields {
			if bv, ok := bt.Fields[k]; ok {
				if !unify(v, bv, subst) {
					return false
				}
			} else {
				return false
			}
		}
		return true

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

	case Int64Type:
		switch b.(type) {
		case Int64Type, IntType:
			return true
		default:
			return false
		}

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
		Return: Int64Type{},
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
			typ = resolveTypeRef(s.Let.Type, env)
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
			typ = resolveTypeRef(s.Var.Type, env)
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
			if !(unify(sourceType, IntType{}, nil) || unify(sourceType, Int64Type{}, nil)) ||
				!(unify(endType, IntType{}, nil) || unify(endType, Int64Type{}, nil)) {
				return errRangeRequiresInts(s.For.Pos)
			}
			// Range loop yields integers matching the input type.
			if _, ok := sourceType.(Int64Type); ok {
				elemType = Int64Type{}
			} else {
				elemType = IntType{}
			}
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

	case s.Type != nil:
		fields := map[string]Type{}
		methods := map[string]*parser.FunStmt{}
		methodTypes := map[string]FuncType{}
		for _, m := range s.Type.Members {
			if m.Field != nil {
				fields[m.Field.Name] = resolveTypeRef(m.Field.Type, env)
			} else if m.Method != nil {
				methods[m.Method.Name] = m.Method
				params := make([]Type, len(m.Method.Params))
				for i, p := range m.Method.Params {
					if p.Type == nil {
						return errParamMissingType(m.Method.Pos, p.Name)
					}
					params[i] = resolveTypeRef(p.Type, env)
				}
				var ret Type = VoidType{}
				if m.Method.Return != nil {
					ret = resolveTypeRef(m.Method.Return, env)
				}
				methodTypes[m.Method.Name] = FuncType{Params: params, Return: ret}
			}
		}
		env.SetStruct(s.Type.Name, StructType{Name: s.Type.Name, Fields: fields, Methods: methods, MethodTypes: methodTypes})

		// Type check methods
		for name, fun := range methods {
			ft := methodTypes[name]
			child := NewEnv(env)
			for fName, fType := range fields {
				child.SetVar(fName, fType, true)
			}
			for mName, mft := range methodTypes {
				child.SetVar(mName, mft, false)
			}
			for i, p := range fun.Params {
				child.SetVar(p.Name, ft.Params[i], true)
			}
			for _, stmt := range fun.Body {
				if err := checkStmt(stmt, child, ft.Return); err != nil {
					return err
				}
			}
		}
		return nil

	case s.Model != nil:
		for _, f := range s.Model.Fields {
			if _, err := checkExpr(f.Value, env); err != nil {
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
			params = append(params, resolveTypeRef(p.Type, env))
		}
		var ret Type = VoidType{}
		if s.Fun.Return != nil {
			ret = resolveTypeRef(s.Fun.Return, env)
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

func resolveTypeRef(t *parser.TypeRef, env *Env) Type {
	if t.Fun != nil {
		params := make([]Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret Type = VoidType{}
		if t.Fun.Return != nil {
			ret = resolveTypeRef(t.Fun.Return, env)
		}
		return FuncType{Params: params, Return: ret}
	}

	if t.Generic != nil {
		name := t.Generic.Name
		args := t.Generic.Args
		switch name {
		case "list":
			if len(args) == 1 {
				return ListType{Elem: resolveTypeRef(args[0], env)}
			}
		case "map":
			if len(args) == 2 {
				return MapType{
					Key:   resolveTypeRef(args[0], env),
					Value: resolveTypeRef(args[1], env),
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
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
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

	type token struct {
		pos lexer.Position
		op  string
	}

	operands := []Type{left}
	operators := []token{}

	for _, part := range b.Right {
		typ, err := checkPostfix(part.Right, env, nil)
		if err != nil {
			return nil, err
		}
		operands = append(operands, typ)
		operators = append(operators, token{part.Pos, part.Op})
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!="},
		{"&&"},
		{"||"},
	} {
		for i := 0; i < len(operators); {
			op := operators[i].op
			if contains(level, op) {
				l := operands[i]
				r := operands[i+1]
				res, err := applyBinaryType(operators[i].pos, op, l, r)
				if err != nil {
					return nil, err
				}
				operands[i] = res
				operands = append(operands[:i+1], operands[i+2:]...)
				operators = append(operators[:i], operators[i+1:]...)
			} else {
				i++
			}
		}
	}

	if len(operands) != 1 {
		return nil, fmt.Errorf("unexpected state after binary type eval")
	}
	return operands[0], nil
}

func applyBinaryType(pos lexer.Position, op string, left, right Type) (Type, error) {
	switch op {
	case "+", "-", "*", "/", "%":
		switch {
		case (unify(left, IntType{}, nil) || unify(left, Int64Type{}, nil)) &&
			(unify(right, IntType{}, nil) || unify(right, Int64Type{}, nil)):
			if _, ok := left.(Int64Type); ok {
				return Int64Type{}, nil
			}
			if _, ok := right.(Int64Type); ok {
				return Int64Type{}, nil
			}
			return IntType{}, nil
		case unify(left, FloatType{}, nil) && unify(right, FloatType{}, nil):
			return FloatType{}, nil
		case op == "+" && unify(left, StringType{}, nil) && unify(right, StringType{}, nil):
			return StringType{}, nil
		default:
			return nil, errOperatorMismatch(pos, op, left, right)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		if !unify(left, right, nil) {
			return nil, errIncompatibleComparison(pos)
		}
		return BoolType{}, nil
	case "&&", "||":
		if !(unify(left, BoolType{}, nil) && unify(right, BoolType{}, nil)) {
			return nil, errOperatorMismatch(pos, op, left, right)
		}
		return BoolType{}, nil
	default:
		return nil, errUnsupportedOperator(pos, op)
	}
}

func contains(ops []string, op string) bool {
	for _, o := range ops {
		if o == op {
			return true
		}
	}
	return false
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
				if !(unify(startType, IntType{}, nil) || unify(startType, Int64Type{}, nil)) {
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
					if !(unify(startType, IntType{}, nil) || unify(startType, Int64Type{}, nil)) {
						return nil, errIndexNotInteger(idx.Pos)
					}
				}
				if idx.End != nil {
					endType, err := checkExpr(idx.End, env)
					if err != nil {
						return nil, err
					}
					if !(unify(endType, IntType{}, nil) || unify(endType, Int64Type{}, nil)) {
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
		for _, field := range p.Selector.Tail {
			st, ok := typ.(StructType)
			if !ok {
				return nil, errNotStruct(p.Pos, typ)
			}
			ft, ok := st.Fields[field]
			if !ok {
				return nil, errUnknownField(p.Pos, field, st)
			}
			typ = ft
		}
		return typ, nil

	case p.Method != nil:
		path := append([]string{}, p.Method.Path...)
		if len(path) == 0 {
			return nil, errUnknownFunction(p.Pos, "")
		}
		methodName := path[len(path)-1]
		recvSel := &parser.SelectorExpr{Root: p.Method.Root, Tail: path[:len(path)-1]}
		recvType, err := checkSelectorExpr(recvSel, env)
		if err != nil {
			return nil, err
		}
		st, ok := recvType.(StructType)
		if !ok {
			return nil, errNotStruct(p.Pos, recvType)
		}
		mt, ok := st.MethodTypes[methodName]
		if !ok {
			return nil, errUnknownFunction(p.Pos, methodName)
		}
		if len(p.Method.Args) != len(mt.Params) {
			if len(p.Method.Args) > len(mt.Params) {
				return nil, errTooManyArgs(p.Pos, len(mt.Params), len(p.Method.Args))
			}
			return nil, errTooManyArgs(p.Pos, len(mt.Params), len(p.Method.Args))
		}
		for i, arg := range p.Method.Args {
			at, err := checkExprWithExpected(arg, env, mt.Params[i])
			if err != nil {
				return nil, err
			}
			if !unify(at, mt.Params[i], nil) {
				return nil, errArgTypeMismatch(p.Pos, i, mt.Params[i], at)
			}
		}
		return mt.Return, nil

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

	case p.Struct != nil:
		st, ok := env.GetStruct(p.Struct.Name)
		if !ok {
			// treat unknown struct literal as map for tool specs
			for _, field := range p.Struct.Fields {
				if _, err := checkExpr(field.Value, env); err != nil {
					return nil, err
				}
			}
			return MapType{Key: StringType{}, Value: AnyType{}}, nil
		}
		for _, field := range p.Struct.Fields {
			ft, ok := st.Fields[field.Name]
			if !ok {
				return nil, errUnknownField(p.Pos, field.Name, st)
			}
			valT, err := checkExpr(field.Value, env)
			if err != nil {
				return nil, err
			}
			if !unify(ft, valT, nil) {
				return nil, errTypeMismatch(field.Value.Pos, ft, valT)
			}
		}
		return st, nil

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

	case p.Match != nil:
		return checkMatchExpr(p.Match, env, expected)

	case p.Generate != nil:
		for _, f := range p.Generate.Fields {
			var expect Type
			switch f.Name {
			case "prompt", "model", "text":
				expect = StringType{}
			case "temperature", "top_p":
				expect = FloatType{}
			case "max_tokens":
				expect = IntType{}
			case "stop":
				expect = ListType{Elem: StringType{}}
			case "normalize":
				expect = BoolType{}
			case "args", "tools", "tool_choice":
				expect = nil
			}
			var err error
			if expect != nil {
				_, err = checkExprWithExpected(f.Value, env, expect)
			} else {
				_, err = checkExpr(f.Value, env)
			}
			if err != nil {
				return nil, err
			}
		}
		if p.Generate.Target == "text" {
			return StringType{}, nil
		}
		if p.Generate.Target == "embedding" {
			return ListType{Elem: FloatType{}}, nil
		}
		st, ok := env.GetStruct(p.Generate.Target)
		if !ok {
			return nil, errUnknownType(p.Pos, p.Generate.Target)
		}
		return st, nil

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
		paramTypes[i] = resolveTypeRef(p.Type, env)
	}

	var declaredRet Type
	if f.Return != nil {
		declaredRet = resolveTypeRef(f.Return, env)
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

func checkSelectorExpr(sel *parser.SelectorExpr, env *Env) (Type, error) {
	typ, err := env.GetVar(sel.Root)
	if err != nil {
		return nil, errUnknownVariable(lexer.Position{}, sel.Root)
	}
	for _, field := range sel.Tail {
		st, ok := typ.(StructType)
		if !ok {
			return nil, errNotStruct(lexer.Position{}, typ)
		}
		ft, ok := st.Fields[field]
		if !ok {
			return nil, errUnknownField(lexer.Position{}, field, st)
		}
		typ = ft
	}
	return typ, nil
}

func checkMatchExpr(m *parser.MatchExpr, env *Env, expected Type) (Type, error) {
	targetType, err := checkExpr(m.Target, env)
	if err != nil {
		return nil, err
	}
	var resultType Type
	for _, c := range m.Cases {
		if !isUnderscoreExpr(c.Pattern) {
			pType, err := checkExpr(c.Pattern, env)
			if err != nil {
				return nil, err
			}
			if !unify(targetType, pType, nil) {
				return nil, errTypeMismatch(c.Pos, targetType, pType)
			}
		}
		rType, err := checkExprWithExpected(c.Result, env, expected)
		if err != nil {
			return nil, err
		}
		if resultType == nil {
			resultType = rType
		} else if !unify(resultType, rType, nil) {
			resultType = AnyType{}
		}
	}
	if resultType == nil {
		resultType = AnyType{}
	}
	if expected != nil && !unify(resultType, expected, nil) {
		return nil, errTypeMismatch(m.Pos, expected, resultType)
	}
	return resultType, nil
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
	if len(p.Index) != 0 {
		return false
	}
	if p.Target.Selector != nil && p.Target.Selector.Root == "_" && len(p.Target.Selector.Tail) == 0 {
		return true
	}
	return false
}
