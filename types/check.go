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

type BigIntType struct{}

func (BigIntType) String() string { return "bigint" }

type BigRatType struct{}

func (BigRatType) String() string { return "bigrat" }

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

type GroupType struct {
	Key  Type
	Elem Type
}

func (GroupType) String() string { return "group" }

type StructType struct {
	Name    string
	Fields  map[string]Type
	Order   []string
	Methods map[string]Method
}

type Method struct {
	Decl *parser.FunStmt
	Type FuncType
}

func (t StructType) String() string { return t.Name }

type UnionType struct {
	Name     string
	Variants map[string]StructType
}

func (t UnionType) String() string { return t.Name }

type AnyType struct{}

func (AnyType) String() string { return "any" }

type TypeVar struct {
	Name string
}

func (t *TypeVar) String() string { return t.Name }

type FuncType struct {
	Params   []Type
	Return   Type
	Pure     bool
	Variadic bool
}

func (f FuncType) String() string {
	s := "fun("
	for i, p := range f.Params {
		if i > 0 {
			s += ", "
		}
		s += p.String()
	}
	if f.Variadic {
		if len(f.Params) > 0 {
			s += ", ..."
		} else {
			s += "..."
		}
	}
	s += ")"
	if f.Return != nil && f.Return.String() != "void" {
		s += ": " + f.Return.String()
	}
	if f.Pure {
		s += " [pure]"
	}
	return s
}

type Subst map[string]Type

// unify attempts to determine if type a can be unified with type b.
// If a substitution map is provided, it will be updated to resolve type variables.
// If subst == nil, unification checks structural equality.
// unify attempts to determine if type a can be unified with type b.
// If a substitution map is provided, it will be updated to resolve type variables.
// If subst == nil, unification checks structural equality.
func unify(a, b Type, subst Subst) bool {
	if _, ok := b.(AnyType); ok {
		return true
	}
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

	case GroupType:
		switch bt := b.(type) {
		case GroupType:
			return unify(at.Key, bt.Key, subst) && unify(at.Elem, bt.Elem, subst)
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
		switch bt := b.(type) {
		case StructType:
			if at.Name != "" && bt.Name != "" && at.Name != bt.Name {
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
		case UnionType:
			if vt, ok := bt.Variants[at.Name]; ok {
				return unify(at, vt, subst)
			}
			return false
		default:
			return false
		}

	case UnionType:
		switch bt := b.(type) {
		case UnionType:
			if at.Name != bt.Name || len(at.Variants) != len(bt.Variants) {
				return false
			}
			for k, v := range at.Variants {
				bv, ok := bt.Variants[k]
				if !ok || !unify(v, bv, subst) {
					return false
				}
			}
			return true
		case StructType:
			if vt, ok := at.Variants[bt.Name]; ok {
				return unify(vt, bt, subst)
			}
			return false
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
		if !ok || at.Variadic != bt.Variadic || len(at.Params) != len(bt.Params) {
			return false
		}
		for i := range at.Params {
			if !unify(at.Params[i], bt.Params[i], subst) {
				return false
			}
		}
		return unify(at.Return, bt.Return, subst)

	case IntType:
		switch b.(type) {
		case IntType, BigIntType:
			return true
		default:
			return false
		}

	case Int64Type:
		switch b.(type) {
		case Int64Type, IntType:
			return true
		default:
			return false
		}

	case BigIntType:
		switch b.(type) {
		case BigIntType, IntType, Int64Type:
			return true
		default:
			return false
		}

	case BigRatType:
		switch b.(type) {
		case BigRatType, FloatType, IntType, Int64Type, BigIntType:
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
	env.SetVar("print", FuncType{
		Params:   []Type{AnyType{}},
		Return:   VoidType{},
		Variadic: true,
	}, false)
	env.SetVar("len", FuncType{
		Params: []Type{AnyType{}}, // loosely typed
		Return: IntType{},
		Pure:   true,
	}, false)
	env.SetVar("append", FuncType{
		Params: []Type{ListType{Elem: AnyType{}}, AnyType{}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("concat", FuncType{
		Params:   []Type{ListType{Elem: AnyType{}}},
		Return:   ListType{Elem: AnyType{}},
		Pure:     true,
		Variadic: true,
	}, false)
	env.SetVar("first", FuncType{
		Params: []Type{ListType{Elem: AnyType{}}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("reverse", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("distinct", FuncType{
		Params: []Type{ListType{Elem: AnyType{}}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("push", FuncType{
		Params: []Type{ListType{Elem: AnyType{}}, AnyType{}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("keys", FuncType{
		Params: []Type{MapType{Key: AnyType{}, Value: AnyType{}}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("values", FuncType{
		Params: []Type{MapType{Key: AnyType{}, Value: AnyType{}}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("range", FuncType{
		Params:   []Type{IntType{}},
		Return:   ListType{Elem: IntType{}},
		Pure:     true,
		Variadic: true,
	}, false)
	env.SetVar("now", FuncType{
		Params: []Type{},
		Return: Int64Type{},
	}, false)
	env.SetVar("json", FuncType{
		Params: []Type{AnyType{}},
		Return: VoidType{},
	}, false)
	env.SetVar("to_json", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("str", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("upper", FuncType{
		Params: []Type{StringType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("lower", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("trim", FuncType{
		Params: []Type{StringType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("contains", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: BoolType{},
		Pure:   true,
	}, false)
	env.SetVar("split", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: ListType{Elem: StringType{}},
		Pure:   true,
	}, false)
	env.SetVar("join", FuncType{
		Params: []Type{ListType{Elem: StringType{}}, StringType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("substring", FuncType{
		Params: []Type{StringType{}, IntType{}, IntType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("substr", FuncType{
		Params: []Type{StringType{}, IntType{}, IntType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("num", FuncType{
		Params: []Type{AnyType{}},
		Return: BigIntType{},
		Pure:   true,
	}, false)
	env.SetVar("denom", FuncType{
		Params: []Type{AnyType{}},
		Return: BigIntType{},
		Pure:   true,
	}, false)
	env.SetVar("input", FuncType{
		Params: []Type{},
		Return: StringType{},
	}, false)
	env.SetVar("count", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
		Pure:   true,
	}, false)
	env.SetVar("exists", FuncType{
		Params: []Type{AnyType{}},
		Return: BoolType{},
		Pure:   true,
	}, false)
	env.SetVar("avg", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
		Pure:   true,
	}, false)
	env.SetVar("abs", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("ceil", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
		Pure:   true,
	}, false)
	env.SetVar("floor", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
		Pure:   true,
	}, false)
	env.SetVar("sum", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
		Pure:   true,
	}, false)
	env.SetVar("keys", FuncType{
		Params: []Type{MapType{Key: AnyType{}, Value: AnyType{}}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("min", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("max", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("reduce", FuncType{
		Params: []Type{AnyType{}, AnyType{}, AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	env.SetVar("keys", FuncType{
		Params: []Type{AnyType{}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("eval", FuncType{
		Params: []Type{StringType{}},
		Return: AnyType{},
	}, false)

	var errs []error

	// First process all type declarations so functions can reference them
	// regardless of their order in the source file.
	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := checkStmt(stmt, env, VoidType{}); err != nil {
				errs = append(errs, err)
			}
		}
	}

	for _, stmt := range prog.Statements {
		if stmt.Type == nil {
			if err := checkStmt(stmt, env, VoidType{}); err != nil {
				errs = append(errs, err)
			}
		}
	}
	return errs
}

// --- Helpers ---

func buildStreamFields(fields []*parser.StreamField, env *Env) (map[string]Type, []string) {
	out := map[string]Type{}
	order := []string{}
	for _, f := range fields {
		if f == nil {
			continue
		}
		out[f.Name] = resolveTypeRef(f.Type, env)
		order = append(order, f.Name)
	}
	return out, order
}

func checkStmt(s *parser.Statement, env *Env, expectedReturn Type) error {
	switch {
	case s.Stream != nil:
		fields, order := buildStreamFields(s.Stream.Fields, env)
		st := StructType{Name: s.Stream.Name, Fields: fields, Order: order}
		env.SetStream(s.Stream.Name, st)
		env.SetStruct(s.Stream.Name, st)
		env.types[s.Stream.Name] = st
		return nil

	case s.Agent != nil:
		fields := map[string]Type{}
		order := []string{}
		methods := map[string]Method{}
		for _, blk := range s.Agent.Body {
			switch {
			case blk.Let != nil:
				var t Type = AnyType{}
				if blk.Let.Type != nil {
					t = resolveTypeRef(blk.Let.Type, env)
				}
				fields[blk.Let.Name] = t
				order = append(order, blk.Let.Name)
			case blk.Var != nil:
				var t Type = AnyType{}
				if blk.Var.Type != nil {
					t = resolveTypeRef(blk.Var.Type, env)
				}
				fields[blk.Var.Name] = t
				order = append(order, blk.Var.Name)
			case blk.Intent != nil:
				params := make([]Type, len(blk.Intent.Params))
				for i, p := range blk.Intent.Params {
					if p.Type != nil {
						params[i] = resolveTypeRef(p.Type, env)
					} else {
						params[i] = AnyType{}
					}
				}
				var ret Type = AnyType{}
				if blk.Intent.Return != nil {
					ret = resolveTypeRef(blk.Intent.Return, env)
				}
				pure := isPureFunction(&parser.FunStmt{Params: blk.Intent.Params, Return: blk.Intent.Return, Body: blk.Intent.Body}, env)
				methods[blk.Intent.Name] = Method{Decl: &parser.FunStmt{Params: blk.Intent.Params, Return: blk.Intent.Return, Body: blk.Intent.Body}, Type: FuncType{Params: params, Return: ret, Pure: pure}}
			}
		}
		st := StructType{Name: s.Agent.Name, Fields: fields, Order: order, Methods: methods}
		env.SetStruct(s.Agent.Name, st)
		env.SetAgent(s.Agent.Name, s.Agent)
		env.types[s.Agent.Name] = st
		return nil

	case s.On != nil:
		st, ok := env.GetStream(s.On.Stream)
		if !ok {
			return errUnknownStream(s.On.Pos, s.On.Stream)
		}
		child := NewEnv(env)
		child.SetVar(s.On.Alias, st, true)
		for _, stmt := range s.On.Body {
			if err := checkStmt(stmt, child, expectedReturn); err != nil {
				return err
			}
		}
		return nil

	case s.Emit != nil:
		st, ok := env.GetStream(s.Emit.Stream)
		if !ok {
			return errUnknownStream(s.Emit.Pos, s.Emit.Stream)
		}
		for _, f := range s.Emit.Fields {
			ft, ok := st.Fields[f.Name]
			if !ok {
				return errUnknownField(f.Pos, f.Name, st)
			}
			if _, err := checkExprWithExpected(f.Value, env, ft); err != nil {
				return err
			}
		}
		return nil

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

	case s.Import != nil:
		alias := s.Import.As
		if alias == "" {
			alias = parser.AliasFromPath(s.Import.Path)
		}
		env.SetVar(alias, AnyType{}, false)
		return nil

	case s.ExternVar != nil:
		var typ Type = AnyType{}
		if s.ExternVar.Type != nil {
			typ = resolveTypeRef(s.ExternVar.Type, env)
		}
		env.SetVar(s.ExternVar.Name(), typ, false)
		return nil

	case s.ExternFun != nil:
		params := make([]Type, len(s.ExternFun.Params))
		for i, p := range s.ExternFun.Params {
			if p.Type != nil {
				params[i] = resolveTypeRef(p.Type, env)
			} else {
				params[i] = AnyType{}
			}
		}
		var ret Type = AnyType{}
		if s.ExternFun.Return != nil {
			ret = resolveTypeRef(s.ExternFun.Return, env)
		}
		env.SetVar(s.ExternFun.Name(), FuncType{Params: params, Return: ret}, false)
		return nil

	case s.Fact != nil:
		return nil

	case s.Rule != nil:
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
		if len(s.Assign.Index) > 0 {
			for _, idx := range s.Assign.Index {
				switch lt := lhsType.(type) {
				case MapType:
					if idx.Colon != nil {
						return errInvalidMapSlice(idx.Pos)
					}
					keyType, err := checkExpr(idx.Start, env)
					if err != nil {
						return err
					}
					if !unify(keyType, lt.Key, nil) {
						return errIndexTypeMismatch(idx.Pos, lt.Key, keyType)
					}
					lhsType = lt.Value
				case ListType:
					if idx.Colon != nil {
						if idx.Start != nil {
							t, err := checkExpr(idx.Start, env)
							if err != nil {
								return err
							}
							if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
								return errIndexNotInteger(idx.Pos)
							}
						}
						if idx.End != nil {
							t, err := checkExpr(idx.End, env)
							if err != nil {
								return err
							}
							if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
								return errIndexNotInteger(idx.Pos)
							}
						}
						lhsType = lt
					} else {
						idxType, err := checkExpr(idx.Start, env)
						if err != nil {
							return err
						}
						if _, ok := idxType.(IntType); !ok {
							if _, ok := idxType.(Int64Type); !ok {
								return errIndexNotInteger(idx.Pos)
							}
						}
						lhsType = lt.Elem
					}
				case StringType:
					if idx.Start == nil && idx.Colon == nil {
						return errMissingIndex(idx.Pos)
					}
					if idx.Start != nil {
						t, err := checkExpr(idx.Start, env)
						if err != nil {
							return err
						}
						if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
							return errIndexNotInteger(idx.Pos)
						}
					}
					if idx.End != nil {
						t, err := checkExpr(idx.End, env)
						if err != nil {
							return err
						}
						if !(unify(t, IntType{}, nil) || unify(t, Int64Type{}, nil)) {
							return errIndexNotInteger(idx.Pos)
						}
					}
					if idx.Colon != nil {
						lhsType = StringType{}
					} else {
						lhsType = StringType{}
					}
				default:
					return errNotIndexable(s.Assign.Pos, lhsType)
				}
			}
		}
		if len(s.Assign.Field) > 0 {
			for _, fop := range s.Assign.Field {
				field := fop.Name
				switch lt := lhsType.(type) {
				case StructType:
					ft, ok := lt.Fields[field]
					if !ok {
						return errUnknownField(fop.Pos, field, lt)
					}
					lhsType = ft
				case MapType:
					if unify(lt.Key, StringType{}, nil) {
						lhsType = lt.Value
					} else {
						return errNotStruct(fop.Pos, lt)
					}
				case AnyType:
					lhsType = AnyType{}
				default:
					return errNotStruct(fop.Pos, lt)
				}
			}
		}
		if !unify(lhsType, rhsType, nil) {
			return errCannotAssign(s.Assign.Pos, rhsType, s.Assign.Name, lhsType)
		}
		return nil

	case s.Fetch != nil:
		// type of the fetched value is unknown (any)
		if _, err := checkExprWithExpected(s.Fetch.URL, env, StringType{}); err != nil {
			return err
		}
		if s.Fetch.With != nil {
			if _, err := checkExpr(s.Fetch.With, env); err != nil {
				return err
			}
		}
		env.SetVar(s.Fetch.Target, AnyType{}, false)
		return nil

	case s.Update != nil:
		listType, err := env.GetVar(s.Update.Target)
		if err != nil {
			return errAssignUndeclared(s.Update.Pos, s.Update.Target)
		}
		lt, ok := listType.(ListType)
		if !ok {
			return errQuerySourceList(s.Update.Pos)
		}
		st, ok := lt.Elem.(StructType)
		if !ok {
			return fmt.Errorf("update element is not struct")
		}
		child := NewEnv(env)
		for name, t := range st.Fields {
			child.SetVar(name, t, true)
		}
		for _, item := range s.Update.Set.Items {
			if key, ok := stringKey(item.Key); ok {
				ft, ok2 := st.Fields[key]
				if !ok2 {
					return errUnknownField(item.Pos, key, st)
				}
				vt, err := checkExpr(item.Value, child)
				if err != nil {
					return err
				}
				if !unify(ft, vt, nil) {
					return errTypeMismatch(item.Value.Pos, ft, vt)
				}
			}
		}
		if s.Update.Where != nil {
			wt, err := checkExprWithExpected(s.Update.Where, child, BoolType{})
			if err != nil {
				return err
			}
			if !unify(wt, BoolType{}, nil) {
				return errWhereBoolean(s.Update.Where.Pos)
			}
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
			case StringType:
				elemType = StringType{}
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
		if len(s.Type.Members) > 0 {
			fields := map[string]Type{}
			order := []string{}
			methods := map[string]Method{}
			st := StructType{Name: s.Type.Name, Fields: fields, Order: order, Methods: methods}
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			// First pass: collect fields
			for _, m := range s.Type.Members {
				if m.Field != nil {
					fields[m.Field.Name] = resolveTypeRef(m.Field.Type, env)
					order = append(order, m.Field.Name)
				}
			}
			// Second pass: check methods
			for _, m := range s.Type.Members {
				if m.Method != nil {
					params := []Type{}
					for _, p := range m.Method.Params {
						if p.Type == nil {
							return errParamMissingType(m.Method.Pos, p.Name)
						}
						params = append(params, resolveTypeRef(p.Type, env))
					}
					var ret Type = VoidType{}
					if m.Method.Return != nil {
						ret = resolveTypeRef(m.Method.Return, env)
					}
					methodEnv := NewEnv(env)
					for name, t := range fields {
						methodEnv.SetVar(name, t, true)
					}
					for i, p := range m.Method.Params {
						methodEnv.SetVar(p.Name, params[i], true)
					}
					for _, stmt := range m.Method.Body {
						if err := checkStmt(stmt, methodEnv, ret); err != nil {
							return err
						}
					}
					pure := isPureFunction(&parser.FunStmt{Params: m.Method.Params, Return: m.Method.Return, Body: m.Method.Body}, methodEnv)
					methods[m.Method.Name] = Method{Decl: m.Method, Type: FuncType{Params: params, Return: ret, Pure: pure}}
				}
			}
			st.Fields = fields
			st.Order = order
			st.Methods = methods
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			return nil
		}
		if len(s.Type.Variants) > 0 {
			variants := map[string]StructType{}
			for _, v := range s.Type.Variants {
				vf := map[string]Type{}
				order := []string{}
				for _, f := range v.Fields {
					vf[f.Name] = resolveTypeRef(f.Type, env)
					order = append(order, f.Name)
				}
				st := StructType{Name: v.Name, Fields: vf, Order: order}
				variants[v.Name] = st
				env.SetStruct(v.Name, st)
				params := make([]Type, 0, len(v.Fields))
				for _, f := range v.Fields {
					params = append(params, resolveTypeRef(f.Type, env))
				}
				env.SetFuncType(v.Name, FuncType{Params: params, Return: UnionType{Name: s.Type.Name, Variants: nil}})
			}
			ut := UnionType{Name: s.Type.Name, Variants: variants}
			env.SetUnion(s.Type.Name, ut)
			env.types[s.Type.Name] = ut
			return nil
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
		pure := isPureFunction(s.Fun, env)
		env.SetVar(name, FuncType{Params: params, Return: ret, Pure: pure}, false)
		env.SetFunc(name, s.Fun)

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

	if t.Struct != nil {
		fields := map[string]Type{}
		order := []string{}
		for _, f := range t.Struct.Fields {
			fields[f.Name] = resolveTypeRef(f.Type, env)
			order = append(order, f.Name)
		}
		return StructType{Name: "", Fields: fields, Order: order}
	}

	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return IntType{}
		case "float":
			return FloatType{}
		case "bigint":
			return BigIntType{}
		case "bigrat":
			return BigRatType{}
		case "string":
			return StringType{}
		case "bool":
			return BoolType{}
		default:
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
			if ut, ok := env.FindUnionByVariant(*t.Simple); ok {
				return ut
			}
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			return AnyType{}
		}
	}

	return AnyType{}
}

func resolveTypeName(name string, env *Env) Type {
	return resolveTypeRef(&parser.TypeRef{Simple: &name}, env)
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
		op := part.Op
		if part.Op == "union" && part.All {
			op = "union_all"
		}
		operators = append(operators, token{part.Pos, op})
	}

	for _, level := range [][]string{
		{"*", "/", "%"},
		{"+", "-"},
		{"<", "<=", ">", ">="},
		{"==", "!=", "in"},
		{"&&"},
		{"||"},
		{"union", "union_all", "except", "intersect"},
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
	if _, ok := left.(AnyType); ok {
		return AnyType{}, nil
	}
	if _, ok := right.(AnyType); ok {
		return AnyType{}, nil
	}
	if op == "+" || op == "union" || op == "union_all" || op == "except" || op == "intersect" {
		if llist, ok := left.(ListType); ok {
			if rlist, ok := right.(ListType); ok {
				if !unify(llist.Elem, rlist.Elem, nil) {
					return nil, errOperatorMismatch(pos, op, left, right)
				}
				return ListType{Elem: llist.Elem}, nil
			}
		}
	}
	switch op {
	case "+", "-", "*", "/", "%":
		switch {
		case isNumeric(left) && isNumeric(right):
			if op == "/" && unify(left, IntType{}, nil) && unify(right, IntType{}, nil) {
				return FloatType{}, nil
			}
			if unify(left, FloatType{}, nil) || unify(right, FloatType{}, nil) {
				return FloatType{}, nil
			}
			if unify(left, Int64Type{}, nil) || unify(right, Int64Type{}, nil) {
				return Int64Type{}, nil
			}
			return IntType{}, nil
		case op == "+" && (unify(left, StringType{}, nil) || unify(right, StringType{}, nil)):
			return StringType{}, nil
		default:
			return nil, errOperatorMismatch(pos, op, left, right)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		if !unify(left, right, nil) {
			if isNumeric(left) && isNumeric(right) {
				return BoolType{}, nil
			}
			return nil, errIncompatibleComparison(pos)
		}
		return BoolType{}, nil
	case "in":
		switch rt := right.(type) {
		case MapType:
			if !unify(left, rt.Key, nil) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		case ListType:
			if !unify(left, rt.Elem, nil) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		default:
			if !(unify(left, StringType{}, nil) && unify(right, StringType{}, nil)) {
				return nil, errOperatorMismatch(pos, op, left, right)
			}
			return BoolType{}, nil
		}
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

func checkUnary(u *parser.Unary, env *Env, expected Type) (Type, error) {
	return checkPostfix(u.Value, env, expected)
}

func checkPostfix(p *parser.PostfixExpr, env *Env, expected Type) (Type, error) {
	typ, err := checkPrimary(p.Target, env, expected)
	if err != nil {
		return nil, err
	}

	for _, op := range p.Ops {
		if idx := op.Index; idx != nil {
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

			case StringType:
				if idx.Start == nil && idx.Colon == nil {
					return nil, errMissingIndex(idx.Pos)
				}
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
				typ = StringType{}

			default:
				return nil, errNotIndexable(p.Target.Pos, typ)
			}
		} else if call := op.Call; call != nil {
			ft, ok := typ.(FuncType)
			if !ok {
				if _, isAny := typ.(AnyType); isAny {
					// dynamic call, assume any
					for _, arg := range call.Args {
						if _, err := checkExpr(arg, env); err != nil {
							return nil, err
						}
					}
					typ = AnyType{}
					continue
				}
				return nil, errNotFunction(call.Pos, "")
			}
			argCount := len(call.Args)
			paramCount := len(ft.Params)
			if argCount > paramCount {
				return nil, errTooManyArgs(call.Pos, paramCount, argCount)
			}
			for i := 0; i < argCount; i++ {
				at, err := checkExprWithExpected(call.Args[i], env, ft.Params[i])
				if err != nil {
					return nil, err
				}
				if !unify(at, ft.Params[i], nil) {
					return nil, errArgTypeMismatch(call.Pos, i, ft.Params[i], at)
				}
			}
			if argCount == paramCount {
				typ = ft.Return
			} else {
				typ = curryFuncType(ft.Params[argCount:], ft.Return)
			}
		} else if cast := op.Cast; cast != nil {
			typ = resolveTypeRef(cast.Type, env)
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
		case p.Lit.Null:
			return AnyType{}, nil
		}

	case p.Selector != nil:
		typ, err := env.GetVar(p.Selector.Root)
		if err != nil {
			return nil, errUnknownVariable(p.Pos, p.Selector.Root)
		}
		prefix := p.Selector.Root
		for _, field := range p.Selector.Tail {
			if t, err := env.GetVar(prefix + "." + field); err == nil {
				typ = t
				prefix = prefix + "." + field
				continue
			}
			switch t := typ.(type) {
			case StructType:
				if ft, ok := t.Fields[field]; ok {
					typ = ft
					continue
				}
				if m, ok := t.Methods[field]; ok {
					typ = m.Type
					continue
				}
				return nil, errUnknownField(p.Pos, field, t)
			case GroupType:
				if field == "key" {
					typ = t.Key
					continue
				}
				if field == "items" {
					typ = ListType{Elem: t.Elem}
					continue
				}
				typ = AnyType{}
				continue
			case StringType:
				if field == "contains" {
					typ = FuncType{Params: []Type{StringType{}}, Return: BoolType{}}
					continue
				}
				return nil, errNotStruct(p.Pos, typ)
			case MapType:
				if field == "keys" {
					typ = FuncType{Params: []Type{}, Return: ListType{Elem: AnyType{}}, Pure: true}
					continue
				}
				if unify(t.Key, StringType{}, nil) {
					typ = t.Value
					continue
				}
				return nil, errNotStruct(p.Pos, typ)
			case AnyType:
				typ = AnyType{}
				continue
			default:
				return nil, errNotStruct(p.Pos, typ)
			}
		}
		return typ, nil

	case p.Call != nil:
		fnType, err := env.GetVar(p.Call.Func)
		if err != nil {
			return nil, errUnknownFunction(p.Pos, p.Call.Func)
		}

		ft, ok := fnType.(FuncType)
		if !ok {
			return nil, errNotFunction(p.Pos, p.Call.Func)
		}
		argCount := len(p.Call.Args)
		paramCount := len(ft.Params)

		if exp, ok := builtinArity[p.Call.Func]; ok && !ft.Variadic {
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if argCount != exp {
					return nil, errArgCount(p.Pos, p.Call.Func, exp, argCount)
				}
			}
		}

		if !ft.Variadic && argCount > paramCount {
			return nil, errTooManyArgs(p.Pos, paramCount, argCount)
		}

		// check fixed parameters
		fixed := paramCount
		if ft.Variadic && paramCount > 0 {
			fixed = paramCount - 1
		}

		argTypes := make([]Type, argCount)
		for i := 0; i < argCount && i < fixed; i++ {
			at, err := checkExprWithExpected(p.Call.Args[i], env, ft.Params[i])
			if err != nil {
				return nil, err
			}
			argTypes[i] = at
			if !unify(at, ft.Params[i], nil) {
				return nil, errArgTypeMismatch(p.Pos, i, ft.Params[i], at)
			}
		}

		var variadicType Type
		if ft.Variadic {
			if paramCount == 0 {
				variadicType = AnyType{}
			} else {
				variadicType = ft.Params[paramCount-1]
			}
			for i := fixed; i < argCount; i++ {
				at, err := checkExprWithExpected(p.Call.Args[i], env, variadicType)
				if err != nil {
					return nil, err
				}
				argTypes[i] = at
				if !unify(at, variadicType, nil) {
					return nil, errArgTypeMismatch(p.Pos, i, variadicType, at)
				}
			}
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
					return nil, err
				}
			}
			return ft.Return, nil
		}
		if _, defined := env.GetFunc(p.Call.Func); !defined {
			if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
				return nil, err
			}
		}
		if argCount == paramCount {
			return ft.Return, nil
		}
		return curryFuncType(ft.Params[argCount:], ft.Return), nil

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
			var kt Type
			if _, ok := stringKey(item.Key); ok {
				kt = StringType{}
			} else {
				var err error
				kt, err = checkExpr(item.Key, env)
				if err != nil {
					return nil, err
				}
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

	case p.Query != nil:
		return checkQueryExpr(p.Query, env, expected)

	case p.LogicQuery != nil:
		return ListType{Elem: MapType{Key: StringType{}, Value: AnyType{}}}, nil

	case p.Fetch != nil:
		urlT, err := checkExpr(p.Fetch.URL, env)
		if err != nil {
			return nil, err
		}
		if !unify(urlT, StringType{}, nil) {
			return nil, errFetchURLString(p.Pos)
		}
		if p.Fetch.With != nil {
			wt, err := checkExpr(p.Fetch.With, env)
			if err != nil {
				return nil, err
			}
			mt, ok := wt.(MapType)
			if !ok || !unify(mt.Key, StringType{}, nil) {
				return nil, errFetchOptsMap(p.Pos)
			}
			if withMl := p.Fetch.With.Binary.Left.Value.Target.Map; withMl != nil {
				for _, item := range withMl.Items {
					if key, ok := stringKey(item.Key); ok {
						var expect Type
						switch key {
						case "method":
							expect = StringType{}
						case "headers":
							expect = MapType{Key: StringType{}, Value: StringType{}}
						case "body":
							expect = nil
						case "query":
							expect = MapType{Key: StringType{}, Value: StringType{}}
						case "timeout":
							expect = FloatType{}
						default:
							expect = nil
						}
						if expect != nil {
							vt, err := checkExpr(item.Value, env)
							if err != nil {
								return nil, err
							}
							if !unify(vt, expect, nil) {
								return nil, errFetchOptType(item.Value.Pos, key, expect, vt)
							}
						} else {
							if _, err := checkExpr(item.Value, env); err != nil {
								return nil, err
							}
						}
					} else {
						if _, err := checkExpr(item.Value, env); err != nil {
							return nil, err
						}
					}
				}
			}
		}
		if expected != nil {
			return expected, nil
		}
		return AnyType{}, nil

	case p.Load != nil:
		var elem Type = AnyType{}
		if p.Load.Type != nil {
			elem = resolveTypeRef(p.Load.Type, env)
		}
		return ListType{Elem: elem}, nil

	case p.Save != nil:
		if _, err := checkExpr(p.Save.Src, env); err != nil {
			return VoidType{}, err
		}
		if p.Save.With != nil {
			if _, err := checkExpr(p.Save.With, env); err != nil {
				return VoidType{}, err
			}
		}
		return VoidType{}, nil

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

	case p.If != nil:
		return checkIfExpr(p.If, env, expected)
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

func checkIfExpr(ie *parser.IfExpr, env *Env, expected Type) (Type, error) {
	condT, err := checkExpr(ie.Cond, env)
	if err != nil {
		return nil, err
	}
	if !unify(condT, BoolType{}, nil) {
		return nil, errIfCondBoolean(ie.Cond.Pos)
	}

	thenT, err := checkExprWithExpected(ie.Then, env, expected)
	if err != nil {
		return nil, err
	}

	var elseT Type = AnyType{}
	if ie.ElseIf != nil {
		elseT, err = checkIfExpr(ie.ElseIf, env, thenT)
		if err != nil {
			return nil, err
		}
	} else if ie.Else != nil {
		elseT, err = checkExprWithExpected(ie.Else, env, thenT)
		if err != nil {
			return nil, err
		}
	}

	result := thenT
	if !unify(result, elseT, nil) {
		result = AnyType{}
	}
	if expected != nil && !unify(result, expected, nil) {
		return nil, errTypeMismatch(ie.Pos, expected, result)
	}
	return result, nil
}

func checkMatchExpr(m *parser.MatchExpr, env *Env, expected Type) (Type, error) {
	targetType, err := checkExpr(m.Target, env)
	if err != nil {
		return nil, err
	}
	var resultType Type
	for _, c := range m.Cases {
		caseEnv := env
		if call, ok := callPattern(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				if len(call.Args) != len(st.Order) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				if !unify(targetType, st, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				child := NewEnv(env)
				for idx, arg := range call.Args {
					if name, ok := identName(arg); ok {
						child.SetVar(name, st.Fields[st.Order[idx]], true)
					}
				}
				caseEnv = child
			}
		} else if ident, ok := identName(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(ident); ok {
				st := ut.Variants[ident]
				if !unify(targetType, st, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
			} else if !isUnderscoreExpr(c.Pattern) {
				pType, err := checkExpr(c.Pattern, env)
				if err != nil {
					return nil, err
				}
				if !unify(targetType, pType, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, pType)
				}
			}
		} else if !isUnderscoreExpr(c.Pattern) {
			pType, err := checkExpr(c.Pattern, env)
			if err != nil {
				return nil, err
			}
			if !unify(targetType, pType, nil) {
				return nil, errTypeMismatch(c.Pos, targetType, pType)
			}
		}

		rType, err := checkExprWithExpected(c.Result, caseEnv, expected)
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

func checkQueryExpr(q *parser.QueryExpr, env *Env, expected Type) (Type, error) {
	srcT, err := checkExpr(q.Source, env)
	if err != nil {
		return nil, err
	}
	var elemT Type
	switch t := srcT.(type) {
	case ListType:
		elemT = t.Elem
	case GroupType:
		elemT = t.Elem
	default:
		return nil, errQuerySourceList(q.Pos)
	}
	child := NewEnv(env)
	child.SetVar(q.Var, elemT, true)

	for _, f := range q.Froms {
		ft, err := checkExpr(f.Src, child)
		if err != nil {
			return nil, err
		}
		var fe Type
		switch t := ft.(type) {
		case ListType:
			fe = t.Elem
		case GroupType:
			fe = t.Elem
		default:
			return nil, errJoinSourceList(f.Pos)
		}
		child.SetVar(f.Var, fe, true)
	}

	for _, j := range q.Joins {
		jt, err := checkExpr(j.Src, child)
		if err != nil {
			return nil, err
		}
		var je Type
		switch t := jt.(type) {
		case ListType:
			je = t.Elem
		case GroupType:
			je = t.Elem
		default:
			return nil, errJoinSourceList(j.Pos)
		}
		child.SetVar(j.Var, je, true)
		onT, err := checkExpr(j.On, child)
		if err != nil {
			return nil, err
		}
		if !unify(onT, BoolType{}, nil) {
			return nil, errJoinOnBoolean(j.On.Pos)
		}
	}

	if q.Where != nil {
		wt, err := checkExprWithExpected(q.Where, child, BoolType{})
		if err != nil {
			return nil, err
		}
		if !unify(wt, BoolType{}, nil) {
			return nil, errWhereBoolean(q.Where.Pos)
		}
	}

	var selT Type
	if q.Group != nil {
		keyT, err := checkExpr(q.Group.Exprs[0], child)
		if err != nil {
			return nil, err
		}
		genv := NewEnv(child)
		gStruct := GroupType{Key: keyT, Elem: elemT}
		genv.SetVar(q.Group.Name, gStruct, true)
		if q.Group.Having != nil {
			ht, err := checkExprWithExpected(q.Group.Having, genv, BoolType{})
			if err != nil {
				return nil, err
			}
			if !unify(ht, BoolType{}, nil) {
				return nil, errHavingBoolean(q.Group.Having.Pos)
			}
		}
		selT, err = checkExpr(q.Select, genv)
	} else {
		if name, arg, ok := aggregateCallName(q.Select); ok {
			at, err := checkExpr(arg, child)
			if err != nil {
				return nil, err
			}
			switch name {
			case "sum", "avg":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, errSumOperand(q.Select.Pos, at)
				}
				selT = FloatType{}
			case "min", "max":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, fmt.Errorf("min/max() expects numeric expression")
				}
				selT = at
			case "count":
				selT = IntType{}
			}
		} else {
			selT, err = checkExpr(q.Select, child)
			if err != nil {
				return nil, err
			}
			result := ListType{Elem: selT}
			if expected != nil && !unify(result, expected, nil) {
				return nil, errTypeMismatch(q.Pos, expected, result)
			}
			return result, nil
		}
	}
	if err != nil {
		return nil, err
	}
	if _, _, ok := aggregateCallName(q.Select); ok {
		if expected != nil && !unify(selT, expected, nil) {
			return nil, errTypeMismatch(q.Pos, expected, selT)
		}
		return selT, nil
	}
	result := ListType{Elem: selT}
	if expected != nil && !unify(result, expected, nil) {
		return nil, errTypeMismatch(q.Pos, expected, result)
	}
	return result, nil
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

func identName(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	return "", false
}

func stringKey(e *parser.Expr) (string, bool) {
	if e == nil {
		return "", false
	}
	if len(e.Binary.Right) != 0 {
		return "", false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", false
	}
	p := u.Value
	if len(p.Ops) != 0 {
		return "", false
	}
	if p.Target.Selector != nil && len(p.Target.Selector.Tail) == 0 {
		return p.Target.Selector.Root, true
	}
	if p.Target.Lit != nil && p.Target.Lit.Str != nil {
		return *p.Target.Lit.Str, true
	}
	return "", false
}

func aggregateCallName(e *parser.Expr) (string, *parser.Expr, bool) {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return "", nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return "", nil, false
	}
	p := u.Value
	if p == nil || len(p.Ops) != 0 || p.Target == nil || p.Target.Call == nil {
		return "", nil, false
	}
	call := p.Target.Call
	if len(call.Args) != 1 {
		return "", nil, false
	}
	if isQueryExpr(call.Args[0]) {
		return "", nil, false
	}
	switch call.Func {
	case "sum", "avg", "min", "max", "count":
		return call.Func, call.Args[0], true
	default:
		return "", nil, false
	}
}

func isQueryExpr(e *parser.Expr) bool {
	for e != nil {
		if e.Binary == nil || len(e.Binary.Right) != 0 {
			return false
		}
		u := e.Binary.Left
		if len(u.Ops) != 0 {
			return false
		}
		p := u.Value
		if p == nil || len(p.Ops) != 0 {
			return false
		}
		prim := p.Target
		if prim == nil {
			return false
		}
		if prim.Query != nil || prim.LogicQuery != nil {
			return true
		}
		if prim.Group != nil {
			e = prim.Group
			continue
		}
		return false
	}
	return false
}

func isNumeric(t Type) bool {
	switch t.(type) {
	case IntType, Int64Type, FloatType, BigIntType, BigRatType:
		return true
	default:
		return false
	}
}

var builtinArity = map[string]int{
	"now":       0,
	"input":     0,
	"json":      1,
	"to_json":   1,
	"str":       1,
	"upper":     1,
	"lower":     1,
	"reverse":   1,
	"distinct":  1,
	"trim":      1,
	"contains":  2,
	"split":     2,
	"join":      2,
	"eval":      1,
	"len":       1,
	"count":     1,
	"exists":    1,
	"avg":       1,
	"abs":       1,
	"ceil":      1,
	"floor":     1,
	"sum":       1,
	"min":       1,
	"max":       1,
	"keys":      1,
	"values":    1,
	"reduce":    3,
	"append":    2,
	"push":      2,
	"first":     1,
	"substring": 3,
	"num":       1,
	"denom":     1,
}

func checkBuiltinCall(name string, args []Type, pos lexer.Position) error {
	switch name {
	case "now", "input":
		if len(args) != 0 {
			return errArgCount(pos, name, 0, len(args))
		}
		return nil
	case "json", "to_json", "str", "upper", "lower", "eval":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if name == "eval" {
			if _, ok := args[0].(StringType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		}
		return nil
	case "len":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, MapType, StringType, AnyType:
			return nil
		default:
			return errLenOperand(pos, args[0])
		}
	case "count":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, GroupType, AnyType:
			return nil
		default:
			return errCountOperand(pos, args[0])
		}
	case "exists":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, MapType, StringType, AnyType, GroupType:
			return nil
		default:
			return fmt.Errorf("exists expects list, map or string")
		}
	case "avg":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errAvgOperand(pos, a.Elem)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errAvgOperand(pos, a.Elem)
		case AnyType:
			return nil
		default:
			return errAvgOperand(pos, a)
		}
	case "sum":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errSumOperand(pos, a.Elem)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return errSumOperand(pos, a.Elem)
		case AnyType:
			return nil
		default:
			return errSumOperand(pos, a)
		}
	case "abs", "ceil", "floor":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if !isNumeric(args[0]) {
			return fmt.Errorf("%s() expects numeric", name)
		}
		return nil
	case "min", "max":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch a := args[0].(type) {
		case ListType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return fmt.Errorf("%s() expects numeric list", name)
		case GroupType:
			if _, ok := a.Elem.(AnyType); ok || isNumeric(a.Elem) {
				return nil
			}
			if _, ok := a.Elem.(StringType); ok {
				return nil
			}
			if _, ok := a.Elem.(BoolType); ok {
				return nil
			}
			return fmt.Errorf("%s() expects numeric list", name)
		case AnyType:
			return nil
		default:
			return fmt.Errorf("%s() expects list", name)
		}
	case "keys", "values":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case MapType, AnyType:
			return nil
		default:
			return fmt.Errorf("%s() expects map", name)
		}
	case "reduce":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		// first argument should be list
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("reduce() expects list, got %v", args[0])
			}
		}
		return nil
	case "concat":
		for _, a := range args {
			if _, ok := a.(ListType); !ok {
				if _, ok := a.(AnyType); !ok {
					return fmt.Errorf("concat() expects list, got %v", a)
				}
			}
		}
		return nil
	case "append", "push":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("%s() expects list, got %v", name, args[0])
			}
		}
		return nil
	case "range":
		if len(args) < 1 || len(args) > 3 {
			return errArgCount(pos, name, 1, len(args))
		}
		for i, a := range args {
			if _, ok := a.(IntType); !ok {
				if _, ok := a.(AnyType); !ok {
					return errArgTypeMismatch(pos, i, IntType{}, a)
				}
			}
		}
		return nil
	case "first":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if _, ok := args[0].(ListType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("first() expects list, got %v", args[0])
			}
		}
		return nil
	case "reverse":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, StringType, AnyType:
			return nil
		default:
			return fmt.Errorf("reverse() expects list or string, got %v", args[0])
		}
	case "distinct":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, AnyType:
			return nil
		default:
			return fmt.Errorf("distinct() expects list, got %v", args[0])
		}
	case "substring":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("substring() expects string, got %v", args[0])
			}
		}
		for i := 1; i < 3; i++ {
			if _, ok := args[i].(IntType); !ok {
				if _, ok := args[i].(AnyType); !ok {
					return errArgTypeMismatch(pos, i, IntType{}, args[i])
				}
			}
		}
		return nil
	case "num", "denom":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		if !isNumeric(args[0]) {
			return fmt.Errorf("%s() expects numeric", name)
		}
		return nil
	}
	return nil
}

func callPattern(e *parser.Expr) (*parser.CallExpr, bool) {
	if e == nil {
		return nil, false
	}
	if len(e.Binary.Right) != 0 {
		return nil, false
	}
	u := e.Binary.Left
	if len(u.Ops) != 0 {
		return nil, false
	}
	p := u.Value
	if len(p.Ops) != 0 || p.Target.Call == nil {
		return nil, false
	}
	return p.Target.Call, true
}
