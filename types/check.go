package types

import (
	"fmt"
	"strings"

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

type UnitType struct{}

func (UnitType) String() string { return "unit" }

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

type OptionType struct {
	Elem Type
}

func (t OptionType) String() string { return fmt.Sprintf("option[%s]", t.Elem.String()) }

type GroupType struct {
	Key  Type
	Elem Type
}

func (GroupType) String() string { return "group" }

// StructField is a single declared field of a StructType. The slice of
// fields on StructType is ordered by declaration sequence so JSON
// encoding and pretty-printing stay deterministic without a parallel
// `Order` slice (MEP 4 P10).
type StructField struct {
	Name string
	Type Type
}

// StructType is the type of a nominal record. Fields are stored as a
// single ordered slice that doubles as the iteration order and the
// lookup table (via helper methods). The previous representation kept
// `Fields map[string]Type` plus `Order []string` in parallel, which
// could drift; MEP 4 P10 consolidates them.
type StructType struct {
	Name    string
	Fields  []StructField
	Methods map[string]Method
}

type Method struct {
	Decl *parser.FunStmt
	Type FuncType
}

func (t StructType) String() string { return t.Name }

// FieldType returns the declared type of the named field, or nil and
// false if the field is not present.
func (t StructType) FieldType(name string) (Type, bool) {
	for _, f := range t.Fields {
		if f.Name == name {
			return f.Type, true
		}
	}
	return nil, false
}

// HasField reports whether the named field is declared on the struct.
func (t StructType) HasField(name string) bool {
	_, ok := t.FieldType(name)
	return ok
}

// FieldNames returns the field names in declaration order.
func (t StructType) FieldNames() []string {
	names := make([]string, len(t.Fields))
	for i, f := range t.Fields {
		names[i] = f.Name
	}
	return names
}

// FieldMap returns a fresh lookup map of field name to declared type.
// Callers that perform many lookups against the same struct should
// cache the result.
func (t StructType) FieldMap() map[string]Type {
	m := make(map[string]Type, len(t.Fields))
	for _, f := range t.Fields {
		m[f.Name] = f.Type
	}
	return m
}

// WithField returns a copy of t with the named field's type set to
// ftype. If the field is already declared the type is updated in
// place (preserving order); otherwise the field is appended.
func (t StructType) WithField(name string, ftype Type) StructType {
	fields := make([]StructField, len(t.Fields))
	copy(fields, t.Fields)
	for i, f := range fields {
		if f.Name == name {
			fields[i].Type = ftype
			out := t
			out.Fields = fields
			return out
		}
	}
	out := t
	out.Fields = append(fields, StructField{Name: name, Type: ftype})
	return out
}

// NewStructType is a convenience constructor for an ordered field set
// passed as alternating (name, type) pairs.
func NewStructType(name string, fields ...StructField) StructType {
	out := StructType{Name: name}
	out.Fields = append([]StructField(nil), fields...)
	return out
}

type UnionType struct {
	Name string
	// Variants stores the per-variant struct type keyed by variant name.
	// Order preserves the declaration order of variants and is the
	// canonical iteration sequence (MEP 4 P11). Variants is kept in
	// sync but is not authoritative for ordering.
	Variants map[string]StructType
	Order    []string
}

func (t UnionType) String() string { return t.Name }

type AnyType struct{}

func (AnyType) String() string { return "any" }

// TypeVar is the polymorphism kind used to represent a generic type
// parameter. Its methods take a pointer receiver because identity, not
// value, is what distinguishes two type variables: a fresh `*TypeVar`
// with the same `Name` as an existing one must compare unequal under
// unification. Every other kind in this package is a value type with
// value receivers. If a future kind needs the same identity semantics
// (a `RowVar` for row polymorphism is the obvious candidate, planned
// under MEP 11), it should follow the same convention so callers can
// rely on a single discriminator (`x.(*TypeVar)` vs `x.(SomeValueKind)`).
// See MEP 4 §6 problem 14 and MEP 12.
type TypeVar struct {
	Name string
}

func (t *TypeVar) String() string { return t.Name }

// FuncType is the type of a function value. Params is the fixed prefix
// of parameter types. Variadic is the element type of the trailing
// varargs sequence, or nil if the function is not variadic (MEP 4 P13).
// Pure is the inferred purity flag (MEP 4 P7).
type FuncType struct {
	Params   []Type
	Return   Type
	Pure     bool
	Variadic Type
	// TypeParams lists the names of TypeVars quantified at this
	// signature (MEP-12). A non-empty value means the function is
	// generic: the call site freshens these names via Instantiate before
	// unifying arguments. Non-generic functions leave the field nil.
	TypeParams []string
}

func (f FuncType) String() string {
	s := "fun("
	for i, p := range f.Params {
		if i > 0 {
			s += ", "
		}
		s += p.String()
	}
	if f.Variadic != nil {
		if len(f.Params) > 0 {
			s += ", ..." + f.Variadic.String()
		} else {
			s += "..." + f.Variadic.String()
		}
	}
	s += ")"
	if f.Return != nil && f.Return.String() != "unit" {
		s += ": " + f.Return.String()
	}
	if f.Pure {
		s += " [pure]"
	}
	return s
}

type Subst map[string]Type

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

	case OptionType:
		switch bt := b.(type) {
		case OptionType:
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
			for _, f := range at.Fields {
				bv, ok := bt.FieldType(f.Name)
				if !ok {
					return false
				}
				if !unify(f.Type, bv, subst) {
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
			return at.Name == bt.Name
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
		if !ok || len(at.Params) != len(bt.Params) {
			return false
		}
		if (at.Variadic == nil) != (bt.Variadic == nil) {
			return false
		}
		for i := range at.Params {
			if !unify(at.Params[i], bt.Params[i], subst) {
				return false
			}
		}
		if at.Variadic != nil && !unify(at.Variadic, bt.Variadic, subst) {
			return false
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

	case UnitType:
		_, ok := b.(UnitType)
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
		case OptionType:
			if ot, ok := a.(OptionType); ok {
				return unify(ot.Elem, bt.Elem, subst)
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
		Params:   []Type{},
		Return:   UnitType{},
		Variadic: AnyType{},
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
	// concat<T>(...xs: list<T>): list<T> - MEP-12.4. Every argument is
	// a list<T>; the variadic unifier in checkPrimary pins T from the
	// first argument and rejects any later argument whose element type
	// disagrees with T047.
	concatT := &TypeVar{Name: "T"}
	env.SetVar("concat", FuncType{
		Params:     []Type{},
		Return:     ListType{Elem: concatT},
		Pure:       true,
		Variadic:   ListType{Elem: concatT},
		TypeParams: []string{"T"},
	}, false)
	// first<T>(xs: list<T>): T - MEP-12.4. Generic so the result of
	// first(list<int>) is int rather than any, and the user no longer
	// needs an `as T` cast at the consumer site.
	firstT := &TypeVar{Name: "T"}
	env.SetVar("first", FuncType{
		Params:     []Type{ListType{Elem: firstT}},
		Return:     firstT,
		Pure:       true,
		TypeParams: []string{"T"},
	}, false)
	env.SetVar("reverse", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
		Pure:   true,
	}, false)
	// distinct<T>(xs: list<T>): list<T> - MEP-12.4. Shape-preserving;
	// the result list element type matches the input.
	distinctT := &TypeVar{Name: "T"}
	env.SetVar("distinct", FuncType{
		Params:     []Type{ListType{Elem: distinctT}},
		Return:     ListType{Elem: distinctT},
		Pure:       true,
		TypeParams: []string{"T"},
	}, false)
	env.SetVar("push", FuncType{
		Params: []Type{ListType{Elem: AnyType{}}, AnyType{}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	// keys<K,V>(m: map<K,V>): list<K> - MEP-12.4. Existing call-site
	// post-processing in checkPrimary already specialises the return
	// from the inferred map type; this declaration carries the same
	// shape through the call-site instantiator as well.
	keysK := &TypeVar{Name: "K"}
	keysV := &TypeVar{Name: "V"}
	env.SetVar("keys", FuncType{
		Params:     []Type{MapType{Key: keysK, Value: keysV}},
		Return:     ListType{Elem: keysK},
		Pure:       true,
		TypeParams: []string{"K", "V"},
	}, false)
	valuesK := &TypeVar{Name: "K"}
	valuesV := &TypeVar{Name: "V"}
	env.SetVar("values", FuncType{
		Params:     []Type{MapType{Key: valuesK, Value: valuesV}},
		Return:     ListType{Elem: valuesV},
		Pure:       true,
		TypeParams: []string{"K", "V"},
	}, false)
	env.SetVar("collect", FuncType{
		Params: []Type{AnyType{}},
		Return: ListType{Elem: AnyType{}},
		Pure:   true,
	}, false)
	env.SetVar("range", FuncType{
		Params:   []Type{},
		Return:   ListType{Elem: IntType{}},
		Pure:     true,
		Variadic: IntType{},
	}, false)
	env.SetVar("now", FuncType{
		Params: []Type{},
		Return: Int64Type{},
	}, false)
	env.SetVar("json", FuncType{
		Params: []Type{AnyType{}},
		Return: UnitType{},
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
	env.SetVar("parseIntStr", FuncType{
		Params: []Type{StringType{}, IntType{}},
		Return: IntType{},
		Pure:   true,
	}, false)
	env.SetVar("int", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
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
	env.SetVar("padStart", FuncType{
		Params: []Type{StringType{}, IntType{}, StringType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("substr", FuncType{
		Params: []Type{StringType{}, IntType{}, IntType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("indexOf", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: IntType{},
		Pure:   true,
	}, false)
	env.SetVar("repeat", FuncType{
		Params: []Type{StringType{}, IntType{}},
		Return: StringType{},
		Pure:   true,
	}, false)
	env.SetVar("sha256", FuncType{
		Params: []Type{AnyType{}},
		Return: ListType{Elem: IntType{}},
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
		Return: IntType{},
		Pure:   true,
	}, false)
	// min/max are generic over the list element type (MEP-12.4):
	//     min<T>(xs: list<T>): T
	//     max<T>(xs: list<T>): T
	// The TypeVar names match the TypeParams entry so the call-site
	// Instantiate freshens them. The legacy unifier still admits an
	// `any` argument (a `list<any>` carries `AnyType` element) without
	// constraining T, so the return falls back to the fresh variable.
	minT := &TypeVar{Name: "T"}
	env.SetVar("min", FuncType{
		Params:     []Type{ListType{Elem: minT}},
		Return:     minT,
		Pure:       true,
		TypeParams: []string{"T"},
	}, false)
	maxT := &TypeVar{Name: "T"}
	env.SetVar("max", FuncType{
		Params:     []Type{ListType{Elem: maxT}},
		Return:     maxT,
		Pure:       true,
		TypeParams: []string{"T"},
	}, false)
	env.SetVar("round", FuncType{
		Params: []Type{FloatType{}, IntType{}},
		Return: FloatType{},
		Pure:   true,
	}, false)
	// reduce is generic over the list element type and the accumulator
	// type (MEP-12.4):
	//     reduce<A, B>(xs: list<A>, fn: fun(B, A): B, init: B): B
	reduceA := &TypeVar{Name: "A"}
	reduceB := &TypeVar{Name: "B"}
	env.SetVar("reduce", FuncType{
		Params: []Type{
			ListType{Elem: reduceA},
			FuncType{Params: []Type{reduceB, reduceA}, Return: reduceB},
			reduceB,
		},
		Return:     reduceB,
		Pure:       true,
		TypeParams: []string{"A", "B"},
	}, false)
	env.SetVar("eval", FuncType{
		Params: []Type{StringType{}},
		Return: AnyType{},
	}, false)

	var errs []error

	// Pre-pass: register struct and union name stubs so the function pass
	// below can resolve forward references in parameter and return types.
	// The dedicated type-declaration pass that follows replaces each stub
	// with the fully populated shape.
	for _, stmt := range prog.Statements {
		if stmt.Type == nil {
			continue
		}
		if len(stmt.Type.Members) > 0 {
			stub := StructType{Name: stmt.Type.Name}
			env.SetStruct(stmt.Type.Name, stub)
			env.types[stmt.Type.Name] = stub
			continue
		}
		if len(stmt.Type.Variants) > 0 {
			variants := map[string]StructType{}
			for _, v := range stmt.Type.Variants {
				vs := StructType{Name: v.Name}
				variants[v.Name] = vs
				env.SetStruct(v.Name, vs)
			}
			stub := UnionType{Name: stmt.Type.Name, Variants: variants}
			env.SetUnion(stmt.Type.Name, stub)
			env.types[stmt.Type.Name] = stub
		}
		// Type aliases are handled during the dedicated pass; the right
		// hand side may itself be a forward reference.
	}

	// First pass: gather all function declarations so methods defined in types
	// can reference them regardless of order in the source file.
	for _, stmt := range prog.Statements {
		if stmt.Fun != nil {
			sigEnv := env
			if len(stmt.Fun.TypeParams) > 0 {
				sigEnv = NewEnv(env)
				for _, tp := range stmt.Fun.TypeParams {
					sigEnv.SetTypeParam(tp, &TypeVar{Name: tp})
				}
			}
			params := make([]Type, len(stmt.Fun.Params))
			for i, p := range stmt.Fun.Params {
				if p.Type != nil {
					params[i] = resolveTypeRef(p.Type, sigEnv)
				} else {
					params[i] = AnyType{}
				}
			}
			var ret Type = UnitType{}
			if stmt.Fun.Return != nil {
				ret = resolveTypeRef(stmt.Fun.Return, sigEnv)
			}
			pure := isPureFunction(stmt.Fun, env)
			env.SetVar(stmt.Fun.Name, FuncType{
				Params:     params,
				Return:     ret,
				Pure:       pure,
				TypeParams: append([]string(nil), stmt.Fun.TypeParams...),
			}, false)
			env.SetFunc(stmt.Fun.Name, stmt.Fun)
		}
	}

	// Second pass: process type declarations now that all functions are known.
	for _, stmt := range prog.Statements {
		if stmt.Type != nil {
			if err := checkStmt(stmt, env, UnitType{}, false); err != nil {
				errs = append(errs, err)
			}
		}
	}

	// Final pass: check remaining statements, including function bodies.
	for _, stmt := range prog.Statements {
		if stmt.Type == nil {
			if err := checkStmt(stmt, env, UnitType{}, false); err != nil {
				errs = append(errs, err)
			}
		}
	}
	// Flush diagnostics raised in resolveTypeRef and other contexts
	// that cannot return errors directly to a caller.
	errs = append(errs, env.TakeDiagnostics()...)
	return errs
}

// --- Helpers ---

func buildStreamFields(fields []*parser.StreamField, env *Env) []StructField {
	out := make([]StructField, 0, len(fields))
	for _, f := range fields {
		if f == nil {
			continue
		}
		out = append(out, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
	}
	return out
}

func checkStmt(s *parser.Statement, env *Env, expectedReturn Type, inLoop bool) error {
	switch {
	case s.Stream != nil:
		fields := buildStreamFields(s.Stream.Fields, env)
		st := StructType{Name: s.Stream.Name, Fields: fields}
		env.SetStream(s.Stream.Name, st)
		env.SetStruct(s.Stream.Name, st)
		env.types[s.Stream.Name] = st
		return nil

	case s.Agent != nil:
		var fields []StructField
		methods := map[string]Method{}
		for _, blk := range s.Agent.Body {
			switch {
			case blk.Let != nil:
				var t Type = AnyType{}
				if blk.Let.Type != nil {
					t = resolveTypeRef(blk.Let.Type, env)
				}
				fields = append(fields, StructField{Name: blk.Let.Name, Type: t})
			case blk.Var != nil:
				var t Type = AnyType{}
				if blk.Var.Type != nil {
					t = resolveTypeRef(blk.Var.Type, env)
				}
				fields = append(fields, StructField{Name: blk.Var.Name, Type: t})
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
		st := StructType{Name: s.Agent.Name, Fields: fields, Methods: methods}
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
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
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
			ft, ok := st.FieldType(f.Name)
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
				// MEP-11.2: route the let-binding check through
				// Assignable rather than unify. Direction matters:
				// the value flows into the declared slot, so we ask
				// `Assignable(rhs, declared)` not the symmetric form.
				if !Assignable(exprType, typ) {
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
		// MEP-10 A3: reject aliasing an immutable aggregate into a
		// mutable binding. `let xs: list<int>; var ys = xs` would share
		// storage; a later `ys[0] = ...` would mutate xs too. Clone
		// explicitly or declare the source as `var`.
		if src := bareIdentName(s.Var.Value); src != "" {
			if mut, err := env.IsMutable(src); err == nil && !mut {
				if srcT, err := env.GetVar(src); err == nil && isAliasableAggregate(srcT) {
					return errAliasImmutableAggregate(s.Var.Pos, src)
				}
			}
		}
		var typ Type
		if s.Var.Type != nil {
			typ = resolveTypeRef(s.Var.Type, env)
			if s.Var.Value != nil {
				exprType, err := checkExprWithExpected(s.Var.Value, env, typ)
				if err != nil {
					return err
				}
				// MEP-11.2: route the var-binding check through
				// Assignable; see the matching change on Let above.
				if !Assignable(exprType, typ) {
					return errTypeMismatch(s.Var.Pos, typ, exprType)
				}
				// MEP-10 B3 / B3c: when the source expression names
				// live aggregate storage (bare identifier, an index
				// chain like `rows[0]`, or a field chain like
				// `obj.f`), the new binding shares storage with the
				// source. A widened element type lets a later
				// `ys[i] = ...` deposit a value the source's static
				// type rejects, corrupting reads through the source.
				// Aliasing requires structural equality on aggregate
				// element, key, and value types.
				if src := aliasSourceLabel(s.Var.Value); src != "" {
					if isAliasableAggregate(exprType) && isAliasableAggregate(typ) && !equalKinds(exprType, typ) {
						return errAliasWidensElement(s.Var.Pos, src, exprType, typ)
					}
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
				case AnyType:
					lhsType = AnyType{}
				default:
					if IsAnyType(lhsType) {
						// Allow indexing on dynamic values in
						// assignments, propagating `any`.
						lhsType = AnyType{}
					} else {
						return errNotIndexable(s.Assign.Pos, lhsType)
					}
				}
			}
		}
		if len(s.Assign.Field) > 0 {
			for _, fop := range s.Assign.Field {
				field := fop.Name
				switch lt := lhsType.(type) {
				case StructType:
					ft, ok := lt.FieldType(field)
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
		rhsType, err := checkExprWithExpected(s.Assign.Value, env, lhsType)
		if err != nil {
			return err
		}
		if !unify(lhsType, rhsType, nil) {
			return errCannotAssign(s.Assign.Pos, rhsType, s.Assign.Name, lhsType)
		}
		// MEP-10 B3d: when the LHS targets a slot reached through an
		// index or field (`bag[i]`, `r.items`) and the RHS names live
		// aggregate storage, the assignment aliases the RHS into a slot
		// with a widened element type. A later write through
		// `bag[i][j]` would deposit a value the RHS's static type
		// rejects, corrupting reads through the RHS name. Require
		// structural equality at the slot type. Fresh-value RHS
		// (literals, calls, computed values) keep working since
		// aliasSourceLabel only fires on live-storage paths.
		if len(s.Assign.Index) > 0 || len(s.Assign.Field) > 0 {
			if src := aliasSourceLabel(s.Assign.Value); src != "" {
				if isAliasableAggregate(rhsType) && isAliasableAggregate(lhsType) && !equalKinds(rhsType, lhsType) {
					return errAliasWidensElement(s.Assign.Pos, src, rhsType, lhsType)
				}
			}
		}
		if len(s.Assign.Index) == 0 && len(s.Assign.Field) == 0 {
			if ContainsAny(rhsType) {
				if _, ok := lhsType.(AnyType); ok {
					env.SetVar(s.Assign.Name, rhsType, true)
				}
			} else {
				env.SetVar(s.Assign.Name, rhsType, true)
			}
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
		for _, f := range st.Fields {
			child.SetVar(f.Name, f.Type, true)
		}
		for _, item := range s.Update.Set.Items {
			if key, ok := stringKey(item.Key); ok {
				ft, ok2 := st.FieldType(key)
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
			// MEP-5 §Inference for control flow [T-ForList, T-ForMap, T-ForStr]:
			// any other shape (including bare `any`) is T022.
			switch t := sourceType.(type) {
			case ListType:
				elemType = t.Elem
			case MapType:
				elemType = t.Key // loop iterates over keys
			case StringType:
				elemType = StringType{}
			default:
				return errCannotIterate(s.For.Pos, sourceType)
			}
		}

		// Create new scope for the loop variable
		child := NewEnv(env)
		child.SetVar(s.For.Name, elemType, true)

		// Check loop body
		for _, stmt := range s.For.Body {
			if err := checkStmt(stmt, child, expectedReturn, true); err != nil {
				return err
			}
		}
		return nil

	case s.Type != nil:
		if s.Type.Alias != nil {
			t := resolveTypeRef(s.Type.Alias, env)
			env.types[s.Type.Name] = t
			return nil
		}
		if len(s.Type.Members) > 0 {
			var fields []StructField
			methods := map[string]Method{}
			st := StructType{Name: s.Type.Name, Fields: fields, Methods: methods}
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			// First pass: collect fields
			for _, m := range s.Type.Members {
				if m.Field != nil {
					fields = append(fields, StructField{Name: m.Field.Name, Type: resolveTypeRef(m.Field.Type, env)})
				}
			}
			// Precompute method types so they can reference each other.
			for _, m := range s.Type.Members {
				if m.Method != nil {
					params := make([]Type, len(m.Method.Params))
					for i, p := range m.Method.Params {
						if p.Type == nil {
							return errParamMissingType(m.Method.Pos, p.Name)
						}
						params[i] = resolveTypeRef(p.Type, env)
					}
					var ret Type = UnitType{}
					if m.Method.Return != nil {
						ret = resolveTypeRef(m.Method.Return, env)
					}
					methods[m.Method.Name] = Method{Decl: m.Method, Type: FuncType{Params: params, Return: ret}}
				}
			}
			// Second pass: check methods using the computed types.
			for _, m := range s.Type.Members {
				if m.Method != nil {
					meth := methods[m.Method.Name]
					params := meth.Type.Params
					ret := meth.Type.Return

					methodEnv := NewEnv(env)
					for _, f := range fields {
						methodEnv.SetVar(f.Name, f.Type, true)
					}
					for name, mt := range methods {
						methodEnv.SetVar(name, mt.Type, true)
					}
					for i, p := range m.Method.Params {
						methodEnv.SetVar(p.Name, params[i], true)
					}
					for _, stmt := range m.Method.Body {
						if err := checkStmt(stmt, methodEnv, ret, false); err != nil {
							return err
						}
					}
					pure := isPureFunction(&parser.FunStmt{Params: m.Method.Params, Return: m.Method.Return, Body: m.Method.Body}, methodEnv)
					methods[m.Method.Name] = Method{Decl: m.Method, Type: FuncType{Params: params, Return: ret, Pure: pure}}
				}
			}
			st.Fields = fields
			st.Methods = methods
			env.SetStruct(s.Type.Name, st)
			env.types[s.Type.Name] = st
			return nil
		}
		if len(s.Type.Variants) > 0 {
			// Build the union with a shared variants map so recursive
			// references resolve correctly as variants are populated.
			variants := map[string]StructType{}
			variantOrder := make([]string, 0, len(s.Type.Variants))
			ut := UnionType{Name: s.Type.Name, Variants: variants, Order: variantOrder}
			env.SetUnion(s.Type.Name, ut)
			env.types[s.Type.Name] = ut

			for _, v := range s.Type.Variants {
				var vf []StructField
				for _, f := range v.Fields {
					vf = append(vf, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
				}
				st := StructType{Name: v.Name, Fields: vf}
				variants[v.Name] = st
				variantOrder = append(variantOrder, v.Name)
				env.SetStruct(v.Name, st)
				params := make([]Type, 0, len(v.Fields))
				for _, f := range v.Fields {
					params = append(params, resolveTypeRef(f.Type, env))
				}
				env.SetFuncType(v.Name, FuncType{Params: params, Return: UnionType{Name: s.Type.Name, Variants: nil}})
				if len(params) == 0 {
					env.SetVar(v.Name, UnionType{Name: s.Type.Name, Variants: nil}, false)
				}
			}
			ut.Order = variantOrder
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
		sigEnv := env
		if len(s.Fun.TypeParams) > 0 {
			sigEnv = NewEnv(env)
			for _, tp := range s.Fun.TypeParams {
				sigEnv.SetTypeParam(tp, &TypeVar{Name: tp})
			}
		}
		params := []Type{}
		for _, p := range s.Fun.Params {
			if p.Type == nil {
				// Permit functions without explicit parameter
				// annotations by defaulting them to `any`.
				params = append(params, AnyType{})
			} else {
				params = append(params, resolveTypeRef(p.Type, sigEnv))
			}
		}
		var ret Type = AnyType{}
		if s.Fun.Return != nil {
			ret = resolveTypeRef(s.Fun.Return, sigEnv)
		}
		pure := isPureFunction(s.Fun, env)
		env.SetVar(name, FuncType{
			Params:     params,
			Return:     ret,
			Pure:       pure,
			TypeParams: append([]string(nil), s.Fun.TypeParams...),
		}, false)
		env.SetFunc(name, s.Fun)

		child := NewEnv(sigEnv)
		for i, p := range s.Fun.Params {
			child.SetVar(p.Name, params[i], true)
		}
		for _, stmt := range s.Fun.Body {
			if err := checkStmt(stmt, child, ret, false); err != nil {
				return err
			}
		}
		return nil

	case s.Expr != nil:
		_, err := checkExprWithExpected(s.Expr.Expr, env, nil)
		return err

	case s.Return != nil:
		if s.Return.Value == nil {
			if !unify(UnitType{}, expectedReturn, nil) {
				return errReturnMismatch(s.Return.Pos, expectedReturn, UnitType{})
			}
			return nil
		}
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
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
				return err
			}
		}
		return nil

	case s.Bench != nil:
		child := NewEnv(env)
		for _, stmt := range s.Bench.Body {
			if err := checkStmt(stmt, child, expectedReturn, false); err != nil {
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

	case s.If != nil:
		return checkIfStmt(s.If, env, expectedReturn, inLoop)

	case s.While != nil:
		condT, err := checkExprWithExpected(s.While.Cond, env, nil)
		if err != nil {
			return err
		}
		if !unify(condT, BoolType{}, nil) {
			return errIfCondBoolean(s.While.Cond.Pos)
		}
		child := NewEnv(env)
		for _, stmt := range s.While.Body {
			if err := checkStmt(stmt, child, expectedReturn, true); err != nil {
				return err
			}
		}
		return nil

	case s.Break != nil:
		if !inLoop {
			return errBreakContinueOutsideLoop(s.Break.Pos, "break")
		}
		return nil

	case s.Continue != nil:
		if !inLoop {
			return errBreakContinueOutsideLoop(s.Continue.Pos, "continue")
		}
		return nil
	}
	return nil
}

func checkIfStmt(stmt *parser.IfStmt, env *Env, expectedReturn Type, inLoop bool) error {
	condT, err := checkExprWithExpected(stmt.Cond, env, nil)
	if err != nil {
		return err
	}
	if !unify(condT, BoolType{}, nil) {
		return errIfCondBoolean(stmt.Cond.Pos)
	}
	child := NewEnv(env)
	for _, s := range stmt.Then {
		if err := checkStmt(s, child, expectedReturn, inLoop); err != nil {
			return err
		}
	}
	if stmt.ElseIf != nil {
		return checkIfStmt(stmt.ElseIf, env, expectedReturn, inLoop)
	}
	elseChild := NewEnv(env)
	for _, s := range stmt.Else {
		if err := checkStmt(s, elseChild, expectedReturn, inLoop); err != nil {
			return err
		}
	}
	return nil
}

func resolveTypeRef(t *parser.TypeRef, env *Env) Type {
	typ := resolveTypeRefInner(t, env)
	if t.Optional {
		// MEP-10 C1: `T?` desugars to `option[T]`.
		typ = OptionType{Elem: typ}
	}
	return typ
}

func resolveTypeRefInner(t *parser.TypeRef, env *Env) Type {
	if t.Fun != nil {
		params := make([]Type, len(t.Fun.Params))
		for i, p := range t.Fun.Params {
			params[i] = resolveTypeRef(p, env)
		}
		var ret Type = UnitType{}
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
		var fields []StructField
		for _, f := range t.Struct.Fields {
			fields = append(fields, StructField{Name: f.Name, Type: resolveTypeRef(f.Type, env)})
		}
		return StructType{Name: "", Fields: fields}
	}

	if t.Simple != nil {
		switch *t.Simple {
		case "int":
			return IntType{}
		case "int64":
			return Int64Type{}
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
		case "unit":
			return UnitType{}
		case "any":
			return AnyType{}
		default:
			if tv, ok := env.LookupTypeParam(*t.Simple); ok {
				return tv
			}
			if ut, ok := env.GetUnion(*t.Simple); ok {
				return ut
			}
			if st, ok := env.GetStruct(*t.Simple); ok {
				return st
			}
			if typ, ok := env.LookupType(*t.Simple); ok {
				return typ
			}
			if ut, ok := env.FindUnionByVariant(*t.Simple); ok {
				return ut
			}
			if isTypeParamName(*t.Simple) {
				return AnyType{}
			}
			env.RecordDiagnostic(errUnknownType(t.Pos, *t.Simple))
			return AnyType{}
		}
	}

	return AnyType{}
}

// isTypeParamName recognises single uppercase letters used as generic
// type-parameter names (`T`, `K`, `V`). MEP 12 will plumb a real type-
// parameter scope through resolveTypeRef; until then this heuristic
// keeps the unknown-type-name diagnostic from firing on names that are
// already valid in generic positions.
func isTypeParamName(name string) bool {
	if len(name) != 1 {
		return false
	}
	c := name[0]
	return c >= 'A' && c <= 'Z'
}

func resolveTypeName(name string, env *Env) Type {
	return resolveTypeRef(&parser.TypeRef{Simple: &name}, env)
}

func checkExpr(e *parser.Expr, env *Env) (Type, error) {
	return checkExprWithExpected(e, env, nil)
}

func checkExprWithExpected(e *parser.Expr, env *Env, expected Type) (Type, error) {
	actual, err := checkBinaryExpr(e.Binary, env, expected)
	if err != nil {
		return nil, err
	}
	if expected != nil && !unify(actual, expected, nil) {
		return nil, errTypeMismatch(e.Pos, expected, actual)
	}
	return actual, nil
}
func checkBinaryExpr(b *parser.BinaryExpr, env *Env, expected Type) (Type, error) {
	left, err := checkUnary(b.Left, env, expected)
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
		typ, err := checkUnary(part.Right, env, nil)
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
	if IsAnyType(left) || IsAnyType(right) {
		if op == "+" && (unify(left, StringType{}, nil) || unify(right, StringType{}, nil)) {
			return StringType{}, nil
		}
		switch op {
		case "+", "-", "*", "/", "%":
			return AnyType{}, nil
		case "<", "<=", ">", ">=", "==", "!=", "&&", "||", "in":
			return BoolType{}, nil
		default:
			return AnyType{}, nil
		}
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
			// MEP-10 B2: numeric mix is the lattice join. The same
			// helper drives inferBinaryType so the inferrer and the
			// checker agree on the result type kind. Without this,
			// inferring `int + int = int` while the checker reports
			// `bigint` produced spurious "expected int, got bigint"
			// errors at let-binding sites once MEP-11.2 routed those
			// through Subtype.
			if joined, ok := numericJoin(left, right); ok {
				return joined, nil
			}
			return IntType{}, nil
		case op == "+" && (unify(left, StringType{}, nil) || unify(right, StringType{}, nil)):
			return StringType{}, nil
		default:
			return nil, errOperatorMismatch(pos, op, left, right)
		}
	case "==", "!=", "<", "<=", ">", ">=":
		// MEP 4 P9. The `any` short-circuit at the top of the function
		// already handles the soundness escape hatch. Here, `unify` is
		// used (not `equalTypes`) because empty-list literals carry
		// element type `any`, and comparing `xs == []` is a common
		// surface idiom that must continue to type-check.
		if unify(left, right, nil) {
			return BoolType{}, nil
		}
		if isNumeric(left) && isNumeric(right) {
			return BoolType{}, nil
		}
		return nil, errIncompatibleComparison(pos)
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
	exp := expected
	if len(p.Ops) > 0 {
		exp = nil
	}
	typ, err := checkPrimary(p.Target, env, exp)
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
				typ = OptionType{Elem: t.Value}

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

			case AnyType:
				typ = AnyType{}
			default:
				if IsAnyType(typ) {
					// Allow dynamic indexing on values of unknown
					// type, propagating the `any` type forward.
					typ = AnyType{}
				} else {
					return nil, errNotIndexable(p.Target.Pos, typ)
				}
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
			target := resolveTypeRef(cast.Type, env)
			if !castOk(typ, target) {
				return nil, errInvalidCast(op.Pos, typ, target)
			}
			typ = target
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
			// MEP-10 A2: `null` is the lone value of `option[any]`. It
			// unifies with any other option type via the top-level any
			// case in `unify`, and against `any` itself via the top
			// short-circuit. A non-option target like `int` no longer
			// accepts `null` without an explicit option-typed slot.
			return OptionType{Elem: AnyType{}}, nil
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
				if ft, ok := t.FieldType(field); ok {
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
				if field == "padStart" {
					typ = FuncType{Params: []Type{IntType{}, StringType{}}, Return: StringType{}, Pure: true}
					continue
				}
				return nil, errNotStruct(p.Pos, typ)
			case MapType:
				switch field {
				case "keys":
					typ = FuncType{Params: []Type{}, Return: ListType{Elem: AnyType{}}, Pure: true}
					continue
				case "get":
					typ = FuncType{Params: []Type{t.Key, t.Value}, Return: t.Value, Pure: true}
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
		// MEP-12.2: instantiate TypeParams with fresh vars so distinct
		// call sites of the same generic function get distinct vars. The
		// returned `callSubst` accumulates per-arg bindings; it is
		// applied to the return type after all args are typed so the
		// caller sees a concrete result (e.g. `min(list<int>) → int`).
		callSubst := Subst{}
		if len(ft.TypeParams) > 0 {
			inst, sub := Instantiate(ft, ft.TypeParams)
			ft = inst.(FuncType)
			for k, v := range sub {
				callSubst[k] = v
			}
		}
		argCount := len(p.Call.Args)
		paramCount := len(ft.Params)

		if exp, ok := builtinArity[p.Call.Func]; ok && ft.Variadic == nil {
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if argCount != exp {
					return nil, errArgCount(p.Pos, p.Call.Func, exp, argCount)
				}
			}
		}

		if ft.Variadic == nil && argCount > paramCount {
			return nil, errTooManyArgs(p.Pos, paramCount, argCount)
		}

		// Params is the fixed prefix; ft.Variadic (if non-nil) types the
		// trailing varargs sequence (MEP 4 P13).
		fixed := paramCount

		argTypes := make([]Type, argCount)
		for i := 0; i < argCount && i < fixed; i++ {
			expected := callSubst.Apply(ft.Params[i])
			// MEP-12.2/12.3: when the original parameter mentions any
			// TypeVar quantified by this signature, never propagate the
			// hint. Two cases:
			//   (1) the var is still unbound: Subtype has no TypeVar
			//       rule, so the hint-driven check would mishandle it.
			//   (2) the var was bound by an earlier argument: forcing
			//       the bound type as a hint would surface a T008 in
			//       place of the more informative T047 unify-conflict
			//       diagnostic the call-site Unify produces below.
			hint := expected
			if len(FreeTypeVars(ft.Params[i], Subst{})) > 0 {
				hint = nil
			}
			at, err := checkExprWithExpected(p.Call.Args[i], env, hint)
			if err != nil {
				return nil, err
			}
			argTypes[i] = at
			if next, err := Unify(at, expected, callSubst); err == nil {
				callSubst = next
			} else if !unify(at, expected, nil) {
				// MEP-12.3: when a generic call fails to unify, surface
				// T047 with the offending type parameter rather than the
				// generic T007 mismatch. The legacy fallback above keeps
				// non-generic calls on T007.
				if len(ft.TypeParams) > 0 {
					// Use the structural TypeVar name from the
					// pre-substitution param so we can report the
					// original declared name (e.g. `T`) even after
					// Instantiate freshened it to `T#1`.
					if name := structuralTypeVarName(ft.Params[i]); name != "" {
						return nil, errTypeParamConflict(p.Pos, name, expected, at)
					}
				}
				return nil, errArgTypeMismatch(p.Pos, i, expected, at)
			}
			// MEP-10 B3b / B3c: when the argument names live
			// aggregate storage (bare ident, `rows[i]`, `obj.f`)
			// and the parameter slot is the same aggregate kind
			// with a widened element / key / value type, the
			// callee can deposit a value the source's static type
			// rejects through the alias. Require structural
			// equality. The check is narrow: it skips
			// `any`-typed parameters (the design-loose interop
			// position) because no structural write through the
			// parameter is reachable without an explicit `as` cast,
			// which the runtime guard from MEP-11.7 checks at the
			// boundary.
			if src := aliasSourceLabel(p.Call.Args[i]); src != "" {
				paramFinal := callSubst.Apply(ft.Params[i])
				if isAliasableAggregate(at) && isAliasableAggregate(paramFinal) && !equalKinds(at, paramFinal) {
					return nil, errAliasWidensElement(p.Pos, src, at, paramFinal)
				}
			}
		}

		if ft.Variadic != nil {
			variadicType := ft.Variadic
			variadicIsGeneric := len(FreeTypeVars(ft.Variadic, Subst{})) > 0
			for i := fixed; i < argCount; i++ {
				expected := callSubst.Apply(variadicType)
				hint := expected
				if variadicIsGeneric {
					hint = nil
				}
				at, err := checkExprWithExpected(p.Call.Args[i], env, hint)
				if err != nil {
					return nil, err
				}
				argTypes[i] = at
				if next, err := Unify(at, expected, callSubst); err == nil {
					callSubst = next
				} else if !unify(at, expected, nil) {
					if len(ft.TypeParams) > 0 {
						if name := structuralTypeVarName(variadicType); name != "" {
							return nil, errTypeParamConflict(p.Pos, name, expected, at)
						}
					}
					return nil, errArgTypeMismatch(p.Pos, i, expected, at)
				}
			}
			if _, defined := env.GetFunc(p.Call.Func); !defined {
				if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
					return nil, err
				}
			}
			ret := callSubst.Apply(ft.Return)
			if p.Call.Func == "keys" && len(argTypes) == 1 {
				if mt, ok := argTypes[0].(MapType); ok {
					ret = ListType{Elem: mt.Key}
				}
			}
			if p.Call.Func == "values" && len(argTypes) == 1 {
				if mt, ok := argTypes[0].(MapType); ok {
					ret = ListType{Elem: mt.Value}
				}
			}
			// MEP-12.4: reverse mirrors its argument's static shape.
			// The declared signature stays loose (any -> any) so the
			// list-or-string discriminator in checkBuiltinCall still
			// applies; the post-process here pins the return type at
			// the call site, so reverse([1,2,3]) types as list<int>
			// rather than any.
			if p.Call.Func == "reverse" && len(argTypes) == 1 {
				switch at := argTypes[0].(type) {
				case ListType:
					ret = at
				case StringType:
					ret = StringType{}
				}
			}
			return ret, nil
		}
		if _, defined := env.GetFunc(p.Call.Func); !defined {
			if err := checkBuiltinCall(p.Call.Func, argTypes, p.Pos); err != nil {
				return nil, err
			}
		}
		ret := callSubst.Apply(ft.Return)
		if p.Call.Func == "keys" && len(argTypes) == 1 {
			if mt, ok := argTypes[0].(MapType); ok {
				ret = ListType{Elem: mt.Key}
			}
		}
		if p.Call.Func == "values" && len(argTypes) == 1 {
			if mt, ok := argTypes[0].(MapType); ok {
				ret = ListType{Elem: mt.Value}
			}
		}
		// MEP-12.4: reverse mirrors its argument's static shape.
		if p.Call.Func == "reverse" && len(argTypes) == 1 {
			switch at := argTypes[0].(type) {
			case ListType:
				ret = at
			case StringType:
				ret = StringType{}
			}
		}
		// MEP-12.3: T048 when the declared generic result still mentions
		// an unbound type parameter after argument unification. Only
		// fire on full applications; partial application still carries
		// the unbound var through the curried result.
		if len(ft.TypeParams) > 0 && argCount == paramCount {
			if name := firstFreeTypeVar(ret, callSubst); name != "" {
				return nil, errTypeParamEscapes(p.Pos, name)
			}
		}
		if argCount == paramCount {
			return ret, nil
		}
		return curryFuncType(ft.Params[argCount:], ret), nil

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
			ft, ok := st.FieldType(field.Name)
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
		// MEP-5 §Collections [T-List]: every element must unify with the
		// principal element type. The prior rule widened to AnyType when
		// elements disagreed, masking the heterogeneity at type-check
		// time. We now reject with T008.
		var elemType Type = nil
		for _, elem := range p.List.Elems {
			t, err := checkExpr(elem, env)
			if err != nil {
				return nil, err
			}
			if elemType == nil {
				elemType = t
				continue
			}
			if !unify(elemType, t, nil) {
				return nil, errTypeMismatch(elem.Pos, elemType, t)
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
			return UnitType{}, err
		}
		if p.Save.With != nil {
			if _, err := checkExpr(p.Save.With, env); err != nil {
				return UnitType{}, err
			}
		}
		return UnitType{}, nil

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
			// Default missing parameter types to `any` rather than
			// failing. This allows algorithms that omit explicit
			// parameter annotations to type-check, falling back to
			// dynamic behaviour.
			paramTypes[i] = AnyType{}
		} else {
			paramTypes[i] = resolveTypeRef(p.Type, env)
		}
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
			if err := checkStmt(stmt, child, declaredRet, false); err != nil {
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

// bareIdentName returns the source identifier if e is a single
// identifier reference with no operators, indices, or selectors. It
// drives the MEP-10 A3 alias-into-var check; only a bare reference
// can share storage with the source binding.
func bareIdentName(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	px := u.Value
	if len(px.Ops) != 0 || px.Target == nil {
		return ""
	}
	sel := px.Target.Selector
	if sel == nil || len(sel.Tail) != 0 {
		return ""
	}
	return sel.Root
}

// aliasSourceLabel returns a short human-readable name for the
// aliasable source expression e: a bare identifier, an index chain
// (`rows[0]`), or a field chain (`obj.f`). It returns "" when the
// expression cannot name live storage (literals, calls, casts,
// computed values). MEP-10 B3c uses this to widen B3 beyond bare
// identifiers so index/field reads of aliasable aggregates also
// reject element-widening alias targets.
func aliasSourceLabel(e *parser.Expr) string {
	if e == nil || e.Binary == nil || len(e.Binary.Right) != 0 {
		return ""
	}
	u := e.Binary.Left
	if u == nil || len(u.Ops) != 0 || u.Value == nil {
		return ""
	}
	px := u.Value
	if px.Target == nil {
		return ""
	}
	sel := px.Target.Selector
	if sel == nil {
		return ""
	}
	label := sel.Root
	for _, t := range sel.Tail {
		label += "." + t
	}
	for _, op := range px.Ops {
		switch {
		case op.Field != nil:
			label += "." + op.Field.Name
		case op.Index != nil:
			label += "[...]"
		default:
			return ""
		}
	}
	return label
}

// firstFreeTypeVar returns the lexicographically first free TypeVar
// name in t after applying sub, or "" if none. It powers the T047 and
// T048 messages so the user sees the offending parameter rather than
// the substitution-internal label.
func firstFreeTypeVar(t Type, sub Subst) string {
	free := FreeTypeVars(t, sub)
	if len(free) == 0 {
		return ""
	}
	name := free[0]
	if i := strings.IndexByte(name, '#'); i >= 0 {
		return name[:i]
	}
	return name
}

// structuralTypeVarName returns the first TypeVar's declared name found
// in t, ignoring substitutions. The "T#N" suffix introduced by
// FreshTypeVar is stripped so the caller surfaces the user-declared
// parameter name (e.g. "T") in diagnostics.
func structuralTypeVarName(t Type) string {
	free := FreeTypeVars(t, Subst{})
	if len(free) == 0 {
		return ""
	}
	name := free[0]
	if i := strings.IndexByte(name, '#'); i >= 0 {
		return name[:i]
	}
	return name
}

// isAliasableAggregate reports whether t is a value kind whose storage
// is shared on copy (list, map, struct). These are the kinds where an
// alias from an immutable binding into a mutable one would let writes
// through the alias mutate the original.
func isAliasableAggregate(t Type) bool {
	switch t.(type) {
	case ListType, MapType, StructType:
		return true
	}
	return false
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
	// MEP-10 A4: track variant coverage when the scrutinee is a union.
	// A wildcard `_` (or an identifier binding that does not name a
	// variant) covers the remainder; otherwise every variant must be
	// matched explicitly.
	matchedVariants := map[string]bool{}
	hasCatchAll := false
	for _, c := range m.Cases {
		caseEnv := env
		if call, ok := callPattern(c.Pattern); ok {
			if ut, ok := env.FindUnionByVariant(call.Func); ok {
				st := ut.Variants[call.Func]
				if len(call.Args) != len(st.Fields) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				if !unify(targetType, st, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, st)
				}
				matchedVariants[call.Func] = true
				child := NewEnv(env)
				for idx, arg := range call.Args {
					if name, ok := identName(arg); ok {
						child.SetVar(name, st.Fields[idx].Type, true)
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
				matchedVariants[ident] = true
			} else if !isUnderscoreExpr(c.Pattern) {
				pType, err := checkExpr(c.Pattern, env)
				if err != nil {
					return nil, err
				}
				if !unify(targetType, pType, nil) {
					return nil, errTypeMismatch(c.Pos, targetType, pType)
				}
			} else {
				hasCatchAll = true
			}
		} else if !isUnderscoreExpr(c.Pattern) {
			pType, err := checkExpr(c.Pattern, env)
			if err != nil {
				return nil, err
			}
			if !unify(targetType, pType, nil) {
				return nil, errTypeMismatch(c.Pos, targetType, pType)
			}
		} else {
			hasCatchAll = true
		}
		if c.Result == nil {
			for _, st := range c.Block {
				if err := checkStmt(st, caseEnv, expected, false); err != nil {
					return nil, err
				}
			}
			continue
		}

		rType, err := checkExprWithExpected(c.Result, caseEnv, expected)
		if err != nil {
			return nil, err
		}
		if resultType == nil {
			resultType = rType
			continue
		}
		// MEP-5 §Match [T-Match]: every arm's result type must unify with
		// the principal type. Heterogeneous arms are T008; the prior rule
		// silently widened to AnyType.
		if !unify(resultType, rType, nil) {
			return nil, errTypeMismatch(c.Pos, resultType, rType)
		}
	}
	// MEP-10 A4: when the scrutinee is a union, every variant must be
	// covered by an explicit arm or a wildcard `_` arm. The check runs
	// after all arms have been typed so the error pinpoints the match
	// site rather than an arbitrary arm.
	if ut, ok := targetType.(UnionType); ok && !hasCatchAll {
		var missing []string
		for _, name := range ut.Order {
			if !matchedVariants[name] {
				missing = append(missing, name)
			}
		}
		if len(missing) > 0 {
			return nil, errMatchNonExhaustive(m.Pos, ut.Name, missing)
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
		if name, pos, ok := firstImpureCall(q.Where, child); ok {
			return nil, errImpurePredicate(pos, name, "where")
		}
	}

	var selT Type
	if q.Group != nil {
		var keyT Type
		if len(q.Group.Exprs) == 1 {
			var err error
			keyT, err = checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
		} else if len(q.Group.Exprs) == 2 {
			k1, err := checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
			k2, err := checkExpr(q.Group.Exprs[1], child)
			if err != nil {
				return nil, err
			}
			if _, ok := k1.(StringType); ok {
				if _, ok2 := k2.(StringType); ok2 {
					keyT = StructType{Name: "pair_string", Fields: []StructField{{Name: "a", Type: StringType{}}, {Name: "b", Type: StringType{}}}}
				}
			}
			if keyT == nil {
				keyT = AnyType{}
			}
		} else {
			var err error
			keyT, err = checkExpr(q.Group.Exprs[0], child)
			if err != nil {
				return nil, err
			}
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
			if name, pos, ok := firstImpureCall(q.Group.Having, genv); ok {
				return nil, errImpurePredicate(pos, name, "having")
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
			case "sum":
				if _, ok := at.(AnyType); !ok && !isNumeric(at) {
					return nil, errSumOperand(q.Select.Pos, at)
				}
				// Preserve the element type per MEP-5 P2; widening to int/float
				// lost precision for bigint/bigrat/int64 summands.
				selT = at
			case "avg":
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

// castOk reports whether an `e as T` expression should be accepted by
// the type checker. The policy is deliberately narrow: every accepted
// cast must have a well-defined runtime semantics. String parsing and
// arbitrary cross-kind casts are rejected; use a parsing builtin
// (`parseIntStr`, `int`, `str`, ...) for those.
func castOk(from, to Type) bool {
	if equalTypes(from, to) {
		return true
	}
	if _, ok := from.(AnyType); ok {
		return true
	}
	if _, ok := to.(AnyType); ok {
		return true
	}
	if isNumeric(from) && isNumeric(to) {
		return true
	}
	if u, ok := from.(UnionType); ok {
		if s, ok := to.(StructType); ok {
			if _, in := u.Variants[s.Name]; in {
				return true
			}
		}
	}
	if _, ok := from.(MapType); ok {
		if _, ok := to.(StructType); ok {
			return true
		}
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
	"now":         0,
	"input":       0,
	"json":        1,
	"to_json":     1,
	"str":         1,
	"parseIntStr": 2,
	"int":         1,
	"upper":       1,
	"lower":       1,
	"reverse":     1,
	"distinct":    1,
	"trim":        1,
	"contains":    2,
	"split":       2,
	"join":        2,
	"eval":        1,
	"len":         1,
	"count":       1,
	"exists":      1,
	"avg":         1,
	"abs":         1,
	"ceil":        1,
	"floor":       1,
	"sum":         1,
	"min":         1,
	"max":         1,
	"keys":        1,
	"values":      1,
	"reduce":      3,
	"append":      2,
	"push":        2,
	"first":       1,
	"substring":   3,
	"padStart":    3,
	"indexOf":     2,
	"repeat":      2,
	"sha256":      1,
	"num":         1,
	"denom":       1,
}

func checkBuiltinCall(name string, args []Type, pos lexer.Position) error {
	switch name {
	case "now", "input":
		if len(args) != 0 {
			return errArgCount(pos, name, 0, len(args))
		}
		return nil
	case "json", "to_json", "str", "upper", "lower", "int", "eval":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch name {
		case "eval":
			if _, ok := args[0].(StringType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		case "int":
			switch args[0].(type) {
			case StringType, IntType, FloatType, AnyType:
				// ok
			default:
				return fmt.Errorf("int() expects numeric or string")
			}
		}
		return nil
	case "parseIntStr":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
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
	case "collect":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case ListType, GroupType, AnyType:
			return nil
		default:
			return fmt.Errorf("collect() expects list or group")
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
			switch args[i].(type) {
			case IntType, AnyType:
				// ok
			case BigIntType:
				// allow bigint indices, treat as int
				args[i] = IntType{}
			default:
				return errArgTypeMismatch(pos, i, IntType{}, args[i])
			}
		}
		return nil
	case "padStart":
		if len(args) != 3 {
			return errArgCount(pos, name, 3, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return fmt.Errorf("padStart() expects string, got %v", args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
		}
		if _, ok := args[2].(StringType); !ok {
			if _, ok := args[2].(AnyType); !ok {
				return errArgTypeMismatch(pos, 2, StringType{}, args[2])
			}
		}
		return nil
	case "indexOf":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		for i := 0; i < 2; i++ {
			if _, ok := args[i].(StringType); !ok {
				if _, ok := args[i].(AnyType); !ok {
					return errArgTypeMismatch(pos, i, StringType{}, args[i])
				}
			}
		}
		return nil
	case "repeat":
		if len(args) != 2 {
			return errArgCount(pos, name, 2, len(args))
		}
		if _, ok := args[0].(StringType); !ok {
			if _, ok := args[0].(AnyType); !ok {
				return errArgTypeMismatch(pos, 0, StringType{}, args[0])
			}
		}
		switch args[1].(type) {
		case IntType, AnyType:
			// ok
		case BigIntType:
			args[1] = IntType{}
		default:
			return errArgTypeMismatch(pos, 1, IntType{}, args[1])
		}
		return nil
	case "sha256":
		if len(args) != 1 {
			return errArgCount(pos, name, 1, len(args))
		}
		switch args[0].(type) {
		case StringType, ListType, AnyType:
			return nil
		default:
			return fmt.Errorf("sha256 expects string or list")
		}
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
