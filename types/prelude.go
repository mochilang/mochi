package types

// This file is the single registration point for every builtin name the
// type checker exposes to source programs. Each group below is a small,
// independently-toggleable bundle of names. Adding or removing a name
// changes the user-visible surface of the language; do it here, not in
// check.go.
//
// The grouping is for readability and for letting embedders disable a
// bundle. registerPrelude is the unconditional entry point used by the
// regular Check pass.

// registerPrelude installs the full default builtin surface on env. It
// is called once at the top of Check(). Embedders that want a reduced
// surface can call the per-group functions directly instead.
func registerPrelude(env *Env) {
	registerErrorTypes(env)
	registerCollectionBuiltins(env)
	registerStringBuiltins(env)
	registerNumericBuiltins(env)
	registerTimeBuiltins(env)
	registerMetaBuiltins(env)
	registerIOBuiltins(env)
}

// registerErrorTypes installs the prelude's closed union error kinds.
// Today the only one is IOError, which is the error arm of the
// `result<…, IOError>` returns produced by load / save / fetch.
// Variants are kept narrow and named after the actual failure modes
// that the runtime can distinguish, so a match has at most five arms.
func registerErrorTypes(env *Env) {
	notFound := StructType{Name: "NotFound", Fields: []StructField{
		{Name: "path", Type: StringType{}},
	}}
	permDenied := StructType{Name: "PermissionDenied", Fields: []StructField{
		{Name: "path", Type: StringType{}},
	}}
	invalidFmt := StructType{Name: "InvalidFormat", Fields: []StructField{
		{Name: "path", Type: StringType{}},
		{Name: "reason", Type: StringType{}},
	}}
	netErr := StructType{Name: "NetworkError", Fields: []StructField{
		{Name: "url", Type: StringType{}},
		{Name: "reason", Type: StringType{}},
	}}
	ioFailure := StructType{Name: "IOFailure", Fields: []StructField{
		{Name: "reason", Type: StringType{}},
	}}
	for _, st := range []StructType{notFound, permDenied, invalidFmt, netErr, ioFailure} {
		env.SetStruct(st.Name, st)
	}
	ioError := UnionType{
		Name: "IOError",
		Variants: map[string]StructType{
			"NotFound":         notFound,
			"PermissionDenied": permDenied,
			"InvalidFormat":    invalidFmt,
			"NetworkError":     netErr,
			"IOFailure":        ioFailure,
		},
		Order: []string{"NotFound", "PermissionDenied", "InvalidFormat", "NetworkError", "IOFailure"},
	}
	env.SetUnion("IOError", ioError)
}

// registerCollectionBuiltins covers list, map, and group operations.
// Most of these are generic (MEP-12.4). The TypeVar names here match
// the TypeParams entry so the call-site Instantiate freshens them.
func registerCollectionBuiltins(env *Env) {
	env.SetVar("len", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
	}, false)
	// append<T>(xs: list<T>, x: T): list<T> - MEP-12.4. The element type
	// is pinned by the first argument; the second must unify with it,
	// so push of a string into a list<int> now fails T047 at the call
	// site instead of widening the result to list<any>.
	appendT := &TypeVar{Name: "T"}
	env.SetVar("append", FuncType{
		Params:     []Type{ListType{Elem: appendT}, appendT},
		Return:     ListType{Elem: appendT},
		TypeParams: []string{"T"},
	}, false)
	// concat<T>(...xs: list<T>): list<T> - MEP-12.4. Every argument is
	// a list<T>; the variadic unifier in checkPrimary pins T from the
	// first argument and rejects any later argument whose element type
	// disagrees with T047.
	concatT := &TypeVar{Name: "T"}
	env.SetVar("concat", FuncType{
		Params:     []Type{},
		Return:     ListType{Elem: concatT},
		Variadic:   ListType{Elem: concatT},
		TypeParams: []string{"T"},
	}, false)
	// first<T>(xs: list<T>): T? - MEP-12.4 generic, MEP-16 Stage 4.
	// Under MEP-16 the empty list returns `none` instead of panicking,
	// so the static signature must say `T?`. Callers handle empties
	// with `first(xs) ?? default` or by narrowing through the binding.
	firstT := &TypeVar{Name: "T"}
	env.SetVar("first", FuncType{
		Params:     []Type{ListType{Elem: firstT}},
		Return:     OptionType{Elem: firstT},
		TypeParams: []string{"T"},
	}, false)
	env.SetVar("reverse", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
	}, false)
	// distinct<T>(xs: list<T>): list<T> - MEP-12.4. Shape-preserving;
	// the result list element type matches the input.
	distinctT := &TypeVar{Name: "T"}
	env.SetVar("distinct", FuncType{
		Params:     []Type{ListType{Elem: distinctT}},
		Return:     ListType{Elem: distinctT},
		TypeParams: []string{"T"},
	}, false)
	// push<T>(xs: list<T>, x: T): list<T> - MEP-12.4 mirror of append.
	pushT := &TypeVar{Name: "T"}
	env.SetVar("push", FuncType{
		Params:     []Type{ListType{Elem: pushT}, pushT},
		Return:     ListType{Elem: pushT},
		TypeParams: []string{"T"},
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
		TypeParams: []string{"K", "V"},
	}, false)
	valuesK := &TypeVar{Name: "K"}
	valuesV := &TypeVar{Name: "V"}
	env.SetVar("values", FuncType{
		Params:     []Type{MapType{Key: valuesK, Value: valuesV}},
		Return:     ListType{Elem: valuesV},
		TypeParams: []string{"K", "V"},
	}, false)
	// collect<T>(xs: list<T>): list<T> - MEP-12.4. The legacy any
	// signature also accepted GroupType; that overload is preserved by
	// checkBuiltinCall's "list or group" arity check, which runs after
	// the call-site unifier and rejects everything else with a tailored
	// message. For the list arm the parametric signature pins the
	// element type so consumers no longer need an explicit cast.
	collectT := &TypeVar{Name: "T"}
	env.SetVar("collect", FuncType{
		Params:     []Type{ListType{Elem: collectT}},
		Return:     ListType{Elem: collectT},
		TypeParams: []string{"T"},
	}, false)
	env.SetVar("range", FuncType{
		Params:   []Type{},
		Return:   ListType{Elem: IntType{}},
		Variadic: IntType{},
	}, false)
	env.SetVar("count", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
	}, false)
	env.SetVar("exists", FuncType{
		Params: []Type{AnyType{}},
		Return: BoolType{},
	}, false)
	// reduce<A, B>(xs: list<A>, fn: fun(B, A): B, init: B): B
	reduceA := &TypeVar{Name: "A"}
	reduceB := &TypeVar{Name: "B"}
	env.SetVar("reduce", FuncType{
		Params: []Type{
			ListType{Elem: reduceA},
			FuncType{Params: []Type{reduceB, reduceA}, Return: reduceB},
			reduceB,
		},
		Return:     reduceB,
		TypeParams: []string{"A", "B"},
	}, false)
}

// registerStringBuiltins covers string predicates, slicing, parsing,
// and formatting that does not return a number.
func registerStringBuiltins(env *Env) {
	env.SetVar("str", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
	}, false)
	env.SetVar("upper", FuncType{
		Params: []Type{StringType{}},
		Return: StringType{},
	}, false)
	env.SetVar("lower", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
	}, false)
	env.SetVar("trim", FuncType{
		Params: []Type{StringType{}},
		Return: StringType{},
	}, false)
	env.SetVar("contains", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: BoolType{},
	}, false)
	env.SetVar("split", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: ListType{Elem: StringType{}},
	}, false)
	env.SetVar("join", FuncType{
		Params: []Type{ListType{Elem: StringType{}}, StringType{}},
		Return: StringType{},
	}, false)
	env.SetVar("substring", FuncType{
		Params: []Type{StringType{}, IntType{}, IntType{}},
		Return: StringType{},
	}, false)
	env.SetVar("padStart", FuncType{
		Params: []Type{StringType{}, IntType{}, StringType{}},
		Return: StringType{},
	}, false)
	env.SetVar("substr", FuncType{
		Params: []Type{StringType{}, IntType{}, IntType{}},
		Return: StringType{},
	}, false)
	env.SetVar("indexOf", FuncType{
		Params: []Type{StringType{}, StringType{}},
		Return: IntType{},
	}, false)
	env.SetVar("repeat", FuncType{
		Params: []Type{StringType{}, IntType{}},
		Return: StringType{},
	}, false)
	env.SetVar("sha256", FuncType{
		Params: []Type{AnyType{}},
		Return: ListType{Elem: IntType{}},
	}, false)
}

// registerNumericBuiltins covers casting, arithmetic-ish helpers, and
// statistical reductions over lists of numbers.
func registerNumericBuiltins(env *Env) {
	env.SetVar("parseIntStr", FuncType{
		Params: []Type{StringType{}, IntType{}},
		Return: IntType{},
	}, false)
	env.SetVar("int", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
	}, false)
	env.SetVar("num", FuncType{
		Params: []Type{AnyType{}},
		Return: BigIntType{},
	}, false)
	env.SetVar("denom", FuncType{
		Params: []Type{AnyType{}},
		Return: BigIntType{},
	}, false)
	env.SetVar("avg", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
	}, false)
	env.SetVar("abs", FuncType{
		Params: []Type{AnyType{}},
		Return: AnyType{},
	}, false)
	env.SetVar("ceil", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
	}, false)
	env.SetVar("floor", FuncType{
		Params: []Type{AnyType{}},
		Return: FloatType{},
	}, false)
	env.SetVar("sum", FuncType{
		Params: []Type{AnyType{}},
		Return: IntType{},
	}, false)
	env.SetVar("round", FuncType{
		Params: []Type{FloatType{}, IntType{}},
		Return: FloatType{},
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
		TypeParams: []string{"T"},
	}, false)
	maxT := &TypeVar{Name: "T"}
	env.SetVar("max", FuncType{
		Params:     []Type{ListType{Elem: maxT}},
		Return:     maxT,
		TypeParams: []string{"T"},
	}, false)
}

// registerTimeBuiltins covers wall-clock and timing helpers. These are
// the only names that need EffectTime today.
func registerTimeBuiltins(env *Env) {
	env.SetVar("now", FuncType{
		Params:  []Type{},
		Return:  Int64Type{},
		Effects: NewEffectSet(EffectTime),
	}, false)
}

// registerMetaBuiltins covers reflection and dynamic eval.
func registerMetaBuiltins(env *Env) {
	env.SetVar("eval", FuncType{
		Params:  []Type{StringType{}},
		Return:  AnyType{},
		Effects: NewEffectSet(EffectMeta),
	}, false)
}

// registerIOBuiltins covers stdout, stdin, and the JSON helpers. The
// load/save/fetch surface is handled by dedicated AST nodes in
// check_expr.go rather than as named builtins; this group only carries
// the ones that look like ordinary functions.
func registerIOBuiltins(env *Env) {
	env.SetVar("print", FuncType{
		Params:   []Type{},
		Return:   UnitType{},
		Effects:  NewEffectSet(EffectIO),
		Variadic: AnyType{},
	}, false)
	env.SetVar("input", FuncType{
		Params:  []Type{},
		Return:  StringType{},
		Effects: NewEffectSet(EffectIO),
	}, false)
	env.SetVar("json", FuncType{
		Params:  []Type{AnyType{}},
		Return:  UnitType{},
		Effects: NewEffectSet(EffectIO),
	}, false)
	env.SetVar("to_json", FuncType{
		Params: []Type{AnyType{}},
		Return: StringType{},
	}, false)
}
