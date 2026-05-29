package typemap

import (
	"testing"

	breakers "mochi/package3/erlang/errors"
	"mochi/package3/erlang/etf"
)

// Helper: build {type, 1, Name, Args} — the standard ETF form for a built-in type.
func typeNode(name etf.Atom, args ...interface{}) etf.Tuple {
	list := make(etf.List, len(args))
	for i, a := range args {
		list[i] = a
	}
	return etf.Tuple{etf.Atom("type"), 1, name, list}
}

// Helper: build {atom, 1, Value} — atom literal.
func atomNode(v etf.Atom) etf.Tuple {
	return etf.Tuple{etf.Atom("atom"), 1, v}
}

// Helper: build {integer, 1, N}.
func intNode(n int) etf.Tuple {
	return etf.Tuple{etf.Atom("integer"), 1, n}
}

// Helper: {type, 1, product, [A, B, ...]} for fun arg list.
func productNode(args ...interface{}) etf.Tuple {
	list := make(etf.List, len(args))
	for i, a := range args {
		list[i] = a
	}
	return etf.Tuple{etf.Atom("type"), 1, etf.Atom("product"), list}
}

func assertScalar(t *testing.T, term interface{}, want MochiType) {
	t.Helper()
	r, err := TranslateType(term)
	if err != nil {
		t.Fatalf("TranslateType: unexpected error: %v", err)
	}
	if r.Scalar != want {
		t.Errorf("Scalar = %q, want %q", r.Scalar, want)
	}
}

func assertSkip(t *testing.T, term interface{}, wantReason breakers.SkipReason) {
	t.Helper()
	_, err := TranslateType(term)
	if err == nil {
		t.Fatal("TranslateType should return skip error")
	}
	reason, _, ok := IsSkip(err)
	if !ok {
		t.Fatalf("error is not a skip error: %v", err)
	}
	if reason != wantReason {
		t.Errorf("skip reason = %v, want %v", reason, wantReason)
	}
}

// ---- Primitive type tests ----

func TestTranslateBoolean(t *testing.T) {
	assertScalar(t, typeNode("boolean"), MochiBool)
}

func TestTranslateInteger(t *testing.T) {
	for _, name := range []etf.Atom{"integer", "non_neg_integer", "pos_integer", "neg_integer"} {
		assertScalar(t, typeNode(name), MochiInt)
	}
}

func TestTranslateFloat(t *testing.T) {
	assertScalar(t, typeNode("float"), MochiFloat)
}

func TestTranslateAtom(t *testing.T) {
	assertScalar(t, typeNode("atom"), MochiString)
}

func TestTranslateBinary(t *testing.T) {
	assertScalar(t, typeNode("binary"), MochiBytes)
}

func TestTranslatePid(t *testing.T) {
	assertScalar(t, typeNode("pid"), MochiPid)
}

func TestTranslatePort(t *testing.T) {
	assertScalar(t, typeNode("port"), MochiPort)
}

func TestTranslateReference(t *testing.T) {
	assertScalar(t, typeNode("reference"), MochiReference)
}

// ---- Skip tests ----

func TestSkipAnyTerm(t *testing.T) {
	assertSkip(t, typeNode("any"), breakers.SkipAnyTerm)
	assertSkip(t, typeNode("term"), breakers.SkipAnyTerm)
}

func TestSkipIodata(t *testing.T) {
	assertSkip(t, typeNode("iodata"), breakers.SkipIodata)
	assertSkip(t, typeNode("iolist"), breakers.SkipIodata)
}

func TestSkipBitstring(t *testing.T) {
	assertSkip(t, typeNode("bitstring"), breakers.SkipBitstring)
}

func TestSkipCharlist(t *testing.T) {
	assertSkip(t, typeNode("string"), breakers.SkipCharlist)
}

func TestSkipUntypedTuple(t *testing.T) {
	assertSkip(t, typeNode("tuple"), breakers.SkipUntypedTuple)
}

func TestSkipUntypedMap(t *testing.T) {
	assertSkip(t, typeNode("map"), breakers.SkipUntypedMap)
}

func TestSkipTypedMap(t *testing.T) {
	// map with arguments → typed map
	assocNode := etf.Tuple{etf.Atom("type"), 1, etf.Atom("map_field_assoc"),
		etf.List{typeNode("atom"), typeNode("integer")}}
	assertSkip(t, typeNode("map", assocNode), breakers.SkipTypedMap)
}

func TestSkipUntypedFun(t *testing.T) {
	assertSkip(t, typeNode("fun"), breakers.SkipUntypedFun)
}

func TestSkipRemoteType(t *testing.T) {
	remote := etf.Tuple{etf.Atom("remote_type"), 1,
		etf.List{atomNode("cowboy"), atomNode("req"), etf.List{}}}
	assertSkip(t, remote, breakers.SkipRemoteType)
}

func TestSkipComplexUnion(t *testing.T) {
	union := typeNode("union",
		typeNode("atom"),
		typeNode("integer"),
		typeNode("binary"),
	)
	assertSkip(t, union, breakers.SkipComplexUnion)
}

// ---- Compound type tests ----

func TestTranslateList(t *testing.T) {
	list := typeNode("list", typeNode("integer"))
	r, err := TranslateType(list)
	if err != nil {
		t.Fatalf("TranslateType: %v", err)
	}
	if r.ListT == nil {
		t.Fatal("expected ListT to be set")
	}
	if r.ListT.Elem != MochiInt {
		t.Errorf("ListT.Elem = %q, want %q", r.ListT.Elem, MochiInt)
	}
}

func TestTranslateTypedTuple(t *testing.T) {
	tup := typeNode("tuple", typeNode("integer"), typeNode("binary"))
	r, err := TranslateType(tup)
	if err != nil {
		t.Fatalf("TranslateType: %v", err)
	}
	if r.TupleT == nil {
		t.Fatal("expected TupleT to be set")
	}
	if len(r.TupleT.Elems) != 2 {
		t.Errorf("TupleT.Elems len = %d, want 2", len(r.TupleT.Elems))
	}
	if r.TupleT.Elems[0] != MochiInt || r.TupleT.Elems[1] != MochiBytes {
		t.Errorf("TupleT.Elems = %v", r.TupleT.Elems)
	}
}

func TestTranslateOkErrorUnion(t *testing.T) {
	// {ok, binary()} | {error, atom()}
	okTup := typeNode("tuple", atomNode("ok"), typeNode("binary"))
	errTup := typeNode("tuple", atomNode("error"), typeNode("atom"))
	union := typeNode("union", okTup, errTup)
	r, err := TranslateType(union)
	if err != nil {
		t.Fatalf("TranslateType: %v", err)
	}
	if r.ResultT == nil {
		t.Fatal("expected ResultT to be set for ok/error union")
	}
	if r.ResultT.Ok != MochiBytes {
		t.Errorf("ResultT.Ok = %q, want bytes", r.ResultT.Ok)
	}
}

func TestTranslateOkErrorUnion_Reversed(t *testing.T) {
	// {error, atom()} | {ok, integer()} — error first, ok second.
	errTup := typeNode("tuple", atomNode("error"), typeNode("atom"))
	okTup := typeNode("tuple", atomNode("ok"), typeNode("integer"))
	union := typeNode("union", errTup, okTup)
	r, err := TranslateType(union)
	if err != nil {
		t.Fatalf("TranslateType (reversed): %v", err)
	}
	if r.ResultT == nil {
		t.Fatal("ResultT should be set")
	}
	if r.ResultT.Ok != MochiInt {
		t.Errorf("ResultT.Ok = %q, want int", r.ResultT.Ok)
	}
}

func TestSkipNonOkErrorUnion(t *testing.T) {
	// Two branches but not ok/error pattern.
	union := typeNode("union", typeNode("integer"), typeNode("binary"))
	assertSkip(t, union, breakers.SkipNonOkErrorUnion)
}

func TestTranslateFun(t *testing.T) {
	// fun((integer()) -> binary())
	product := productNode(typeNode("integer"))
	fun := typeNode("fun", product, typeNode("binary"))
	r, err := TranslateType(fun)
	if err != nil {
		t.Fatalf("TranslateType fun: %v", err)
	}
	if r.FnT == nil {
		t.Fatal("expected FnT to be set")
	}
	if len(r.FnT.Args) != 1 || r.FnT.Args[0] != MochiInt {
		t.Errorf("FnT.Args = %v, want [int]", r.FnT.Args)
	}
	if r.FnT.Return != MochiBytes {
		t.Errorf("FnT.Return = %q, want bytes", r.FnT.Return)
	}
}

func TestTranslateFun_NoArgs(t *testing.T) {
	// fun(() -> boolean())
	product := productNode()
	fun := typeNode("fun", product, typeNode("boolean"))
	r, err := TranslateType(fun)
	if err != nil {
		t.Fatalf("TranslateType fun no-arg: %v", err)
	}
	if r.FnT == nil {
		t.Fatal("expected FnT")
	}
	if len(r.FnT.Args) != 0 {
		t.Errorf("FnT.Args should be empty, got %v", r.FnT.Args)
	}
	if r.FnT.Return != MochiBool {
		t.Errorf("FnT.Return = %q, want bool", r.FnT.Return)
	}
}

func TestTranslateAtomLiteral(t *testing.T) {
	assertScalar(t, atomNode("ok"), MochiUnit)
	assertScalar(t, atomNode("true"), MochiBool)
	assertScalar(t, atomNode("false"), MochiBool)
	// Other atoms → string
	r, err := TranslateType(atomNode("somevalue"))
	if err != nil {
		t.Fatalf("TranslateType atom literal: %v", err)
	}
	if r.Scalar != MochiString {
		t.Errorf("Scalar = %q, want string for atom literal", r.Scalar)
	}
}

func TestTranslateAnnType(t *testing.T) {
	// annotated type: {ann_type, 1, [{var,1,'X'}, {type,1,integer,[]}]}
	varNode := etf.Tuple{etf.Atom("var"), 1, etf.Atom("X")}
	ann := etf.Tuple{etf.Atom("ann_type"), 1, etf.List{varNode, typeNode("integer")}}
	assertScalar(t, ann, MochiInt)
}

func TestTranslateResultString(t *testing.T) {
	r := TranslateResult{ResultT: &Result{Ok: MochiInt, Err: MochiString}}
	if r.String() != "result<int, string>" {
		t.Errorf("Result.String() = %q", r.String())
	}
}

func TestTranslateListString(t *testing.T) {
	r := TranslateResult{ListT: &List{Elem: MochiBool}}
	if r.String() != "[]bool" {
		t.Errorf("List.String() = %q", r.String())
	}
}

func TestTranslateTupleString(t *testing.T) {
	r := TranslateResult{TupleT: &Tuple{Elems: []MochiType{MochiInt, MochiBytes}}}
	if r.String() != "tuple[int, bytes]" {
		t.Errorf("Tuple.String() = %q", r.String())
	}
}

func TestTranslateFnString(t *testing.T) {
	r := TranslateResult{FnT: &Fn{Args: []MochiType{MochiInt}, Return: MochiBool}}
	if r.String() != "fn(int) bool" {
		t.Errorf("Fn.String() = %q", r.String())
	}
}

func TestTranslateNil(t *testing.T) {
	r, err := TranslateType(typeNode("nil"))
	if err != nil {
		t.Fatalf("nil: %v", err)
	}
	if r.ListT == nil {
		t.Error("nil should produce ListT")
	}
}

func TestTranslateNonemptyList(t *testing.T) {
	r, err := TranslateType(typeNode("nonempty_list", typeNode("integer")))
	if err != nil {
		t.Fatalf("nonempty_list: %v", err)
	}
	if r.ListT == nil || r.ListT.Elem != MochiInt {
		t.Errorf("nonempty_list: unexpected result %v", r)
	}
}

func TestIsSkip(t *testing.T) {
	_, err := TranslateType(typeNode("any"))
	if err == nil {
		t.Fatal("expected error")
	}
	reason, detail, ok := IsSkip(err)
	if !ok {
		t.Fatal("IsSkip = false")
	}
	if reason != breakers.SkipAnyTerm {
		t.Errorf("reason = %v", reason)
	}
	if detail == "" {
		t.Error("detail should not be empty")
	}
}
