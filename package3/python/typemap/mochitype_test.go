package typemap

import "testing"

func TestKindString(t *testing.T) {
	cases := map[Kind]string{
		KindScalar:    "scalar",
		KindList:      "list",
		KindTuple:     "tuple",
		KindMap:       "map",
		KindSet:       "set",
		KindFun:       "fun",
		KindAsync:     "async",
		KindStream:    "stream",
		KindOptional:  "optional",
		KindSum:       "sum",
		KindRecord:    "record",
		KindInterface: "interface",
		KindRef:       "ref",
		KindTypeVar:   "typevar",
		KindUnknown:   "unknown",
		Kind(99):      "unknown",
	}
	for k, want := range cases {
		if got := k.String(); got != want {
			t.Errorf("Kind(%d).String() = %q, want %q", k, got, want)
		}
	}
}

func TestRenderScalars(t *testing.T) {
	for _, n := range []string{"int", "float", "bool", "string", "bytes", "None"} {
		if got := NewScalar(n).Render(); got != n {
			t.Errorf("NewScalar(%q).Render() = %q", n, got)
		}
	}
}

func TestRenderList(t *testing.T) {
	got := NewList(NewScalar("int")).Render()
	if got != "list<int>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderNestedList(t *testing.T) {
	got := NewList(NewList(NewScalar("int"))).Render()
	if got != "list<list<int>>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderMap(t *testing.T) {
	got := NewMap(NewScalar("string"), NewScalar("int")).Render()
	if got != "map<string, int>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderSet(t *testing.T) {
	got := NewSet(NewScalar("int")).Render()
	if got != "set<int>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderTuple(t *testing.T) {
	got := NewTuple(NewScalar("int"), NewScalar("string"), NewScalar("bool")).Render()
	if got != "tuple<int, string, bool>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderOptional(t *testing.T) {
	got := NewOptional(NewScalar("int")).Render()
	if got != "int?" {
		t.Errorf("got %q", got)
	}
}

func TestRenderNestedOptionalIdempotent(t *testing.T) {
	got := NewOptional(NewOptional(NewScalar("int"))).Render()
	if got != "int?" {
		t.Errorf("got %q (Optional should idempotently flatten)", got)
	}
}

func TestRenderSumNoNone(t *testing.T) {
	got := NewSum(NewScalar("int"), NewScalar("string")).Render()
	if got != "int | string" {
		t.Errorf("got %q", got)
	}
}

func TestRenderSumWithNoneBecomesOptional(t *testing.T) {
	got := NewSum(NewScalar("int"), NewScalar("None")).Render()
	if got != "int?" {
		t.Errorf("got %q", got)
	}
}

func TestRenderSumSingleBranch(t *testing.T) {
	got := NewSum(NewScalar("int")).Render()
	if got != "int" {
		t.Errorf("got %q", got)
	}
}

func TestRenderSumAllNoneCollapses(t *testing.T) {
	got := NewSum(NewScalar("None"), NewScalar("None")).Render()
	if got != "None" {
		t.Errorf("got %q", got)
	}
}

func TestRenderAsync(t *testing.T) {
	got := NewAsync(NewScalar("int")).Render()
	if got != "async int" {
		t.Errorf("got %q", got)
	}
}

func TestRenderStream(t *testing.T) {
	got := NewStream(NewScalar("int")).Render()
	if got != "stream<int>" {
		t.Errorf("got %q", got)
	}
}

func TestRenderFun(t *testing.T) {
	got := NewFun(NewScalar("bool"), NewScalar("int"), NewScalar("string")).Render()
	if got != "fun(int, string): bool" {
		t.Errorf("got %q", got)
	}
}

func TestRenderFunNoParams(t *testing.T) {
	got := NewFun(NewScalar("int")).Render()
	if got != "fun(): int" {
		t.Errorf("got %q", got)
	}
}

func TestRenderRef(t *testing.T) {
	got := NewRef("MyClass").Render()
	if got != "MyClass" {
		t.Errorf("got %q", got)
	}
}

func TestRenderTypeVar(t *testing.T) {
	got := NewTypeVar("T").Render()
	if got != "T" {
		t.Errorf("got %q", got)
	}
}

func TestRenderRecord(t *testing.T) {
	r := MochiType{
		Kind: KindRecord,
		Name: "Point",
		Fields: []MochiField{
			{Name: "x", Type: NewScalar("int")},
			{Name: "y", Type: NewScalar("int")},
		},
	}
	got := r.Render()
	if got != "record Point { x: int, y: int }" {
		t.Errorf("got %q", got)
	}
}

func TestRenderRecordFieldsSorted(t *testing.T) {
	r := MochiType{
		Kind: KindRecord,
		Name: "Mix",
		Fields: []MochiField{
			{Name: "z", Type: NewScalar("int")},
			{Name: "a", Type: NewScalar("string")},
		},
	}
	got := r.Render()
	if got != "record Mix { a: string, z: int }" {
		t.Errorf("got %q (fields must render sorted)", got)
	}
}

func TestRenderInterface(t *testing.T) {
	i := MochiType{
		Kind: KindInterface,
		Name: "Greeter",
		Methods: []MochiMethod{
			{Name: "hello", Params: []MochiType{NewScalar("string")}, Return: NewScalar("string")},
		},
	}
	got := i.Render()
	if got != "interface Greeter { hello(string): string }" {
		t.Errorf("got %q", got)
	}
}

func TestRenderInterfaceMethodsSorted(t *testing.T) {
	i := MochiType{
		Kind: KindInterface,
		Name: "Mix",
		Methods: []MochiMethod{
			{Name: "z", Return: NewScalar("int")},
			{Name: "a", Return: NewScalar("string")},
		},
	}
	got := i.Render()
	if got != "interface Mix { a(): string; z(): int }" {
		t.Errorf("got %q (methods must render sorted)", got)
	}
}

func TestRenderAsyncInterfaceMethod(t *testing.T) {
	i := MochiType{
		Kind: KindInterface,
		Name: "Fetcher",
		Methods: []MochiMethod{
			{Name: "fetch", IsAsync: true, Params: []MochiType{NewScalar("string")}, Return: NewScalar("bytes")},
		},
	}
	got := i.Render()
	if got != "interface Fetcher { async fetch(string): bytes }" {
		t.Errorf("got %q", got)
	}
}

func TestEqual(t *testing.T) {
	a := NewList(NewMap(NewScalar("string"), NewScalar("int")))
	b := NewList(NewMap(NewScalar("string"), NewScalar("int")))
	if !a.Equal(b) {
		t.Error("expected structural equality")
	}
	c := NewList(NewMap(NewScalar("string"), NewScalar("float")))
	if a.Equal(c) {
		t.Error("differing inner type should compare unequal")
	}
}

func TestEqualRecords(t *testing.T) {
	a := MochiType{Kind: KindRecord, Name: "P", Fields: []MochiField{{Name: "x", Type: NewScalar("int")}}}
	b := MochiType{Kind: KindRecord, Name: "P", Fields: []MochiField{{Name: "x", Type: NewScalar("int")}}}
	if !a.Equal(b) {
		t.Error("expected equality")
	}
	c := MochiType{Kind: KindRecord, Name: "P", Fields: []MochiField{{Name: "x", Type: NewScalar("float")}}}
	if a.Equal(c) {
		t.Error("differing field type should compare unequal")
	}
}

func TestFunReturnAndParams(t *testing.T) {
	t1 := NewFun(NewScalar("bool"), NewScalar("int"), NewScalar("string"))
	if t1.Return().Render() != "bool" {
		t.Errorf("Return = %q", t1.Return().Render())
	}
	if len(t1.FunParams()) != 2 {
		t.Errorf("FunParams = %d", len(t1.FunParams()))
	}
}

func TestReturnPanicsOnNonFun(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic")
		}
	}()
	_ = NewScalar("int").Return()
}

func TestFunParamsPanicsOnNonFun(t *testing.T) {
	defer func() {
		if r := recover(); r == nil {
			t.Error("expected panic")
		}
	}()
	_ = NewScalar("int").FunParams()
}
