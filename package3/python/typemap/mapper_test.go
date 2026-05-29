package typemap

import (
	"strings"
	"testing"

	"mochi/package3/python/errors"
)

func TestMapScalars(t *testing.T) {
	m := &Mapper{}
	cases := map[string]string{
		"int":      "int",
		"float":    "float",
		"bool":     "bool",
		"str":      "string",
		"bytes":    "bytes",
		"None":     "None",
		"NoneType": "None",
	}
	for src, want := range cases {
		d := m.Map(src)
		if !d.OK() {
			t.Errorf("%q -> skip %s", src, d.Skip.Detail)
			continue
		}
		if got := d.Type.Render(); got != want {
			t.Errorf("%q -> %q, want %q", src, got, want)
		}
	}
}

func TestMapListVariants(t *testing.T) {
	m := &Mapper{}
	for _, src := range []string{"list[int]", "List[int]", "typing.List[int]"} {
		d := m.Map(src)
		if !d.OK() {
			t.Errorf("%q skipped: %v", src, d.Skip)
			continue
		}
		if got := d.Type.Render(); got != "list<int>" {
			t.Errorf("%q -> %q", src, got)
		}
	}
}

func TestMapDict(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Dict[str, int]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "map<string, int>" {
		t.Errorf("got %q", got)
	}
}

func TestMapDictRefusesNonScalarKey(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Dict[float, int]")
	if d.OK() {
		t.Fatal("expected refusal: float key")
	}
	d = m.Map("Dict[bool, int]")
	if d.OK() {
		t.Fatal("expected refusal: bool key")
	}
}

func TestMapTupleHomogeneous(t *testing.T) {
	m := &Mapper{}
	d := m.Map("tuple[int, ...]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "list<int>" {
		t.Errorf("tuple[T, ...] -> %q", got)
	}
}

func TestMapTupleHeterogeneous(t *testing.T) {
	m := &Mapper{}
	d := m.Map("tuple[int, str, bool]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "tuple<int, string, bool>" {
		t.Errorf("got %q", got)
	}
}

func TestMapSetAndFrozenSet(t *testing.T) {
	m := &Mapper{}
	d := m.Map("set[int]")
	if !d.OK() || d.Type.Render() != "set<int>" {
		t.Errorf("set: %+v", d)
	}
	d = m.Map("frozenset[int]")
	if !d.OK() || d.Type.Render() != "set<int>" {
		t.Errorf("frozenset: %+v", d)
	}
}

func TestMapOptional(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Optional[int]")
	if !d.OK() || d.Type.Render() != "int?" {
		t.Errorf("Optional[int]: %+v", d)
	}
	d = m.Map("int | None")
	if !d.OK() || d.Type.Render() != "int?" {
		t.Errorf("int | None: %+v", d)
	}
	d = m.Map("None | int")
	if !d.OK() || d.Type.Render() != "int?" {
		t.Errorf("None | int: %+v", d)
	}
}

func TestMapUnion(t *testing.T) {
	m := &Mapper{}
	d := m.Map("int | str")
	if !d.OK() || d.Type.Render() != "int | string" {
		t.Errorf("int | str: %+v", d)
	}
	d = m.Map("int | str | None")
	if !d.OK() || d.Type.Render() != "int | string?" {
		t.Errorf("int | str | None: got %q", d.Type.Render())
	}
}

func TestMapCallable(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Callable[[int, str], bool]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "fun(int, string): bool" {
		t.Errorf("got %q", got)
	}
}

func TestMapCallableEllipsisRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Callable[..., int]")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipParamSpec {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapAwaitable(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Awaitable[int]")
	if !d.OK() || d.Type.Render() != "async int" {
		t.Errorf("got %+v", d)
	}
}

func TestMapCoroutine(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Coroutine[Any, Any, int]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if got := d.Type.Render(); got != "async int" {
		t.Errorf("Coroutine[Y, S, R] -> %q (want async R)", got)
	}
}

func TestMapAsyncIterator(t *testing.T) {
	m := &Mapper{}
	d := m.Map("AsyncIterator[int]")
	if !d.OK() || d.Type.Render() != "stream<int>" {
		t.Errorf("got %+v", d)
	}
}

func TestMapIterator(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Iterator[int]")
	if !d.OK() || d.Type.Render() != "list<int>" {
		t.Errorf("got %+v", d)
	}
}

func TestMapAnyRefusedByDefault(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Any")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipUnsupportedTypingConstruct {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapAnyAllowedWhenPartial(t *testing.T) {
	m := &Mapper{AllowPartial: true}
	d := m.Map("Any")
	if !d.OK() {
		t.Fatalf("expected mapped Any: %v", d.Skip)
	}
}

func TestMapComplexRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("complex")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipNoComplexType {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapParamSpecRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("ParamSpec[P]")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipParamSpec {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapTypeVarTupleRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Unpack[Ts]")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.Reason != errors.SkipTypeVarTuple {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapGeneratorRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Generator[int, None, None]")
	if d.OK() {
		t.Fatal("expected refusal")
	}
}

func TestMapForwardRefResolved(t *testing.T) {
	m := &Mapper{Classes: map[string]bool{"MyClass": true}}
	d := m.Map(`"MyClass"`)
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindRef || d.Type.Name != "MyClass" {
		t.Errorf("got %+v", d.Type)
	}
}

func TestMapForwardRefUnresolved(t *testing.T) {
	m := &Mapper{}
	d := m.Map("UnknownClass")
	if d.OK() {
		t.Fatal("expected SkipForwardRef")
	}
	if d.Skip.Reason != errors.SkipForwardRef {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapTypeVar(t *testing.T) {
	m := &Mapper{TypeVars: map[string]bool{"T": true}}
	d := m.Map("T")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindTypeVar || d.Type.Name != "T" {
		t.Errorf("got %+v", d.Type)
	}
}

func TestMapTypeVarInList(t *testing.T) {
	m := &Mapper{TypeVars: map[string]bool{"T": true}}
	d := m.Map("List[T]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Render() != "list<T>" {
		t.Errorf("got %q", d.Type.Render())
	}
}

func TestMapClassRef(t *testing.T) {
	m := &Mapper{Classes: map[string]bool{"MyClass": true}}
	d := m.Map("MyClass")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindRef || d.Type.Name != "MyClass" {
		t.Errorf("got %+v", d.Type)
	}
}

func TestMapGenericClass(t *testing.T) {
	m := &Mapper{Classes: map[string]bool{"Container": true}}
	d := m.Map("Container[int]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Kind != KindRef || d.Type.Name != "Container" || len(d.Type.Params) != 1 {
		t.Errorf("got %+v", d.Type)
	}
}

func TestMapLiteralCollapsesToScalar(t *testing.T) {
	m := &Mapper{}
	cases := map[string]string{
		`Literal["a"]`:           "string",
		`Literal["a", "b", "c"]`: "string",
		`Literal[1]`:              "int",
		`Literal[1, 2, 3]`:        "int",
		`Literal[True]`:           "bool",
		`Literal[True, False]`:    "bool",
		`Literal[None]`:           "None",
	}
	for src, want := range cases {
		d := m.Map(src)
		if !d.OK() {
			t.Errorf("%q skipped: %v", src, d.Skip)
			continue
		}
		if got := d.Type.Render(); got != want {
			t.Errorf("%q -> %q, want %q", src, got, want)
		}
	}
}

func TestMapLiteralMixedRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map(`Literal["a", 1]`)
	if d.OK() {
		t.Fatal("expected refusal: mixed Literal")
	}
}

func TestMapFinalUnwraps(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Final[int]")
	if !d.OK() || d.Type.Render() != "int" {
		t.Errorf("got %+v", d)
	}
}

func TestMapClassVarUnwraps(t *testing.T) {
	m := &Mapper{}
	d := m.Map("ClassVar[int]")
	if !d.OK() || d.Type.Render() != "int" {
		t.Errorf("got %+v", d)
	}
}

func TestMapAnnotatedUnwraps(t *testing.T) {
	m := &Mapper{}
	d := m.Map(`Annotated[int, "range(0, 100)"]`)
	if !d.OK() || d.Type.Render() != "int" {
		t.Errorf("got %+v", d)
	}
}

func TestMapNotRequiredUnwraps(t *testing.T) {
	m := &Mapper{}
	d := m.Map("NotRequired[int]")
	if !d.OK() || d.Type.Render() != "int" {
		t.Errorf("got %+v", d)
	}
}

func TestMapTypeRefused(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Type[int]")
	if d.OK() {
		t.Fatal("expected refusal")
	}
}

func TestMapNestedSuccess(t *testing.T) {
	m := &Mapper{}
	d := m.Map("Dict[str, List[Optional[int]]]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Render() != "map<string, list<int?>>" {
		t.Errorf("got %q", d.Type.Render())
	}
}

func TestMapEmpty(t *testing.T) {
	m := &Mapper{}
	d := m.Map("")
	if d.OK() {
		t.Fatal("expected refusal on empty")
	}
}

func TestMapItemPathPropagates(t *testing.T) {
	m := &Mapper{ItemPath: "mod.Foo.bar"}
	d := m.Map("complex")
	if d.OK() {
		t.Fatal("expected refusal")
	}
	if d.Skip.ItemPath != "mod.Foo.bar" {
		t.Errorf("ItemPath = %q", d.Skip.ItemPath)
	}
}

func TestMapBareTypingConstructorRefused(t *testing.T) {
	m := &Mapper{}
	for _, src := range []string{"List", "Optional", "Union", "Callable"} {
		d := m.Map(src)
		if d.OK() {
			t.Errorf("%q: expected refusal", src)
		}
	}
}

func TestMapUnionWithAnyRefused(t *testing.T) {
	m := &Mapper{AllowPartial: true}
	d := m.Map("int | Any")
	if d.OK() {
		t.Fatal("expected SkipOpenUnion when Any appears in union (even with AllowPartial)")
	}
	if d.Skip.Reason != errors.SkipOpenUnion {
		t.Errorf("reason = %v", d.Skip.Reason)
	}
}

func TestMapBuiltinsPrefixStripped(t *testing.T) {
	m := &Mapper{}
	d := m.Map("builtins.int")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Render() != "int" {
		t.Errorf("got %q", d.Type.Render())
	}
}

func TestMapCollectionsAbcStripped(t *testing.T) {
	m := &Mapper{}
	d := m.Map("collections.abc.Iterable[int]")
	if !d.OK() {
		t.Fatalf("skipped: %v", d.Skip)
	}
	if d.Type.Render() != "list<int>" {
		t.Errorf("got %q", d.Type.Render())
	}
}

func TestMapInvalidArityList(t *testing.T) {
	m := &Mapper{}
	d := m.Map("List[int, str]")
	if d.OK() {
		t.Fatal("expected refusal: list takes 1 arg")
	}
	if !strings.Contains(d.Skip.Detail, "1 type argument") {
		t.Errorf("detail = %q", d.Skip.Detail)
	}
}
