package typemap

import (
	"errors"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

func mustParse(t *testing.T, s string) apisurface.GoType {
	t.Helper()
	gt, err := apisurface.ParseType(s)
	if err != nil {
		t.Fatalf("ParseType(%q): %v", s, err)
	}
	return gt
}

func TestMapBasicScalars(t *testing.T) {
	cases := map[string]string{
		"int":     "int",
		"int64":   "int",
		"int8":    "int",
		"uint":    "int",
		"byte":    "int",
		"rune":    "int",
		"uintptr": "int",
		"float32": "float",
		"float64": "float",
		"bool":    "bool",
		"string":  "string",
		"error":   "error",
	}
	m := NewMapper(nil)
	for in, want := range cases {
		gt := mustParse(t, in)
		mp, err := m.Map(gt)
		if err != nil {
			t.Errorf("Map(%q): %v", in, err)
			continue
		}
		s, ok := mp.Mochi.(ScalarType)
		if !ok {
			t.Errorf("Map(%q) = %T; want ScalarType", in, mp.Mochi)
			continue
		}
		if s.Name != want {
			t.Errorf("Map(%q).Name = %q; want %q", in, s.Name, want)
		}
		if mp.Direction != Copy {
			t.Errorf("Map(%q).Direction = %v; want Copy", in, mp.Direction)
		}
	}
}

func TestMapBytes(t *testing.T) {
	m := NewMapper(nil)
	mp, err := m.Map(mustParse(t, "[]byte"))
	if err != nil {
		t.Fatalf("Map: %v", err)
	}
	s, ok := mp.Mochi.(ScalarType)
	if !ok || s.Name != "bytes" {
		t.Errorf("[]byte -> %#v; want ScalarType{bytes}", mp.Mochi)
	}
}

func TestMapSlice(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "[]string"))
	l, ok := mp.Mochi.(ListType)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if l.Elem.(ScalarType).Name != "string" {
		t.Errorf("elem = %#v", l.Elem)
	}
}

func TestMapMap(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "map[string]int"))
	mm, ok := mp.Mochi.(MochiMap)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if mm.Key.(ScalarType).Name != "string" || mm.Value.(ScalarType).Name != "int" {
		t.Errorf("kv = %#v / %#v", mm.Key, mm.Value)
	}
}

func TestMapMapRejectsNonScalarKey(t *testing.T) {
	m := NewMapper(nil)
	_, err := m.Map(mustParse(t, "map[chan int]string"))
	if err == nil || !errors.Is(err, ErrUnmappable) {
		t.Errorf("want ErrUnmappable, got %v", err)
	}
}

func TestMapPointer(t *testing.T) {
	m := NewMapper(nil)
	// pointer to scalar -> option<int>, Copy
	mp, _ := m.Map(mustParse(t, "*int"))
	if _, ok := mp.Mochi.(OptionType); !ok {
		t.Errorf("ptr -> %T; want OptionType", mp.Mochi)
	}

	// pointer to chan -> handle (no double-wrap)
	mp, _ = m.Map(mustParse(t, "*chan int"))
	if _, ok := mp.Mochi.(HandleType); !ok {
		t.Errorf("*chan -> %T; want HandleType", mp.Mochi)
	}
}

func TestMapChan(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "chan int"))
	h, ok := mp.Mochi.(HandleType)
	if !ok {
		t.Fatalf("chan -> %T", mp.Mochi)
	}
	if !strings.Contains(h.Name, "int") {
		t.Errorf("handle.Name = %q", h.Name)
	}
	if mp.Direction != Handle {
		t.Errorf("Direction = %v", mp.Direction)
	}
}

func TestMapFunc(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "func(int) (string, error)"))
	ft, ok := mp.Mochi.(FuncType)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if len(ft.Params) != 1 || len(ft.Results) != 2 {
		t.Errorf("ft = %#v", ft)
	}
	if mp.Direction != Handle {
		t.Errorf("Direction = %v", mp.Direction)
	}
}

func TestMapVariadicFunc(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "func(...string) error"))
	ft := mp.Mochi.(FuncType)
	if _, ok := ft.Params[0].(ListType); !ok {
		t.Errorf("variadic param = %T; want ListType", ft.Params[0])
	}
}

func TestMapInterface(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "interface{}"))
	if _, ok := mp.Mochi.(AnyType); !ok {
		t.Errorf("interface{} -> %T; want AnyType", mp.Mochi)
	}
	mp, _ = m.Map(mustParse(t, "interface{ Read([]byte) (int, error) }"))
	if _, ok := mp.Mochi.(HandleType); !ok {
		t.Errorf("non-empty iface -> %T; want HandleType", mp.Mochi)
	}
}

func TestMapStructLiteral(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "struct { A int }"))
	if _, ok := mp.Mochi.(HandleType); !ok {
		t.Errorf("struct literal -> %T; want HandleType", mp.Mochi)
	}
}

func TestMapAnyComparable(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "any"))
	if _, ok := mp.Mochi.(AnyType); !ok {
		t.Errorf("any -> %T; want AnyType", mp.Mochi)
	}
	if mp.Direction != Handle {
		t.Errorf("any.Direction = %v; want Handle", mp.Direction)
	}
}

func TestMapNamedWithoutSurface(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "io.Reader"))
	h, ok := mp.Mochi.(HandleType)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if h.Name != "io.Reader" {
		t.Errorf("HandleType.Name = %q", h.Name)
	}
}

func TestMapNamedStructWithSurface(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Module:        "example.com/foo",
		Packages: []apisurface.Package{
			{
				ImportPath: "example.com/foo",
				Types: []apisurface.Type{
					{
						Name: "Point",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "X", Type: "int", Exported: true},
							{Name: "Y", Type: "int", Exported: true},
						},
					},
				},
			},
		},
	}
	s, err := apisurface.Load(file, apisurface.LoadOptions{})
	if err != nil {
		t.Fatal(err)
	}
	m := NewMapper(s)
	mp, _ := m.Map(mustParse(t, "example.com/foo.Point"))
	r, ok := mp.Mochi.(RecordType)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if r.Name != "Point" || len(r.Fields) != 2 {
		t.Errorf("Record = %+v", r)
	}
	if mp.Direction != Copy {
		t.Errorf("Direction = %v; want Copy", mp.Direction)
	}
}

func TestMapNamedStructWithUnexportedFieldFallsBackToHandle(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Packages: []apisurface.Package{
			{
				ImportPath: "x",
				Types: []apisurface.Type{
					{
						Name: "P",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "X", Type: "int", Exported: true},
							{Name: "y", Type: "int", Exported: false},
						},
					},
				},
			},
		},
	}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	m := NewMapper(s)
	mp, _ := m.Map(mustParse(t, "x.P"))
	if _, ok := mp.Mochi.(HandleType); !ok {
		t.Errorf("got %T; want HandleType", mp.Mochi)
	}
}

func TestMapNamedAlias(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Packages: []apisurface.Package{
			{
				ImportPath: "x",
				Types: []apisurface.Type{
					{Name: "Name", Kind: apisurface.KindAlias, AliasOf: "string"},
				},
			},
		},
	}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	m := NewMapper(s)
	mp, _ := m.Map(mustParse(t, "x.Name"))
	sc, ok := mp.Mochi.(ScalarType)
	if !ok || sc.Name != "string" {
		t.Errorf("alias -> %#v", mp.Mochi)
	}
}

func TestMapperCacheStable(t *testing.T) {
	m := NewMapper(nil)
	a, _ := m.Map(mustParse(t, "map[string][]int"))
	b, _ := m.Map(mustParse(t, "map[string][]int"))
	if a != b {
		t.Errorf("cache returned different pointers")
	}
}

func TestMapFuncDeclWrap(t *testing.T) {
	m := NewMapper(nil)
	fd := &apisurface.FuncDecl{
		Underlying: &apisurface.Func{Name: "Foo", Variadic: false},
		Params:     []apisurface.ParamDecl{{Name: "x", Type: apisurface.BasicType{Name: "int"}}},
		Results:    []apisurface.ParamDecl{{Type: apisurface.BasicType{Name: "string"}}},
	}
	mp, err := m.MapFunc(fd)
	if err != nil {
		t.Fatalf("MapFunc: %v", err)
	}
	ft := mp.Mochi.(FuncType)
	if len(ft.Params) != 1 || len(ft.Results) != 1 {
		t.Errorf("ft = %+v", ft)
	}
}

func TestTransferDirectionString(t *testing.T) {
	if Copy.String() != "copy" || View.String() != "view" || Handle.String() != "handle" {
		t.Errorf("dir string: %v %v %v", Copy, View, Handle)
	}
}

func TestMochiTypeStringRendering(t *testing.T) {
	cases := []struct {
		in   MochiType
		want string
	}{
		{ScalarType{Name: "int"}, "int"},
		{ListType{Elem: ScalarType{Name: "string"}}, "list<string>"},
		{MochiMap{Key: ScalarType{Name: "string"}, Value: ScalarType{Name: "int"}}, "map<string, int>"},
		{HandleType{Name: "io.Reader"}, "handle<io.Reader>"},
		{HandleType{}, "handle"},
		{OptionType{Elem: ScalarType{Name: "int"}}, "option<int>"},
		{AnyType{}, "any"},
		{FuncType{Params: []MochiType{ScalarType{Name: "int"}}, Results: nil}, "fn(int) -> unit"},
		{FuncType{Params: nil, Results: []MochiType{ScalarType{Name: "int"}}}, "fn() -> int"},
		{FuncType{Results: []MochiType{ScalarType{Name: "int"}, ScalarType{Name: "error"}}}, "fn() -> (int, error)"},
		{RecordType{Name: "Foo"}, "Foo"},
	}
	for _, tc := range cases {
		if got := tc.in.String(); got != tc.want {
			t.Errorf("String() = %q; want %q", got, tc.want)
		}
	}
}

func TestArrayMapsToList(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "[4]byte"))
	if _, ok := mp.Mochi.(ListType); !ok {
		t.Errorf("[4]byte -> %T", mp.Mochi)
	}
}

func TestNilMapErrors(t *testing.T) {
	m := NewMapper(nil)
	if _, err := m.Map(nil); err == nil {
		t.Errorf("Map(nil): want error")
	}
}

func TestEllipsisTopLevel(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "...int"))
	if _, ok := mp.Mochi.(ListType); !ok {
		t.Errorf("...int -> %T", mp.Mochi)
	}
}

func TestMapNestedComposites(t *testing.T) {
	m := NewMapper(nil)
	// list<list<int>>
	mp, err := m.Map(mustParse(t, "[][]int"))
	if err != nil {
		t.Fatalf("Map: %v", err)
	}
	outer, ok := mp.Mochi.(ListType)
	if !ok {
		t.Fatalf("outer = %T", mp.Mochi)
	}
	inner, ok := outer.Elem.(ListType)
	if !ok {
		t.Fatalf("inner = %T", outer.Elem)
	}
	if inner.Elem.(ScalarType).Name != "int" {
		t.Errorf("innermost = %#v", inner.Elem)
	}

	// map<string, list<map<string, int>>>
	mp, err = m.Map(mustParse(t, "map[string][]map[string]int"))
	if err != nil {
		t.Fatalf("Map: %v", err)
	}
	if got := mp.Mochi.String(); got != "map<string, list<map<string, int>>>" {
		t.Errorf("nested map = %q", got)
	}
}

func TestMapPointerToRecordYieldsOptionRecord(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Packages: []apisurface.Package{
			{
				ImportPath: "x",
				Types: []apisurface.Type{
					{
						Name: "P",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "X", Type: "int", Exported: true},
						},
					},
				},
			},
		},
	}
	s, err := apisurface.Load(file, apisurface.LoadOptions{})
	if err != nil {
		t.Fatal(err)
	}
	m := NewMapper(s)
	mp, err := m.Map(mustParse(t, "*x.P"))
	if err != nil {
		t.Fatalf("Map: %v", err)
	}
	opt, ok := mp.Mochi.(OptionType)
	if !ok {
		t.Fatalf("got %T; want OptionType", mp.Mochi)
	}
	r, ok := opt.Elem.(RecordType)
	if !ok {
		t.Fatalf("opt.Elem = %T", opt.Elem)
	}
	if r.Name != "P" {
		t.Errorf("record name = %q", r.Name)
	}
}

func TestMapStructWithHandleFieldFallsBack(t *testing.T) {
	// A struct whose field is itself a chan must fall back to a
	// handle — record requires every field be Copy.
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Packages: []apisurface.Package{
			{
				ImportPath: "x",
				Types: []apisurface.Type{
					{
						Name: "Hub",
						Kind: apisurface.KindStruct,
						Fields: []apisurface.Field{
							{Name: "Name", Type: "string", Exported: true},
							{Name: "Sink", Type: "chan int", Exported: true},
						},
					},
				},
			},
		},
	}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	m := NewMapper(s)
	mp, _ := m.Map(mustParse(t, "x.Hub"))
	h, ok := mp.Mochi.(HandleType)
	if !ok {
		t.Fatalf("got %T; want HandleType", mp.Mochi)
	}
	if h.Name != "Hub" {
		t.Errorf("handle.Name = %q", h.Name)
	}
	// The Note should explain the demotion.
	joined := strings.Join(mp.Notes, " | ")
	if !strings.Contains(joined, "Sink") {
		t.Errorf("Notes missing reason; got %q", joined)
	}
}

func TestMapCrossPackageUnknownHandleQualified(t *testing.T) {
	// With no surface, a cross-package named type becomes a handle
	// keyed by qualified name.
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "github.com/spf13/cobra.Command"))
	h, ok := mp.Mochi.(HandleType)
	if !ok {
		t.Fatalf("got %T", mp.Mochi)
	}
	if !strings.Contains(h.Name, "cobra") || !strings.Contains(h.Name, "Command") {
		t.Errorf("handle.Name = %q", h.Name)
	}
}

func TestMapVariadicHasListLast(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "func(string, ...int) bool"))
	ft := mp.Mochi.(FuncType)
	if len(ft.Params) != 2 {
		t.Fatalf("params = %d", len(ft.Params))
	}
	if _, ok := ft.Params[1].(ListType); !ok {
		t.Errorf("variadic param = %T; want ListType", ft.Params[1])
	}
}

func TestMapNamedAliasToComplex(t *testing.T) {
	// `type IntList = []int` -> list<int>
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Packages: []apisurface.Package{
			{
				ImportPath: "x",
				Types: []apisurface.Type{
					{Name: "IntList", Kind: apisurface.KindAlias, AliasOf: "[]int"},
				},
			},
		},
	}
	s, _ := apisurface.Load(file, apisurface.LoadOptions{})
	m := NewMapper(s)
	mp, err := m.Map(mustParse(t, "x.IntList"))
	if err != nil {
		t.Fatalf("Map: %v", err)
	}
	if _, ok := mp.Mochi.(ListType); !ok {
		t.Errorf("alias -> %T; want ListType", mp.Mochi)
	}
}

func TestMapErrorWrapsBaseError(t *testing.T) {
	m := NewMapper(nil)
	_, err := m.Map(apisurface.BasicType{Name: "totally-not-a-type"})
	if err == nil {
		t.Fatalf("want error")
	}
	if !errors.Is(err, ErrUnmappable) {
		t.Errorf("err = %v; want wraps ErrUnmappable", err)
	}
}

func TestWideningNoteAttachedForNarrowGoInt(t *testing.T) {
	m := NewMapper(nil)
	mp, _ := m.Map(mustParse(t, "int32"))
	if len(mp.Notes) == 0 {
		t.Errorf("int32 should carry a widening note; got %v", mp.Notes)
	}
}
