package apisurface

import (
	"errors"
	"reflect"
	"testing"
)

func TestParseTypeBasics(t *testing.T) {
	cases := []struct {
		in   string
		want GoType
	}{
		{"int", BasicType{Name: "int"}},
		{"string", BasicType{Name: "string"}},
		{"error", BasicType{Name: "error"}},
		{"any", BasicType{Name: "any"}},
		{"comparable", BasicType{Name: "comparable"}},
	}
	for _, tc := range cases {
		got, err := ParseType(tc.in)
		if err != nil {
			t.Errorf("ParseType(%q): %v", tc.in, err)
			continue
		}
		if !reflect.DeepEqual(got, tc.want) {
			t.Errorf("ParseType(%q) = %#v, want %#v", tc.in, got, tc.want)
		}
	}
}

func TestParseTypeComposites(t *testing.T) {
	cases := []struct {
		in   string
		want GoType
	}{
		{"*int", PointerType{Elem: BasicType{Name: "int"}}},
		{"**int", PointerType{Elem: PointerType{Elem: BasicType{Name: "int"}}}},
		{"[]string", SliceType{Elem: BasicType{Name: "string"}}},
		{"[]*string", SliceType{Elem: PointerType{Elem: BasicType{Name: "string"}}}},
		{"[4]byte", ArrayType{Len: 4, Elem: BasicType{Name: "byte"}}},
		{"map[string]int", MapType{Key: BasicType{Name: "string"}, Value: BasicType{Name: "int"}}},
		{"map[string][]int", MapType{Key: BasicType{Name: "string"}, Value: SliceType{Elem: BasicType{Name: "int"}}}},
	}
	for _, tc := range cases {
		got, err := ParseType(tc.in)
		if err != nil {
			t.Errorf("ParseType(%q): %v", tc.in, err)
			continue
		}
		if !reflect.DeepEqual(got, tc.want) {
			t.Errorf("ParseType(%q) = %#v, want %#v", tc.in, got, tc.want)
		}
	}
}

func TestParseTypeChan(t *testing.T) {
	cases := []struct {
		in  string
		dir ChanDir
	}{
		{"chan int", ChanBoth},
		{"chan<- int", ChanSend},
		{"<-chan int", ChanRecv},
	}
	for _, tc := range cases {
		got, err := ParseType(tc.in)
		if err != nil {
			t.Fatalf("ParseType(%q): %v", tc.in, err)
		}
		c, ok := got.(ChanType)
		if !ok {
			t.Errorf("ParseType(%q) returned %T; want ChanType", tc.in, got)
			continue
		}
		if c.Dir != tc.dir {
			t.Errorf("ParseType(%q).Dir = %d; want %d", tc.in, c.Dir, tc.dir)
		}
		if !reflect.DeepEqual(c.Elem, BasicType{Name: "int"}) {
			t.Errorf("ParseType(%q).Elem = %#v", tc.in, c.Elem)
		}
	}
}

func TestParseTypeNamed(t *testing.T) {
	cases := []struct {
		in  string
		pkg string
		nm  string
	}{
		{"io.Reader", "io", "Reader"},
		{"github.com/foo/bar.Baz", "github.com/foo/bar", "Baz"},
		{"MyType", "", "MyType"},
	}
	for _, tc := range cases {
		got, err := ParseType(tc.in)
		if err != nil {
			t.Fatalf("ParseType(%q): %v", tc.in, err)
		}
		n, ok := got.(NamedType)
		if !ok {
			t.Errorf("ParseType(%q) = %T; want NamedType", tc.in, got)
			continue
		}
		if n.PackagePath != tc.pkg || n.Name != tc.nm {
			t.Errorf("ParseType(%q): pkg=%q name=%q (want %q/%q)", tc.in, n.PackagePath, n.Name, tc.pkg, tc.nm)
		}
	}
}

func TestParseTypeGeneric(t *testing.T) {
	got, err := ParseType("Box[int]")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	n, ok := got.(NamedType)
	if !ok {
		t.Fatalf("got %T", got)
	}
	if n.Name != "Box" || len(n.TypeArgs) != 1 {
		t.Errorf("Box[int]: name=%q TypeArgs=%+v", n.Name, n.TypeArgs)
	}

	got, err = ParseType("Pair[string, int]")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	n = got.(NamedType)
	if len(n.TypeArgs) != 2 {
		t.Errorf("Pair: TypeArgs = %+v", n.TypeArgs)
	}

	got, err = ParseType("example.com/x.Box[int]")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	n = got.(NamedType)
	if n.PackagePath != "example.com/x" || n.Name != "Box" || len(n.TypeArgs) != 1 {
		t.Errorf("qualified generic: %+v", n)
	}
}

func TestParseTypeFunc(t *testing.T) {
	got, err := ParseType("func()")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	f := got.(FuncType)
	if len(f.Params) != 0 || len(f.Results) != 0 {
		t.Errorf("func(): %+v", f)
	}

	got, err = ParseType("func(int, string) error")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	f = got.(FuncType)
	if len(f.Params) != 2 || len(f.Results) != 1 {
		t.Errorf("params/results count: %+v", f)
	}

	got, err = ParseType("func(int) (string, error)")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	f = got.(FuncType)
	if len(f.Results) != 2 {
		t.Errorf("results = %+v", f.Results)
	}

	got, err = ParseType("func(...string) error")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	f = got.(FuncType)
	if !f.Variadic {
		t.Errorf("Variadic = false")
	}
	if _, ok := f.Params[0].(EllipsisType); !ok {
		t.Errorf("Params[0] = %T", f.Params[0])
	}
}

func TestParseTypeNested(t *testing.T) {
	got, err := ParseType("map[string]func(int) (io.Reader, error)")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	m := got.(MapType)
	f := m.Value.(FuncType)
	if len(f.Results) != 2 {
		t.Errorf("nested func results: %+v", f.Results)
	}
	if got.String() != "map[string]func(int) (io.Reader, error)" {
		t.Errorf("round-trip String() = %q", got.String())
	}
}

func TestParseTypeMalformed(t *testing.T) {
	cases := []string{
		"",
		"[",
		"[]",
		"map[",
		"map[]int",
		"map[int",
		"chan",
		"func",
		"func(",
		"func(int",
		"func() (",
		"<-",
		"<-int",
		"*",
		"[abc]int",
		"[1abc]int",
		"[1]",
		"Foo[",
		"Foo[int",
		"Foo,bar",
		"int trailing",
	}
	for _, tc := range cases {
		if _, err := ParseType(tc); err == nil {
			t.Errorf("ParseType(%q): want error, got nil", tc)
			continue
		} else if !errors.Is(err, ErrTypeParse) {
			t.Errorf("ParseType(%q): error is not ErrTypeParse: %v", tc, err)
		}
	}
}

func TestParseTypeInterfaceStruct(t *testing.T) {
	got, err := ParseType("interface{ Read(p []byte) (int, error) }")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	if _, ok := got.(InterfaceType); !ok {
		t.Errorf("got %T", got)
	}

	got, err = ParseType("struct { A int; B string }")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	if _, ok := got.(StructType); !ok {
		t.Errorf("got %T", got)
	}
}

func TestParseTypeStringRoundTrip(t *testing.T) {
	cases := []string{
		"int",
		"*int",
		"[]string",
		"[4]byte",
		"map[string]int",
		"chan int",
		"chan<- int",
		"<-chan int",
		"func()",
		"func(int) string",
		"func(int) (string, error)",
		"func(...string) error",
		"io.Reader",
		"github.com/foo/bar.Baz",
		"Box[int]",
		"Pair[string, int]",
		"map[string]func(int) error",
		"*github.com/foo/bar.Baz",
		"[]map[string]*Foo",
	}
	for _, in := range cases {
		got, err := ParseType(in)
		if err != nil {
			t.Errorf("ParseType(%q): %v", in, err)
			continue
		}
		if got.String() != in {
			t.Errorf("round-trip: %q -> %q", in, got.String())
		}
	}
}

func TestParseTypeChanVariants(t *testing.T) {
	// "chan <- T" with whitespace would never come out of TypeString
	// canonical form, but the parser handles whitespace anyway.
	got, err := ParseType("chan int")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	c := got.(ChanType)
	if c.Dir != ChanBoth {
		t.Errorf("Dir = %d", c.Dir)
	}
}

func TestEllipsisOnlyInFuncParams(t *testing.T) {
	// "...int" at top-level parses as EllipsisType but is not
	// semantically valid Go. The parser accepts the shape so phase 5
	// can decide what to do with it; we just confirm the shape here.
	got, err := ParseType("...int")
	if err != nil {
		t.Fatalf("ParseType: %v", err)
	}
	if _, ok := got.(EllipsisType); !ok {
		t.Errorf("got %T", got)
	}
}
