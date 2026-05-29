package typemap

import "testing"

func TestParsePyTypeName(t *testing.T) {
	p, err := ParsePyType("int")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyName || p.Name != "int" {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeQualified(t *testing.T) {
	p, err := ParsePyType("typing.List")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyAttr || p.Name != "List" {
		t.Errorf("%+v", p)
	}
	if p.QualifiedName() != "typing.List" {
		t.Errorf("qualified = %q", p.QualifiedName())
	}
}

func TestParsePyTypeDoubleAttribute(t *testing.T) {
	p, err := ParsePyType("collections.abc.Iterable")
	if err != nil {
		t.Fatal(err)
	}
	if p.QualifiedName() != "collections.abc.Iterable" {
		t.Errorf("qualified = %q", p.QualifiedName())
	}
}

func TestParsePyTypeSubscript(t *testing.T) {
	p, err := ParsePyType("List[int]")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PySubscript {
		t.Fatalf("kind = %v", p.Kind)
	}
	if p.Base.Name != "List" {
		t.Errorf("base = %+v", p.Base)
	}
	if len(p.Args) != 1 || p.Args[0].Name != "int" {
		t.Errorf("args = %+v", p.Args)
	}
}

func TestParsePyTypeNestedSubscript(t *testing.T) {
	p, err := ParsePyType("Dict[str, List[int]]")
	if err != nil {
		t.Fatal(err)
	}
	if len(p.Args) != 2 {
		t.Fatalf("args = %+v", p.Args)
	}
	if p.Args[1].Kind != PySubscript || p.Args[1].Base.Name != "List" {
		t.Errorf("nested = %+v", p.Args[1])
	}
}

func TestParsePyTypeQualifiedSubscript(t *testing.T) {
	p, err := ParsePyType("typing.Dict[str, int]")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PySubscript {
		t.Fatalf("kind = %v", p.Kind)
	}
	if p.Base.QualifiedName() != "typing.Dict" {
		t.Errorf("base = %q", p.Base.QualifiedName())
	}
}

func TestParsePyTypeUnion(t *testing.T) {
	p, err := ParsePyType("int | str")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyUnion {
		t.Fatalf("kind = %v", p.Kind)
	}
	if len(p.Args) != 2 {
		t.Errorf("branches = %+v", p.Args)
	}
}

func TestParsePyTypeUnionMany(t *testing.T) {
	p, err := ParsePyType("int | str | bool | None")
	if err != nil {
		t.Fatal(err)
	}
	if len(p.Args) != 4 {
		t.Errorf("branches = %+v", p.Args)
	}
}

func TestParsePyTypeOptional(t *testing.T) {
	p, err := ParsePyType("Optional[int]")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PySubscript || p.Base.Name != "Optional" {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeCallable(t *testing.T) {
	p, err := ParsePyType("Callable[[int, str], bool]")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PySubscript || len(p.Args) != 2 {
		t.Fatalf("%+v", p)
	}
	if p.Args[0].Kind != PyTuple || len(p.Args[0].Args) != 2 {
		t.Errorf("params = %+v", p.Args[0])
	}
	if p.Args[1].Name != "bool" {
		t.Errorf("return = %+v", p.Args[1])
	}
}

func TestParsePyTypeCallableEllipsis(t *testing.T) {
	p, err := ParsePyType("Callable[..., int]")
	if err != nil {
		t.Fatal(err)
	}
	if p.Args[0].Kind != PyEllipsis {
		t.Errorf("params = %+v", p.Args[0])
	}
}

func TestParsePyTypeTupleEllipsis(t *testing.T) {
	p, err := ParsePyType("tuple[int, ...]")
	if err != nil {
		t.Fatal(err)
	}
	if len(p.Args) != 2 || p.Args[1].Kind != PyEllipsis {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeForwardRef(t *testing.T) {
	p, err := ParsePyType(`"MyClass"`)
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyLiteral || p.Literal != `"MyClass"` {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeLiteralValues(t *testing.T) {
	cases := []string{
		`Literal["a"]`,
		`Literal[1]`,
		`Literal[True]`,
		`Literal[None]`,
		`Literal["a", "b", "c"]`,
	}
	for _, src := range cases {
		if _, err := ParsePyType(src); err != nil {
			t.Errorf("%q: %v", src, err)
		}
	}
}

func TestParsePyTypeNoneLiteral(t *testing.T) {
	p, err := ParsePyType("None")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyLiteral || p.Literal != "None" {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeParens(t *testing.T) {
	p, err := ParsePyType("(int | str)")
	if err != nil {
		t.Fatal(err)
	}
	if p.Kind != PyUnion {
		t.Errorf("%+v", p)
	}
}

func TestParsePyTypeError(t *testing.T) {
	cases := []string{
		"List[",
		"List]",
		"int |",
		"|int",
		"((int)",
		`"unterminated`,
	}
	for _, src := range cases {
		if _, err := ParsePyType(src); err == nil {
			t.Errorf("%q: expected error", src)
		}
	}
}

func TestParsePyTypeTrailingInput(t *testing.T) {
	if _, err := ParsePyType("int foo"); err == nil {
		t.Error("expected trailing input error")
	}
}

func TestPyTypeStringRoundTrip(t *testing.T) {
	cases := []string{
		"int",
		"typing.List",
		"List[int]",
		"Dict[str, int]",
		"Callable[[int, str], bool]",
		"int | str",
	}
	for _, src := range cases {
		p, err := ParsePyType(src)
		if err != nil {
			t.Errorf("%q: %v", src, err)
			continue
		}
		if p.String() != src {
			t.Errorf("round-trip: got %q, want %q", p.String(), src)
		}
	}
}

func TestQualifiedNameNonIdent(t *testing.T) {
	p := PyType{Kind: PyLiteral, Literal: `"x"`}
	if p.QualifiedName() != "" {
		t.Error("QualifiedName should be empty for non-ident")
	}
}
