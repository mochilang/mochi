package stubs

import (
	"strings"
	"testing"
)

func TestParsePYIEmpty(t *testing.T) {
	m, err := ParsePYI("")
	if err != nil {
		t.Fatal(err)
	}
	if m == nil {
		t.Fatal("nil module")
	}
	if len(m.Imports)+len(m.Classes)+len(m.Functions)+len(m.Aliases)+len(m.Constants) != 0 {
		t.Errorf("expected empty surface, got %+v", m)
	}
}

func TestParsePYIImports(t *testing.T) {
	src := `import os
import sys as system
from typing import Any, List, Optional
from collections.abc import Iterable as It
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Imports) != 4 {
		t.Fatalf("len(Imports) = %d, want 4: %+v", len(m.Imports), m.Imports)
	}
	if m.Imports[0].Module != "" || m.Imports[0].Names[0].Name != "os" {
		t.Errorf("import os: %+v", m.Imports[0])
	}
	if m.Imports[1].Names[0].Alias != "system" {
		t.Errorf("alias not parsed: %+v", m.Imports[1])
	}
	if m.Imports[2].Module != "typing" {
		t.Errorf("from-module = %q", m.Imports[2].Module)
	}
	if len(m.Imports[2].Names) != 3 {
		t.Errorf("names = %+v", m.Imports[2].Names)
	}
	if m.Imports[3].Names[0].Name != "Iterable" || m.Imports[3].Names[0].Alias != "It" {
		t.Errorf("aliased from-import: %+v", m.Imports[3])
	}
}

func TestParsePYIFromImportParenthesised(t *testing.T) {
	src := `from typing import (
    Any,
    List,
    Optional,
)
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Imports) != 1 {
		t.Fatalf("imports = %+v", m.Imports)
	}
	got := m.Imports[0]
	if got.Module != "typing" {
		t.Errorf("module = %q", got.Module)
	}
	if len(got.Names) != 3 {
		t.Errorf("names = %+v", got.Names)
	}
	wantNames := []string{"Any", "List", "Optional"}
	for i, n := range got.Names {
		if n.Name != wantNames[i] {
			t.Errorf("name[%d] = %q, want %q", i, n.Name, wantNames[i])
		}
	}
}

func TestParsePYIClassesBasic(t *testing.T) {
	src := `class Foo:
    x: int
    y: str = "hi"
    def m(self) -> int: ...

class Bar(Foo):
    ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Classes) != 2 {
		t.Fatalf("classes = %+v", m.Classes)
	}
	foo := m.Classes[0]
	if foo.Name != "Foo" {
		t.Errorf("Name = %q", foo.Name)
	}
	if len(foo.Fields) != 2 {
		t.Errorf("Fields = %+v", foo.Fields)
	}
	if foo.Fields[0].Name != "x" || foo.Fields[0].Type != "int" {
		t.Errorf("field 0: %+v", foo.Fields[0])
	}
	if foo.Fields[1].Default != `"hi"` {
		t.Errorf("field 1 default = %q", foo.Fields[1].Default)
	}
	if len(foo.Methods) != 1 || foo.Methods[0].Name != "m" {
		t.Errorf("Methods = %+v", foo.Methods)
	}
	bar := m.Classes[1]
	if len(bar.Bases) != 1 || bar.Bases[0] != "Foo" {
		t.Errorf("Bar bases = %+v", bar.Bases)
	}
}

func TestParsePYIProtocolAndTypedDict(t *testing.T) {
	src := `class P(Protocol):
    def f(self) -> int: ...

class T(TypedDict):
    x: int

class TG(TypedDict, total=False):
    y: str
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if !m.Classes[0].IsProtocol {
		t.Error("class P should be Protocol")
	}
	if !m.Classes[1].IsTypedDict {
		t.Error("class T should be TypedDict")
	}
	if !m.Classes[2].IsTypedDict {
		t.Error("class TG should be TypedDict (with total=False)")
	}
}

func TestParsePYIDataclassDecorator(t *testing.T) {
	src := `@dataclass
class D:
    x: int

@dataclasses.dataclass
class E:
    y: int
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if !m.Classes[0].IsDataclass {
		t.Error("D should be dataclass")
	}
	if !m.Classes[1].IsDataclass {
		t.Error("E should be dataclass (dataclasses.dataclass)")
	}
}

func TestParsePYIFunctions(t *testing.T) {
	src := `def simple() -> None: ...
async def fetch(url: str) -> bytes: ...
def add(a: int, b: int = 0) -> int: ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Functions) != 3 {
		t.Fatalf("funcs = %+v", m.Functions)
	}
	if m.Functions[0].ReturnType != "None" {
		t.Errorf("simple return = %q", m.Functions[0].ReturnType)
	}
	if !m.Functions[1].IsAsync {
		t.Error("fetch should be async")
	}
	if m.Functions[1].ReturnType != "bytes" {
		t.Errorf("fetch return = %q", m.Functions[1].ReturnType)
	}
	add := m.Functions[2]
	if len(add.Params) != 2 {
		t.Fatalf("add params = %+v", add.Params)
	}
	if add.Params[1].Default != "0" {
		t.Errorf("default = %q", add.Params[1].Default)
	}
}

func TestParsePYIPositionalOnly(t *testing.T) {
	src := `def f(a: int, b: int, /, c: int) -> int: ...`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	f := m.Functions[0]
	if len(f.Params) != 3 {
		t.Fatalf("params = %+v", f.Params)
	}
	if f.Params[0].Kind != ParamPositionalOnly {
		t.Errorf("a kind = %v", f.Params[0].Kind)
	}
	if f.Params[1].Kind != ParamPositionalOnly {
		t.Errorf("b kind = %v", f.Params[1].Kind)
	}
	if f.Params[2].Kind != ParamPositional {
		t.Errorf("c kind = %v", f.Params[2].Kind)
	}
}

func TestParsePYIKeywordOnly(t *testing.T) {
	src := `def f(a: int, *, b: int, c: int = 1) -> int: ...`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	f := m.Functions[0]
	if len(f.Params) != 3 {
		t.Fatalf("params = %+v", f.Params)
	}
	if f.Params[0].Kind != ParamPositional {
		t.Errorf("a kind = %v", f.Params[0].Kind)
	}
	if f.Params[1].Kind != ParamKeywordOnly {
		t.Errorf("b kind = %v", f.Params[1].Kind)
	}
	if f.Params[2].Kind != ParamKeywordOnly {
		t.Errorf("c kind = %v", f.Params[2].Kind)
	}
}

func TestParsePYIVarArgs(t *testing.T) {
	src := `def f(*args: int, **kwargs: str) -> None: ...`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	f := m.Functions[0]
	if len(f.Params) != 2 {
		t.Fatalf("params = %+v", f.Params)
	}
	if f.Params[0].Kind != ParamVarArgs || f.Params[0].Name != "args" {
		t.Errorf("args: %+v", f.Params[0])
	}
	if f.Params[1].Kind != ParamKwArgs || f.Params[1].Name != "kwargs" {
		t.Errorf("kwargs: %+v", f.Params[1])
	}
}

func TestParsePYIMethodDecorators(t *testing.T) {
	src := `class C:
    @property
    def x(self) -> int: ...
    @staticmethod
    def y() -> int: ...
    @overload
    def z(self, a: int) -> int: ...
    @overload
    def z(self, a: str) -> str: ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	c := m.Classes[0]
	if len(c.Methods) != 4 {
		t.Fatalf("methods = %+v", c.Methods)
	}
	if c.Methods[0].Decorators[0] != "property" {
		t.Errorf("property decorator missing: %+v", c.Methods[0])
	}
	if c.Methods[1].Decorators[0] != "staticmethod" {
		t.Errorf("staticmethod decorator missing: %+v", c.Methods[1])
	}
}

func TestParsePYIAliases(t *testing.T) {
	src := `Vector = List[float]
T = TypeVar('T')
StringMap = dict[str, str]
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Aliases) != 3 {
		t.Fatalf("aliases = %+v", m.Aliases)
	}
	if m.Aliases[0].Name != "Vector" || m.Aliases[0].Expr != "List[float]" {
		t.Errorf("Vector: %+v", m.Aliases[0])
	}
	if m.Aliases[1].Expr != "TypeVar('T')" {
		t.Errorf("T expr = %q", m.Aliases[1].Expr)
	}
}

func TestParsePYIConstants(t *testing.T) {
	src := `MAX_LEN: int
NAME: str = "hello"
PAIR: tuple[int, str] = (1, "x")
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Constants) != 3 {
		t.Fatalf("constants = %+v", m.Constants)
	}
	if m.Constants[0].Name != "MAX_LEN" || m.Constants[0].Type != "int" || m.Constants[0].Default != "" {
		t.Errorf("MAX_LEN: %+v", m.Constants[0])
	}
	if m.Constants[1].Default != `"hello"` {
		t.Errorf("NAME default = %q", m.Constants[1].Default)
	}
	if m.Constants[2].Type != "tuple[int, str]" {
		t.Errorf("PAIR type = %q", m.Constants[2].Type)
	}
}

func TestParsePYICommentStripping(t *testing.T) {
	src := `# top comment
def f() -> int: ...  # trailing
X: int = 0  # value comment
Y: str = "hash # inside"  # not a comment
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Functions) != 1 || m.Functions[0].Name != "f" {
		t.Errorf("funcs = %+v", m.Functions)
	}
	if len(m.Constants) != 2 {
		t.Fatalf("constants = %+v", m.Constants)
	}
	if !strings.Contains(m.Constants[1].Default, `"hash # inside"`) {
		t.Errorf("Y default = %q (should preserve # inside string)", m.Constants[1].Default)
	}
}

func TestParsePYIBackslashContinuation(t *testing.T) {
	src := `X: int = \
    42
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Constants) != 1 || m.Constants[0].Default != "42" {
		t.Errorf("constants = %+v", m.Constants)
	}
}

func TestParsePYIParenContinuation(t *testing.T) {
	src := `def f(
    a: int,
    b: int,
) -> int: ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Functions) != 1 || len(m.Functions[0].Params) != 2 {
		t.Errorf("funcs = %+v", m.Functions)
	}
}

func TestParsePYIUnbalancedBrackets(t *testing.T) {
	src := `def f()) -> int: ...`
	if _, err := ParsePYI(src); err == nil {
		t.Fatal("expected error for unbalanced brackets")
	}
}

func TestParsePYIBOM(t *testing.T) {
	src := "\ufeffimport os\n"
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Imports) != 1 || m.Imports[0].Names[0].Name != "os" {
		t.Errorf("imports = %+v", m.Imports)
	}
}

func TestParsePYIRejectsEqEqInAlias(t *testing.T) {
	src := `X = 1 == 2`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Aliases) != 1 {
		t.Fatalf("aliases = %+v", m.Aliases)
	}
	if m.Aliases[0].Expr != "1 == 2" {
		t.Errorf("expr = %q (should not split on ==)", m.Aliases[0].Expr)
	}
}

func TestParsePYIIgnoresUnknownTopLevel(t *testing.T) {
	// Unknown constructs should be silently skipped per doc.
	src := `pass
...
def known() -> int: ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Functions) != 1 {
		t.Errorf("funcs = %+v", m.Functions)
	}
}

func TestParsePYIIsPlainIdent(t *testing.T) {
	cases := []struct {
		in   string
		want bool
	}{
		{"x", true},
		{"X", true},
		{"_x", true},
		{"x1", true},
		{"x_y", true},
		{"", false},
		{"1x", false},
		{"x.y", false},
		{"x[y]", false},
		{"x, y", false},
	}
	for _, c := range cases {
		if got := isPlainIdent(c.in); got != c.want {
			t.Errorf("isPlainIdent(%q) = %v, want %v", c.in, got, c.want)
		}
	}
}

func TestParsePYIFindTopLevelColon(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"X: int", 1},
		{"X[Y]: int", 4},
		{`X: "str:thing"`, 1},
		{"no colon", -1},
		{"X[a:b]", -1},
		{"X{a:b}: int", 6},
	}
	for _, c := range cases {
		if got := findTopLevelColon(c.in); got != c.want {
			t.Errorf("findTopLevelColon(%q) = %d, want %d", c.in, got, c.want)
		}
	}
}

func TestParsePYIFindTopLevelEq(t *testing.T) {
	cases := []struct {
		in   string
		want int
	}{
		{"X = 1", 2},
		{"X == 1", -1},
		{"X >= 1", -1},
		{"X[a=b]", -1},
		{"X = (a == b)", 2},
	}
	for _, c := range cases {
		if got := findTopLevelEq(c.in); got != c.want {
			t.Errorf("findTopLevelEq(%q) = %d, want %d", c.in, got, c.want)
		}
	}
}

func TestParsePYISplitTopLevel(t *testing.T) {
	got := splitTopLevel("a, b, c", ',')
	if len(got) != 3 {
		t.Errorf("simple split: %v", got)
	}
	got = splitTopLevel("a, List[b, c], d", ',')
	if len(got) != 3 {
		t.Errorf("bracketed split: %v", got)
	}
	got = splitTopLevel(`a, "b, c", d`, ',')
	if len(got) != 3 {
		t.Errorf("quoted split: %v", got)
	}
}

func TestParsePYIDataclassRoundTrip(t *testing.T) {
	src := `from dataclasses import dataclass

@dataclass
class Point:
    x: int
    y: int = 0

    def shift(self, dx: int, dy: int = 0) -> "Point": ...
`
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	if len(m.Imports) != 1 {
		t.Errorf("imports = %+v", m.Imports)
	}
	if len(m.Classes) != 1 {
		t.Fatalf("classes = %+v", m.Classes)
	}
	p := m.Classes[0]
	if !p.IsDataclass {
		t.Error("Point should be dataclass")
	}
	if len(p.Fields) != 2 {
		t.Errorf("fields = %+v", p.Fields)
	}
	if len(p.Methods) != 1 || p.Methods[0].ReturnType != `"Point"` {
		t.Errorf("method return = %q", p.Methods[0].ReturnType)
	}
}

func TestParsePYITabsAreIndent(t *testing.T) {
	src := "class C:\n\tx: int\n\tdef m(self) -> int: ...\n"
	m, err := ParsePYI(src)
	if err != nil {
		t.Fatal(err)
	}
	c := m.Classes[0]
	if len(c.Fields) != 1 || len(c.Methods) != 1 {
		t.Errorf("class C: %+v", c)
	}
}
