package pypackage

import (
	"strings"
	"testing"

	"mochi/package3/python/typemap"
)

func intScalar() typemap.MochiType {
	return typemap.MochiType{Kind: typemap.KindScalar, Name: "int"}
}

func strScalar() typemap.MochiType {
	return typemap.MochiType{Kind: typemap.KindScalar, Name: "string"}
}

func boolScalar() typemap.MochiType {
	return typemap.MochiType{Kind: typemap.KindScalar, Name: "bool"}
}

func samplePackage() Package {
	return Package{
		Distribution:   "mochi-sample",
		Version:        "0.1.0",
		Summary:        "sample wrapper",
		License:        "Apache-2.0",
		Author:         "Mochi Team",
		HomePage:       "https://example.test",
		RequiresPython: ">=3.12,<3.15",
		Dependencies:   []string{"httpx>=0.27,<0.28"},
		Exports: []Export{
			{Name: "ping", Kind: ExportFunc, Type: typemap.MochiType{
				Kind:   typemap.KindFun,
				Params: []typemap.MochiType{strScalar(), boolScalar()},
			}},
			{Name: "VERSION", Kind: ExportConstant, Type: strScalar()},
		},
	}
}

func TestPyprojectTOMLContainsCoreFields(t *testing.T) {
	src := PyprojectTOML(samplePackage())
	for _, s := range []string{
		`build-backend = "mochi_build"`,
		`name = "mochi-sample"`,
		`version = "0.1.0"`,
		`description = "sample wrapper"`,
		`requires-python = ">=3.12,<3.15"`,
		`license = { text = "Apache-2.0" }`,
		`urls = { Homepage = "https://example.test" }`,
		`authors = [{ name = "Mochi Team" }]`,
		`"httpx>=0.27,<0.28"`,
	} {
		if !strings.Contains(src, s) {
			t.Fatalf("pyproject.toml missing %q\n%s", s, src)
		}
	}
}

func TestPyprojectTOMLOmitsBlanks(t *testing.T) {
	p := samplePackage()
	p.HomePage = ""
	p.Author = ""
	p.RequiresPython = ""
	p.Dependencies = nil
	src := PyprojectTOML(p)
	for _, s := range []string{"Homepage", "authors", "requires-python", "dependencies"} {
		if strings.Contains(src, s) {
			t.Fatalf("pyproject.toml should omit %q\n%s", s, src)
		}
	}
}

func TestPKGInfoHeaders(t *testing.T) {
	src := PKGInfo(samplePackage())
	for _, s := range []string{
		"Metadata-Version: 2.1",
		"Name: mochi-sample",
		"Version: 0.1.0",
		"Summary: sample wrapper",
		"Home-page: https://example.test",
		"Author: Mochi Team",
		"License: Apache-2.0",
		"Requires-Python: >=3.12,<3.15",
		"Requires-Dist: httpx>=0.27,<0.28",
	} {
		if !strings.Contains(src, s) {
			t.Fatalf("PKG-INFO missing %q\n%s", s, src)
		}
	}
}

func TestWheelMetadataPureLib(t *testing.T) {
	src := WheelMetadata()
	for _, s := range []string{"Wheel-Version: 1.0", "Tag: py3-none-any", "Root-Is-Purelib: true"} {
		if !strings.Contains(src, s) {
			t.Fatalf("WHEEL missing %q\n%s", s, src)
		}
	}
}

func TestInitPyExportsList(t *testing.T) {
	src := InitPy(samplePackage())
	if !strings.Contains(src, `from .mochi_runtime import`) {
		t.Fatalf("__init__.py missing re-export\n%s", src)
	}
	if !strings.Contains(src, `"VERSION"`) || !strings.Contains(src, `"ping"`) {
		t.Fatalf("__init__.py __all__ missing exports\n%s", src)
	}
}

func TestInitPYIRendersFuncAndConstant(t *testing.T) {
	src := InitPYI(samplePackage())
	if !strings.Contains(src, `def ping(arg0: str) -> bool: ...`) {
		t.Fatalf("missing func sig\n%s", src)
	}
	if !strings.Contains(src, `VERSION: str`) {
		t.Fatalf("missing constant\n%s", src)
	}
}

func TestInitPYIRendersRecord(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{
		Name: "Point",
		Kind: ExportRecord,
		Type: typemap.MochiType{Kind: typemap.KindRecord, Fields: []typemap.MochiField{
			{Name: "x", Type: intScalar()},
			{Name: "y", Type: intScalar()},
		}},
	}}
	src := InitPYI(p)
	if !strings.Contains(src, "class Point:") {
		t.Fatalf("missing class header\n%s", src)
	}
	if !strings.Contains(src, "    x: int") || !strings.Contains(src, "    y: int") {
		t.Fatalf("missing fields\n%s", src)
	}
}

func TestInitPYIEmptyRecord(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{Name: "Empty", Kind: ExportRecord, Type: typemap.MochiType{Kind: typemap.KindRecord}}}
	src := InitPYI(p)
	if !strings.Contains(src, "class Empty:\n    ...\n") {
		t.Fatalf("expected empty class body\n%s", src)
	}
}

func TestInitPYIRendersInterface(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{
		Name: "Greeter",
		Kind: ExportInterface,
		Type: typemap.MochiType{Kind: typemap.KindInterface, Methods: []typemap.MochiMethod{
			{Name: "say", Params: []typemap.MochiType{strScalar()}, Return: strScalar()},
			{Name: "stream", IsAsync: true, Params: nil, Return: strScalar()},
		}},
	}}
	src := InitPYI(p)
	if !strings.Contains(src, "class Greeter(Protocol):") {
		t.Fatalf("missing protocol header\n%s", src)
	}
	if !strings.Contains(src, "    def say(self, arg0: str) -> str: ...") {
		t.Fatalf("missing sync method\n%s", src)
	}
	if !strings.Contains(src, "    async def stream(self) -> str: ...") {
		t.Fatalf("missing async method\n%s", src)
	}
	if !strings.Contains(src, "from typing import") || !strings.Contains(src, "Protocol") {
		t.Fatalf("missing Protocol import\n%s", src)
	}
}

func TestInitPYIEmptyInterface(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{Name: "Empty", Kind: ExportInterface, Type: typemap.MochiType{Kind: typemap.KindInterface}}}
	src := InitPYI(p)
	if !strings.Contains(src, "class Empty(Protocol):\n    ...\n") {
		t.Fatalf("expected empty protocol body\n%s", src)
	}
}

func TestPyAnnotationScalars(t *testing.T) {
	for mochi, py := range map[string]string{
		"int":    "int",
		"float":  "float",
		"bool":   "bool",
		"string": "str",
		"bytes":  "bytes",
		"None":   "None",
		"Any":    "Any",
	} {
		got := pyAnnotation(typemap.MochiType{Kind: typemap.KindScalar, Name: mochi})
		if got != py {
			t.Fatalf("scalar %q: got %q, want %q", mochi, got, py)
		}
	}
}

func TestPyAnnotationContainers(t *testing.T) {
	cases := []struct {
		t    typemap.MochiType
		want string
	}{
		{typemap.MochiType{Kind: typemap.KindList, Params: []typemap.MochiType{intScalar()}}, "list[int]"},
		{typemap.MochiType{Kind: typemap.KindSet, Params: []typemap.MochiType{intScalar()}}, "set[int]"},
		{typemap.MochiType{Kind: typemap.KindMap, Params: []typemap.MochiType{strScalar(), intScalar()}}, "dict[str, int]"},
		{typemap.MochiType{Kind: typemap.KindOptional, Params: []typemap.MochiType{intScalar()}}, "Optional[int]"},
		{typemap.MochiType{Kind: typemap.KindSum, Params: []typemap.MochiType{intScalar(), strScalar()}}, "Union[int, str]"},
		{typemap.MochiType{Kind: typemap.KindTuple, Params: []typemap.MochiType{intScalar(), strScalar()}}, "tuple[int, str]"},
		{typemap.MochiType{Kind: typemap.KindAsync, Params: []typemap.MochiType{intScalar()}}, "Awaitable[int]"},
		{typemap.MochiType{Kind: typemap.KindStream, Params: []typemap.MochiType{intScalar()}}, "AsyncIterator[int]"},
		{typemap.MochiType{Kind: typemap.KindFun, Params: []typemap.MochiType{strScalar(), boolScalar()}}, "Callable[[str], bool]"},
		{typemap.MochiType{Kind: typemap.KindRecord, Name: "Point"}, "Point"},
		{typemap.MochiType{Kind: typemap.KindRecord, Name: ""}, "Any"},
		{typemap.MochiType{Kind: typemap.KindInterface, Name: "Greeter"}, "Greeter"},
		{typemap.MochiType{Kind: typemap.KindInterface, Name: ""}, "Any"},
		{typemap.MochiType{Kind: typemap.KindRef, Name: "Custom"}, "Custom"},
		{typemap.MochiType{Kind: typemap.KindTypeVar, Name: "T"}, "T"},
		{typemap.MochiType{Kind: typemap.Kind(99)}, "Any"},
	}
	for _, c := range cases {
		if got := pyAnnotation(c.t); got != c.want {
			t.Fatalf("pyAnnotation(%+v) = %q, want %q", c.t, got, c.want)
		}
	}
}

func TestPyImportsForOptionalUnionStream(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{
		Name: "fn",
		Kind: ExportFunc,
		Type: typemap.MochiType{
			Kind: typemap.KindFun,
			Params: []typemap.MochiType{
				{Kind: typemap.KindOptional, Params: []typemap.MochiType{intScalar()}},
				{Kind: typemap.KindStream, Params: []typemap.MochiType{strScalar()}},
			},
		},
	}}
	imps := pyImportsFor(p)
	if len(imps) != 1 {
		t.Fatalf("imports = %v", imps)
	}
	line := imps[0]
	for _, want := range []string{"Optional", "AsyncIterator", "Callable"} {
		if !strings.Contains(line, want) {
			t.Fatalf("imports missing %q: %s", want, line)
		}
	}
}

func TestPyImportsForRecordWalksFields(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{
		Name: "R",
		Kind: ExportRecord,
		Type: typemap.MochiType{Kind: typemap.KindRecord, Fields: []typemap.MochiField{
			{Name: "v", Type: typemap.MochiType{Kind: typemap.KindOptional, Params: []typemap.MochiType{intScalar()}}},
		}},
	}}
	imps := pyImportsFor(p)
	if len(imps) != 1 || !strings.Contains(imps[0], "Optional") {
		t.Fatalf("expected Optional import via record field, got %v", imps)
	}
}

func TestPyImportsForNoneEmpty(t *testing.T) {
	p := samplePackage()
	p.Exports = []Export{{Name: "k", Kind: ExportConstant, Type: intScalar()}}
	if imps := pyImportsFor(p); imps != nil {
		t.Fatalf("expected nil imports, got %v", imps)
	}
}

func TestMochiBuildBackendNonEmpty(t *testing.T) {
	src := MochiBuildBackend()
	for _, want := range []string{"build_wheel", "build_sdist", "mochi pkg build", "prepare_metadata_for_build_wheel"} {
		if !strings.Contains(src, want) {
			t.Fatalf("backend missing %q", want)
		}
	}
}

func TestRecordLineEmpty(t *testing.T) {
	got := RecordLine(RecordEntry{Path: "RECORD"})
	if got != "RECORD,," {
		t.Fatalf("got %q", got)
	}
}

func TestRecordLineHashed(t *testing.T) {
	got := RecordLine(RecordEntry{Path: "foo.py", Hash: "abc", Size: 42})
	if got != "foo.py,sha256=abc,42" {
		t.Fatalf("got %q", got)
	}
}

func TestHashBodyDeterministic(t *testing.T) {
	a := HashBody("hello")
	b := HashBody("hello")
	if a != b {
		t.Fatalf("hash not stable: %q vs %q", a, b)
	}
	if strings.HasSuffix(a, "=") {
		t.Fatalf("hash should be unpadded: %q", a)
	}
	if strings.Contains(a, "+") || strings.Contains(a, "/") {
		t.Fatalf("hash should be urlsafe: %q", a)
	}
}
