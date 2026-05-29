package library

import (
	"errors"
	"strings"
	"testing"
)

func smallAPI() PublicAPI {
	return PublicAPI{
		ModulePath:  "github.com/mochilang/demo",
		PackageName: "demo",
		Version:     "v0.1.0",
		Items: []Item{
			ItemFunc{
				Name: "Add", Doc: "Add returns a+b.",
				Params:  []Param{{"a", "int64"}, {"b", "int64"}},
				Results: []Result{{Type: "int64"}},
				Body:    "return a + b",
			},
		},
	}
}

func TestEmitBaseline(t *testing.T) {
	res, err := Emit(smallAPI())
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	wantFiles := []string{"go.mod", "doc.go", "demo.go"}
	for _, w := range wantFiles {
		if _, ok := res.Files[w]; !ok {
			t.Errorf("Emit missing %s; got %v", w, res.Names())
		}
	}
	if !strings.Contains(res.Files["go.mod"], "module github.com/mochilang/demo") {
		t.Errorf("go.mod missing module line: %s", res.Files["go.mod"])
	}
	if !strings.Contains(res.Files["go.mod"], "go 1.21") {
		t.Errorf("go.mod missing go directive: %s", res.Files["go.mod"])
	}
	if !strings.Contains(res.Files["doc.go"], "package demo") {
		t.Errorf("doc.go missing package clause: %s", res.Files["doc.go"])
	}
	if !strings.Contains(res.Files["doc.go"], "// Package demo") {
		t.Errorf("doc.go missing first-sentence godoc: %s", res.Files["doc.go"])
	}
	if !strings.Contains(res.Files["demo.go"], "func Add(a int64, b int64) int64") {
		t.Errorf("demo.go signature wrong:\n%s", res.Files["demo.go"])
	}
}

func TestEmitRejectsMissingModulePath(t *testing.T) {
	api := smallAPI()
	api.ModulePath = ""
	if _, err := Emit(api); err == nil || !errors.Is(err, ErrLibrary) {
		t.Errorf("expected ErrLibrary, got %v", err)
	}
}

func TestEmitRejectsBadModulePath(t *testing.T) {
	api := smallAPI()
	api.ModulePath = "/bad/path"
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for bad module path")
	}
	api.ModulePath = "no-dots-here"
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for module path missing dot in first segment")
	}
}

func TestEmitRejectsMissingPackageName(t *testing.T) {
	api := smallAPI()
	api.PackageName = ""
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for empty package name")
	}
}

func TestEmitRejectsBadPackageName(t *testing.T) {
	api := smallAPI()
	api.PackageName = "1invalid"
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for bad package name")
	}
	api.PackageName = "type" // reserved
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for keyword package name")
	}
}

func TestEmitRejectsMissingVersion(t *testing.T) {
	api := smallAPI()
	api.Version = ""
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for empty version")
	}
}

func TestEmitRejectsUnexportedFunc(t *testing.T) {
	api := smallAPI()
	api.Items = []Item{ItemFunc{Name: "add", Body: ""}}
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for unexported func name")
	}
}

func TestEmitRejectsUnknownTypeKind(t *testing.T) {
	api := smallAPI()
	api.Items = []Item{ItemType{Name: "X", Kind: "weird"}}
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for unknown ItemType.Kind")
	}
}

func TestEmitRejectsAliasMissingTarget(t *testing.T) {
	api := smallAPI()
	api.Items = []Item{ItemType{Name: "Foo", Kind: "alias"}}
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for alias missing Target")
	}
}

func TestEmitRejectsConstMissingValue(t *testing.T) {
	api := smallAPI()
	api.Items = []Item{ItemConst{Name: "Pi", Type: "float64"}}
	if _, err := Emit(api); err == nil {
		t.Errorf("expected error for const missing value")
	}
}

func TestEmitDeterministicAcrossPermutations(t *testing.T) {
	api := PublicAPI{
		ModulePath: "github.com/x/y", PackageName: "y", Version: "v0.0.1",
		Items: []Item{
			ItemFunc{Name: "B", Body: ""},
			ItemFunc{Name: "A", Body: ""},
			ItemConst{Name: "Z", Type: "int", Value: "1"},
			ItemConst{Name: "Y", Type: "int", Value: "2"},
			ItemType{Name: "Tb", Kind: "definition", Target: "int"},
			ItemType{Name: "Ta", Kind: "definition", Target: "int"},
			ItemVar{Name: "Vb", Type: "int", Value: "0"},
			ItemVar{Name: "Va", Type: "int", Value: "0"},
		},
	}
	first, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	api.Items = []Item{
		ItemVar{Name: "Va", Type: "int", Value: "0"},
		ItemFunc{Name: "B", Body: ""},
		ItemType{Name: "Tb", Kind: "definition", Target: "int"},
		ItemConst{Name: "Y", Type: "int", Value: "2"},
		ItemFunc{Name: "A", Body: ""},
		ItemVar{Name: "Vb", Type: "int", Value: "0"},
		ItemConst{Name: "Z", Type: "int", Value: "1"},
		ItemType{Name: "Ta", Kind: "definition", Target: "int"},
	}
	second, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if first.Files["y.go"] != second.Files["y.go"] {
		t.Errorf("permutation changed output\n--- first ---\n%s\n--- second ---\n%s",
			first.Files["y.go"], second.Files["y.go"])
	}
}

func TestEmitDeterministicAcrossRuns(t *testing.T) {
	api := smallAPI()
	first, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	for i := 0; i < 8; i++ {
		got, err := Emit(api)
		if err != nil {
			t.Fatalf("iter %d: %v", i, err)
		}
		for _, name := range first.Names() {
			if first.Files[name] != got.Files[name] {
				t.Errorf("iter %d: %s drifted", i, name)
			}
		}
	}
}

func TestSortedItemsGroupsByKind(t *testing.T) {
	items := []Item{
		ItemFunc{Name: "F"},
		ItemConst{Name: "C", Value: "1"},
		ItemVar{Name: "V"},
		ItemType{Name: "T", Kind: "definition", Target: "int"},
	}
	got := sortedItems(items)
	wantOrder := []string{"C", "V", "T", "F"}
	for i, want := range wantOrder {
		if got[i].itemName() != want {
			t.Errorf("position %d: got %q want %q", i, got[i].itemName(), want)
		}
	}
}

func TestRenderGoModWithDeps(t *testing.T) {
	got := renderGoMod("example.com/m", "1.22", map[string]string{
		"b/x": "v1.2.3",
		"a/y": "v0.5.0",
	})
	if !strings.Contains(got, "require (") {
		t.Errorf("require block missing: %s", got)
	}
	if idxA := strings.Index(got, "a/y"); idxA == -1 {
		t.Errorf("a/y missing")
	} else if idxB := strings.Index(got, "b/x"); idxB == -1 || idxB < idxA {
		t.Errorf("deps not sorted: %s", got)
	}
}

func TestRenderGoModOmitsEmptyRequire(t *testing.T) {
	got := renderGoMod("example.com/m", "1.21", nil)
	if strings.Contains(got, "require") {
		t.Errorf("require block should be absent: %s", got)
	}
}

func TestRenderDocGoSynthesisesStub(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
	}
	got := renderDocGo(api)
	if !strings.HasPrefix(got, "// Package m is generated by Mochi MEP-74.") {
		t.Errorf("doc.go stub missing: %s", got)
	}
}

func TestRenderDocGoIncludesRepoAndVersion(t *testing.T) {
	api := smallAPI()
	api.Meta.DocComment = "Package demo does math."
	api.Meta.Repository = "https://github.com/mochilang/demo"
	got := renderDocGo(api)
	if !strings.Contains(got, "// Source: https://github.com/mochilang/demo") {
		t.Errorf("doc.go missing repository line: %s", got)
	}
	if !strings.Contains(got, "// Version: v0.1.0") {
		t.Errorf("doc.go missing version line: %s", got)
	}
}

func TestRenderSourceStructWithFields(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemType{
				Name: "Point", Kind: "struct",
				Doc: "Point is a 2D coordinate.",
				Fields: []Field{
					{Name: "X", Type: "float64", Doc: "X is the abscissa.", Tag: `json:"x"`},
					{Name: "Y", Type: "float64", Tag: `json:"y"`},
				},
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "type Point struct {") {
		t.Errorf("struct header missing: %s", src)
	}
	if !strings.Contains(src, "X float64 `json:\"x\"`") {
		t.Errorf("field rendering wrong: %s", src)
	}
	if !strings.Contains(src, "// X is the abscissa.") {
		t.Errorf("field godoc missing: %s", src)
	}
}

func TestRenderSourceInterfaceMethods(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemType{
				Name: "Reader", Kind: "interface",
				Methods: []Method{
					{Name: "Read", Params: []Param{{"p", "[]byte"}}, Results: []Result{{Type: "int"}, {Type: "error"}}},
				},
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "type Reader interface {") {
		t.Errorf("interface header missing: %s", src)
	}
	if !strings.Contains(src, "Read(p []byte) (int, error)") {
		t.Errorf("method signature wrong: %s", src)
	}
}

func TestRenderSourceAlias(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemType{Name: "ID", Kind: "alias", Target: "int64"},
			ItemType{Name: "Status", Kind: "definition", Target: "string"},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "type ID = int64") {
		t.Errorf("alias rendering wrong: %s", src)
	}
	if !strings.Contains(src, "type Status string") {
		t.Errorf("definition rendering wrong: %s", src)
	}
}

func TestRenderSourceConstAndVar(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemConst{Name: "Pi", Type: "float64", Value: "3.14159"},
			ItemVar{Name: "Counter", Type: "int", Value: "0"},
			ItemVar{Name: "DefaultName", Type: "string"},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "const Pi float64 = 3.14159") {
		t.Errorf("const rendering wrong: %s", src)
	}
	if !strings.Contains(src, "var Counter int = 0") {
		t.Errorf("var with value rendering wrong: %s", src)
	}
	if !strings.Contains(src, "var DefaultName string\n") {
		t.Errorf("var no-value rendering wrong: %s", src)
	}
}

func TestRenderSourceFuncEmptyBody(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemFunc{Name: "Noop"},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "func Noop() {}\n") {
		t.Errorf("empty body func wrong:\n%s", src)
	}
}

func TestRenderSourceFuncMultiResult(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemFunc{
				Name:    "DivMod",
				Params:  []Param{{"a", "int"}, {"b", "int"}},
				Results: []Result{{Type: "int"}, {Type: "int"}},
				Body:    "return a / b, a % b",
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, "func DivMod(a int, b int) (int, int)") {
		t.Errorf("multi-result signature wrong: %s", src)
	}
}

func TestRenderSourceExternEmitsCgoImport(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemFunc{
				Name: "ExtAdd", Extern: true,
				Params:  []Param{{"a", "int64"}, {"b", "int64"}},
				Results: []Result{{Type: "int64"}},
				Body:    "return a + b",
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	src := res.Files["m.go"]
	if !strings.Contains(src, `import "C"`) {
		t.Errorf("cgo import missing for Extern: %s", src)
	}
	if !strings.Contains(src, "//export ExtAdd") {
		t.Errorf("//export directive missing: %s", src)
	}
}

func TestRenderCHeaderDeclares(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		CHeader:    true,
		Items: []Item{
			ItemFunc{Name: "Add", Extern: true,
				Params:  []Param{{"a", "int64"}, {"b", "int64"}},
				Results: []Result{{Type: "int64"}},
				Body:    "return a + b",
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	hdr := res.Files["_cgo_export.h"]
	for _, want := range []string{
		"#ifndef MOCHI_M_H",
		"#include <stdint.h>",
		`extern "C" {`,
		"extern int64_t Add(int64_t a, int64_t b);",
		"#endif /* MOCHI_M_H */",
	} {
		if !strings.Contains(hdr, want) {
			t.Errorf("header missing %q\n--- header ---\n%s", want, hdr)
		}
	}
}

func TestRenderCHeaderSkipsNonPrimitive(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		CHeader:    true,
		Items: []Item{
			ItemFunc{Name: "Join", Extern: true,
				Params:  []Param{{"parts", "[]string"}},
				Results: []Result{{Type: "string"}},
				Body:    "return parts[0]",
			},
		},
	}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	hdr := res.Files["_cgo_export.h"]
	if !strings.Contains(hdr, "/* skipped Join: non-primitive") {
		t.Errorf("expected skip comment for non-primitive: %s", hdr)
	}
}

func TestRenderCHeaderOmittedWhenDisabled(t *testing.T) {
	api := smallAPI()
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	if _, ok := res.Files["_cgo_export.h"]; ok {
		t.Errorf("header should be absent when CHeader=false")
	}
}

func TestEmitReadmeScaffold(t *testing.T) {
	api := smallAPI()
	api.Meta.Description = "A demo math package."
	api.Meta.License = "MIT"
	api.Meta.Authors = []string{"Beta <b@x>", "Alpha <a@x>"}
	res, err := Emit(api)
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}
	rm, ok := res.Files["README.md"]
	if !ok {
		t.Fatalf("README scaffold missing")
	}
	if !strings.Contains(rm, "# demo") {
		t.Errorf("README header missing: %s", rm)
	}
	if !strings.Contains(rm, "A demo math package.") {
		t.Errorf("README description missing: %s", rm)
	}
	if !strings.Contains(rm, "go get github.com/mochilang/demo@v0.1.0") {
		t.Errorf("README install line missing: %s", rm)
	}
	// Authors must be sorted.
	if idxA := strings.Index(rm, "Alpha"); idxA == -1 {
		t.Errorf("Alpha missing")
	} else if idxB := strings.Index(rm, "Beta"); idxB == -1 || idxB < idxA {
		t.Errorf("authors not sorted: %s", rm)
	}
}

func TestEmitReadmeVerbatim(t *testing.T) {
	api := smallAPI()
	api.Meta.Readme = "# custom\n\nhand-written.\n"
	res, _ := Emit(api)
	if res.Files["README.md"] != "# custom\n\nhand-written.\n" {
		t.Errorf("verbatim README clobbered: %s", res.Files["README.md"])
	}
}

func TestEmitLicensePassthrough(t *testing.T) {
	api := smallAPI()
	api.Meta.LicenseText = "MIT licence here.\n"
	res, _ := Emit(api)
	if res.Files["LICENSE"] != "MIT licence here.\n" {
		t.Errorf("LICENSE passthrough wrong")
	}
}

func TestPascalCase(t *testing.T) {
	tests := []struct{ in, want string }{
		{"", ""},
		{"x", "X"},
		{"hello_world", "HelloWorld"},
		{"n_iter_max", "NIterMax"},
		{"_leading", "Leading"},
		{"trailing_", "Trailing"},
		{"AlreadyExported", "AlreadyExported"},
	}
	for _, tc := range tests {
		if got := PascalCase(tc.in); got != tc.want {
			t.Errorf("PascalCase(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestIsGoIdentifier(t *testing.T) {
	good := []string{"x", "Foo", "_bar", "a1", "café"}
	for _, s := range good {
		if !isGoIdentifier(s) {
			t.Errorf("isGoIdentifier(%q) = false; want true", s)
		}
	}
	bad := []string{"", "1bad", "x-y", "type", "var"}
	for _, s := range bad {
		if isGoIdentifier(s) {
			t.Errorf("isGoIdentifier(%q) = true; want false", s)
		}
	}
}

func TestIsModulePath(t *testing.T) {
	good := []string{
		"github.com/x/y",
		"example.com/foo",
		"k8s.io/api",
		"gopkg.in/yaml.v3",
	}
	for _, s := range good {
		if !isModulePath(s) {
			t.Errorf("isModulePath(%q) = false; want true", s)
		}
	}
	bad := []string{
		"",
		"localpkg",
		"/leading",
		"trailing/",
		"foo/../bar",
		"with space/x",
	}
	for _, s := range bad {
		if isModulePath(s) {
			t.Errorf("isModulePath(%q) = true; want false", s)
		}
	}
}

func TestEmitResultNamesSorted(t *testing.T) {
	api := smallAPI()
	api.Meta.LicenseText = "MIT"
	api.Meta.Description = "test"
	api.CHeader = true
	res, _ := Emit(api)
	names := res.Names()
	want := []string{"LICENSE", "README.md", "_cgo_export.h", "demo.go", "doc.go", "go.mod"}
	if len(names) != len(want) {
		t.Fatalf("got %d files, want %d (%v vs %v)", len(names), len(want), names, want)
	}
	for i, w := range want {
		if names[i] != w {
			t.Errorf("position %d: got %q want %q", i, names[i], w)
		}
	}
}

func TestEmitGoVersionOverride(t *testing.T) {
	api := smallAPI()
	api.GoVersion = "1.23"
	res, _ := Emit(api)
	if !strings.Contains(res.Files["go.mod"], "go 1.23") {
		t.Errorf("custom go version not propagated: %s", res.Files["go.mod"])
	}
}

func TestRenderFuncBodyIndents(t *testing.T) {
	api := PublicAPI{
		ModulePath: "example.com/m", PackageName: "m", Version: "v0.1.0",
		Items: []Item{
			ItemFunc{
				Name: "Sum", Params: []Param{{"xs", "[]int"}},
				Results: []Result{{Type: "int"}},
				Body:    "total := 0\nfor _, x := range xs {\n\ttotal += x\n}\nreturn total",
			},
		},
	}
	res, _ := Emit(api)
	src := res.Files["m.go"]
	if !strings.Contains(src, "\ttotal := 0\n") {
		t.Errorf("body not indented: %s", src)
	}
}

func TestEmitDependencyPropagatesToGoMod(t *testing.T) {
	api := smallAPI()
	api.Dependencies = map[string]string{
		"github.com/google/uuid": "v1.6.0",
	}
	res, _ := Emit(api)
	if !strings.Contains(res.Files["go.mod"], "github.com/google/uuid v1.6.0") {
		t.Errorf("dependency missing from go.mod: %s", res.Files["go.mod"])
	}
}
