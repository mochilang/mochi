package apisurface

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/packages"
)

// loadFixture writes a tiny Go module to a temp dir, runs go/packages
// against it, and returns the loaded packages. Each fixture has a
// "go.mod" with module path "fixture.test/x" so packages can resolve
// without external network access.
func loadFixture(t *testing.T, files map[string]string) []*packages.Package {
	t.Helper()
	dir := t.TempDir()
	if _, ok := files["go.mod"]; !ok {
		files["go.mod"] = "module fixture.test/x\n\ngo 1.21\n"
	}
	for name, body := range files {
		full := filepath.Join(dir, name)
		if err := os.MkdirAll(filepath.Dir(full), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(full, []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedCompiledGoFiles |
			packages.NeedImports | packages.NeedTypes | packages.NeedSyntax |
			packages.NeedTypesInfo | packages.NeedDeps | packages.NeedModule,
		Dir:   dir,
		Tests: false,
	}
	pkgs, err := packages.Load(cfg, "./...")
	if err != nil {
		t.Fatalf("packages.Load: %v", err)
	}
	for _, p := range pkgs {
		for _, e := range p.Errors {
			t.Fatalf("package %s error: %v", p.PkgPath, e)
		}
	}
	return pkgs
}

func TestIngestRequiresModule(t *testing.T) {
	if _, err := Ingest(nil, IngestOptions{}); err == nil {
		t.Errorf("Ingest with empty Module: want error")
	}
}

func TestIngestFuncsAndSkipped(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"foo.go": `// Package foo is a fixture.
package foo

// Hello returns a greeting.
func Hello(name string) string { return "hi " + name }

// internal is unexported and should be Skipped.
func internal() {}

// Multi returns two values; the second is variadic.
func Multi(a int, b ...string) (int, error) { return a, nil }
`,
	})
	f, err := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	if len(f.Packages) != 1 {
		t.Fatalf("Packages len = %d, want 1", len(f.Packages))
	}
	pkg := f.Packages[0]
	if pkg.Doc != "Package foo is a fixture." {
		t.Errorf("Doc = %q", pkg.Doc)
	}
	if len(pkg.Funcs) != 2 {
		t.Fatalf("Funcs len = %d, want 2 (Hello, Multi)", len(pkg.Funcs))
	}
	var hello, multi *Func
	for i := range pkg.Funcs {
		switch pkg.Funcs[i].Name {
		case "Hello":
			hello = &pkg.Funcs[i]
		case "Multi":
			multi = &pkg.Funcs[i]
		}
	}
	if hello == nil || multi == nil {
		t.Fatalf("missing Hello or Multi: %+v", pkg.Funcs)
	}
	if hello.Doc != "Hello returns a greeting." {
		t.Errorf("Hello.Doc = %q", hello.Doc)
	}
	if len(hello.Params) != 1 || hello.Params[0].Type != "string" {
		t.Errorf("Hello.Params = %+v", hello.Params)
	}
	if len(hello.Results) != 1 || hello.Results[0].Type != "string" {
		t.Errorf("Hello.Results = %+v", hello.Results)
	}
	if !multi.Variadic {
		t.Errorf("Multi.Variadic = false; want true")
	}
	if len(multi.Results) != 2 || multi.Results[1].Type != "error" {
		t.Errorf("Multi.Results = %+v", multi.Results)
	}
	foundSkipped := false
	for _, s := range pkg.Skipped {
		if s.Name == "internal" && s.Reason == "Unexported" && s.Kind == "func" {
			foundSkipped = true
		}
	}
	if !foundSkipped {
		t.Errorf("unexported func 'internal' not in Skipped: %+v", pkg.Skipped)
	}
}

func TestIngestStruct(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"types.go": `package foo

// Greeter is a polite greeter.
type Greeter struct {
	// Name is who to greet.
	Name string ` + "`json:\"name\"`" + `
	hidden int
}

// Hello greets.
func (g *Greeter) Hello() string { return "hi " + g.Name }

// Reset resets the greeter (value receiver).
func (g Greeter) Reset() {}
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	if len(pkg.Types) != 1 {
		t.Fatalf("Types len = %d, want 1", len(pkg.Types))
	}
	g := pkg.Types[0]
	if g.Name != "Greeter" || g.Kind != KindStruct {
		t.Errorf("Greeter: name=%q kind=%q", g.Name, g.Kind)
	}
	if g.Doc != "Greeter is a polite greeter." {
		t.Errorf("Greeter.Doc = %q", g.Doc)
	}
	if len(g.Fields) != 2 {
		t.Fatalf("Greeter.Fields len = %d, want 2", len(g.Fields))
	}
	var nameField *Field
	for i := range g.Fields {
		if g.Fields[i].Name == "Name" {
			nameField = &g.Fields[i]
		}
	}
	if nameField == nil {
		t.Fatalf("no Name field")
	}
	if !nameField.Exported {
		t.Errorf("Name.Exported = false")
	}
	if !strings.Contains(nameField.Tag, "json:") {
		t.Errorf("Name.Tag = %q", nameField.Tag)
	}
	if len(g.Methods) != 2 {
		t.Fatalf("Greeter.Methods len = %d, want 2", len(g.Methods))
	}
	var hello, reset *Func
	for i := range g.Methods {
		switch g.Methods[i].Name {
		case "Hello":
			hello = &g.Methods[i]
		case "Reset":
			reset = &g.Methods[i]
		}
	}
	if hello == nil || reset == nil {
		t.Fatalf("missing methods")
	}
	if hello.Receiver != "Greeter" || !hello.ReceiverPointer {
		t.Errorf("Hello receiver = %q ptr=%v", hello.Receiver, hello.ReceiverPointer)
	}
	if reset.Receiver != "Greeter" || reset.ReceiverPointer {
		t.Errorf("Reset receiver = %q ptr=%v", reset.Receiver, reset.ReceiverPointer)
	}
}

func TestIngestInterface(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"iface.go": `package foo

// Reader reads bytes.
type Reader interface {
	Read(p []byte) (int, error)
}

// ReadCloser composes Reader and Closer.
type ReadCloser interface {
	Reader
	Close() error
}
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	if len(pkg.Types) != 2 {
		t.Fatalf("Types len = %d, want 2", len(pkg.Types))
	}
	var reader, rc *Type
	for i := range pkg.Types {
		switch pkg.Types[i].Name {
		case "Reader":
			reader = &pkg.Types[i]
		case "ReadCloser":
			rc = &pkg.Types[i]
		}
	}
	if reader == nil || rc == nil {
		t.Fatalf("missing interfaces")
	}
	if reader.Kind != KindInterface {
		t.Errorf("Reader.Kind = %q", reader.Kind)
	}
	if len(reader.InterfaceMethods) != 1 || reader.InterfaceMethods[0].Name != "Read" {
		t.Errorf("Reader.InterfaceMethods = %+v", reader.InterfaceMethods)
	}
	if len(rc.EmbeddedTypes) != 1 || !strings.HasSuffix(rc.EmbeddedTypes[0], "Reader") {
		t.Errorf("ReadCloser.EmbeddedTypes = %+v", rc.EmbeddedTypes)
	}
}

func TestIngestConstsVars(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"vals.go": `package foo

// Version is the schema version.
const Version = "1.0.0"

// Defaults are the default settings.
var Defaults = []string{"a", "b"}
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	if len(pkg.Consts) != 1 || pkg.Consts[0].Name != "Version" {
		t.Fatalf("Consts = %+v", pkg.Consts)
	}
	if pkg.Consts[0].Doc != "Version is the schema version." {
		t.Errorf("Version.Doc = %q", pkg.Consts[0].Doc)
	}
	if !strings.Contains(pkg.Consts[0].Const, "1.0.0") {
		t.Errorf("Version.Const = %q", pkg.Consts[0].Const)
	}
	if len(pkg.Vars) != 1 || pkg.Vars[0].Name != "Defaults" {
		t.Errorf("Vars = %+v", pkg.Vars)
	}
}

func TestIngestAlias(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"alias.go": `package foo

// MyString is an alias for string.
type MyString = string
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	if len(pkg.Types) != 1 {
		t.Fatalf("Types len = %d", len(pkg.Types))
	}
	a := pkg.Types[0]
	if a.Kind != KindAlias {
		t.Errorf("Kind = %q, want alias", a.Kind)
	}
	if a.AliasOf != "string" {
		t.Errorf("AliasOf = %q", a.AliasOf)
	}
}

func TestIngestGenericType(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"gen.go": `package foo

// Box holds a value of any type.
type Box[T any] struct {
	V T
}

// Take returns the held value.
func (b *Box[T]) Take() T { return b.V }

// MapKeys returns keys.
func MapKeys[K comparable, V any](m map[K]V) []K {
	r := make([]K, 0, len(m))
	for k := range m { r = append(r, k) }
	return r
}
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	var box *Type
	for i := range pkg.Types {
		if pkg.Types[i].Name == "Box" {
			box = &pkg.Types[i]
		}
	}
	if box == nil {
		t.Fatalf("Box type missing")
	}
	if len(box.TypeParams) != 1 || box.TypeParams[0].Name != "T" {
		t.Errorf("Box.TypeParams = %+v", box.TypeParams)
	}
	if len(box.Methods) != 1 || box.Methods[0].Name != "Take" {
		t.Errorf("Box.Methods = %+v", box.Methods)
	}
	var mk *Func
	for i := range pkg.Funcs {
		if pkg.Funcs[i].Name == "MapKeys" {
			mk = &pkg.Funcs[i]
		}
	}
	if mk == nil {
		t.Fatalf("MapKeys func missing")
	}
	if len(mk.TypeParams) != 2 {
		t.Errorf("MapKeys.TypeParams = %+v", mk.TypeParams)
	}
}

func TestIngestImportsTracked(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"imp.go": `package foo

import "io"

// Get returns a reader.
func Get() io.Reader { return nil }
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	pkg := f.Packages[0]
	found := false
	for _, imp := range pkg.Imports {
		if imp == "io" {
			found = true
		}
	}
	if !found {
		t.Errorf("Imports does not contain io: %+v", pkg.Imports)
	}
}

func TestIngestMultiPackage(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"a/a.go":   "package a\n\nfunc AFn() {}\n",
		"b/b.go":   "package b\n\nfunc BFn() {}\n",
		"main.go":  "package x\n\nfunc XFn() {}\n",
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	if len(f.Packages) < 2 {
		t.Fatalf("expected >= 2 packages, got %d", len(f.Packages))
	}
	// Sorted by ImportPath.
	for i := 1; i < len(f.Packages); i++ {
		if f.Packages[i].ImportPath < f.Packages[i-1].ImportPath {
			t.Errorf("Packages not sorted: %v before %v", f.Packages[i-1].ImportPath, f.Packages[i].ImportPath)
		}
	}
}

func TestIngestDeterminism(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"a.go": `package foo

func Zeta() {}
func Alpha() {}
func Mu() {}
`,
	})
	f1, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	f2, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	b1, _ := f1.Encode()
	b2, _ := f2.Encode()
	if string(b1) != string(b2) {
		t.Errorf("two ingests produced different JSON")
	}
	pkg := f1.Packages[0]
	for i := 1; i < len(pkg.Funcs); i++ {
		if pkg.Funcs[i].Name < pkg.Funcs[i-1].Name {
			t.Errorf("Funcs not sorted: %v before %v", pkg.Funcs[i-1].Name, pkg.Funcs[i].Name)
		}
	}
}

func TestIngestMainPackage(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"main.go": `package main

func main() {}
`,
	})
	f, _ := Ingest(pkgs, IngestOptions{Module: "fixture.test/x"})
	if len(f.Packages) != 1 {
		t.Fatalf("Packages len = %d", len(f.Packages))
	}
	if !f.Packages[0].IsMain {
		t.Errorf("IsMain = false; want true")
	}
}
