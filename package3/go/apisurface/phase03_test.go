package apisurface

import (
	"strings"
	"testing"
)

// TestPhase3Ingest is the umbrella sentinel for MEP-74 phase 3.
// Drives a complete pipeline: synthesise an in-memory two-package
// fixture, load it via go/packages, run Ingest, Encode the result
// to JSON, Decode it back, and verify every observable assertion.
//
// Passing this sentinel means a downstream caller can integrate
// phase 3 end-to-end: produce a stable ApiSurface JSON document
// from a Go source tree, and parse it back via Decode.
func TestPhase3Ingest(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"go.mod": "module fixture.test/m\n\ngo 1.21\n",
		"app/app.go": `// Package app is a fixture entry point.
package app

import "io"

// Greeter is the central type.
type Greeter struct {
	// Name is who to greet.
	Name string ` + "`json:\"name\"`" + `
}

// Hello returns a greeting.
func (g *Greeter) Hello() string { return "hi " + g.Name }

// New returns a new Greeter.
func New(name string) *Greeter { return &Greeter{Name: name} }

// Stream copies r into a discard.
func Stream(r io.Reader) error { return nil }

// Version of the fixture.
const Version = "1.0.0"

// Default is a default Greeter.
var Default = &Greeter{Name: "world"}
`,
		"util/util.go": `// Package util has helpers.
package util

// Add returns the sum.
func Add[T int | float64](a, b T) T { return a + b }

// Stringer is an interface.
type Stringer interface {
	String() string
}
`,
	})

	f, err := Ingest(pkgs, IngestOptions{
		Module:      "fixture.test/m",
		Version:     "v0.1.0",
		GeneratedBy: "phase03_test",
	})
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}

	// Encode then Decode to exercise the JSON path.
	buf, err := f.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	decoded, err := Decode(buf)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}

	if decoded.Module != "fixture.test/m" {
		t.Errorf("Module = %q", decoded.Module)
	}
	if decoded.Version != "v0.1.0" {
		t.Errorf("Version = %q", decoded.Version)
	}
	if decoded.GeneratedBy != "phase03_test" {
		t.Errorf("GeneratedBy = %q", decoded.GeneratedBy)
	}
	if !strings.HasPrefix(decoded.GoVersion, "go") {
		t.Errorf("GoVersion = %q", decoded.GoVersion)
	}
	if len(decoded.Packages) != 2 {
		t.Fatalf("Packages len = %d, want 2", len(decoded.Packages))
	}

	var app, util *Package
	for i := range decoded.Packages {
		switch {
		case strings.HasSuffix(decoded.Packages[i].ImportPath, "/app"):
			app = &decoded.Packages[i]
		case strings.HasSuffix(decoded.Packages[i].ImportPath, "/util"):
			util = &decoded.Packages[i]
		}
	}
	if app == nil || util == nil {
		t.Fatalf("missing fixture packages: %+v", decoded.Packages)
	}

	// app: 1 type with 1 method, 2 funcs, 1 const, 1 var, io import.
	if got, want := len(app.Types), 1; got != want {
		t.Errorf("app.Types len = %d, want %d", got, want)
	}
	if app.Types[0].Name != "Greeter" {
		t.Errorf("app.Types[0].Name = %q", app.Types[0].Name)
	}
	// New + Stream; Hello is a method, lives on Greeter.Methods.
	if got, want := len(app.Funcs), 2; got != want {
		t.Errorf("app.Funcs len = %d, want %d", got, want)
	}
	if got, want := len(app.Types[0].Methods), 1; got != want {
		t.Errorf("Greeter.Methods len = %d, want %d", got, want)
	}
	if got, want := len(app.Consts), 1; got != want {
		t.Errorf("app.Consts len = %d, want %d", got, want)
	}
	if got, want := len(app.Vars), 1; got != want {
		t.Errorf("app.Vars len = %d, want %d", got, want)
	}
	foundIO := false
	for _, imp := range app.Imports {
		if imp == "io" {
			foundIO = true
		}
	}
	if !foundIO {
		t.Errorf("app.Imports missing io: %+v", app.Imports)
	}

	// util: 1 generic func + 1 interface type.
	if got, want := len(util.Funcs), 1; got != want {
		t.Errorf("util.Funcs len = %d, want %d", got, want)
	}
	if got := util.Funcs[0]; len(got.TypeParams) != 1 {
		t.Errorf("util.Funcs[0].TypeParams = %+v", got.TypeParams)
	}
	if got, want := len(util.Types), 1; got != want {
		t.Errorf("util.Types len = %d, want %d", got, want)
	}
	if util.Types[0].Kind != KindInterface {
		t.Errorf("util.Types[0].Kind = %q", util.Types[0].Kind)
	}

	// Re-encoding must produce identical bytes (canonical form).
	buf2, err := decoded.Encode()
	if err != nil {
		t.Fatalf("re-Encode: %v", err)
	}
	if string(buf) != string(buf2) {
		t.Errorf("Encode round-trip is not byte-stable")
	}
}
