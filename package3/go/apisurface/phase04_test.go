package apisurface

import (
	"testing"
)

// TestPhase4ApisurfaceParser is the umbrella sentinel for MEP-74
// phase 4. Drives the full pipeline: synthesise an in-memory
// fixture module, run Ingest (phase 3) over it, Encode the result
// to JSON, Decode it back, Load the decoded file into a typed
// Surface, and verify the typed lookups recover every declaration
// with the right type shape. Passing this sentinel means a
// downstream caller can integrate phase 4 end-to-end: take an
// ApiSurface JSON blob, walk a typed AST.
func TestPhase4ApisurfaceParser(t *testing.T) {
	pkgs := loadFixture(t, map[string]string{
		"go.mod": "module fixture.test/m\n\ngo 1.21\n",
		"app/app.go": `// Package app is a fixture.
package app

import "io"

// Greeter greets.
type Greeter struct {
	Name string
}

// Hello returns the greeting.
func (g *Greeter) Hello() string { return "hi " + g.Name }

// Read consumes from r.
func Read(r io.Reader, buf []byte) (int, error) { return 0, nil }

// Pair holds two ints.
type Pair struct {
	A, B int
}

// MapKeys returns keys of m.
func MapKeys[K comparable, V any](m map[K]V) []K { return nil }

// Default is a default Greeter.
var Default = &Greeter{Name: "world"}

// Version of the fixture.
const Version = "1.0.0"
`,
	})
	f, err := Ingest(pkgs, IngestOptions{Module: "fixture.test/m", Version: "v0.1.0"})
	if err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	buf, err := f.Encode()
	if err != nil {
		t.Fatalf("Encode: %v", err)
	}
	decoded, err := Decode(buf)
	if err != nil {
		t.Fatalf("Decode: %v", err)
	}
	s, err := Load(decoded, LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Find the app package by suffix match (the import path will be
	// "fixture.test/m/app" depending on how go/packages resolves it).
	var appPath string
	for _, p := range s.PackagePaths() {
		if endsWith(p, "/app") {
			appPath = p
		}
	}
	if appPath == "" {
		t.Fatalf("could not locate app package among %v", s.PackagePaths())
	}

	// Read func: (io.Reader, []byte) -> (int, error)
	read := s.LookupFunc(appPath, "Read")
	if read == nil {
		t.Fatalf("Read missing")
	}
	if len(read.Params) != 2 || len(read.Results) != 2 {
		t.Errorf("Read shape: params=%d results=%d", len(read.Params), len(read.Results))
	}
	n, ok := read.Params[0].Type.(NamedType)
	if !ok || n.PackagePath != "io" || n.Name != "Reader" {
		t.Errorf("Read.Params[0].Type = %#v", read.Params[0].Type)
	}
	if _, ok := read.Params[1].Type.(SliceType); !ok {
		t.Errorf("Read.Params[1].Type = %T", read.Params[1].Type)
	}
	if b, ok := read.Results[0].Type.(BasicType); !ok || b.Name != "int" {
		t.Errorf("Read.Results[0].Type = %#v", read.Results[0].Type)
	}

	// Greeter type + method.
	greeter := s.LookupType(appPath, "Greeter")
	if greeter == nil {
		t.Fatalf("Greeter missing")
	}
	if greeter.Kind != KindStruct {
		t.Errorf("Greeter.Kind = %q", greeter.Kind)
	}
	hello := s.LookupMethod(appPath, "Greeter", "Hello")
	if hello == nil {
		t.Fatalf("Greeter.Hello missing")
	}
	if hello.Underlying.Receiver != "Greeter" || !hello.Underlying.ReceiverPointer {
		t.Errorf("Hello receiver: %q ptr=%v", hello.Underlying.Receiver, hello.Underlying.ReceiverPointer)
	}

	// MapKeys: generic with 2 type params.
	mk := s.LookupFunc(appPath, "MapKeys")
	if mk == nil {
		t.Fatalf("MapKeys missing")
	}
	if len(mk.TypeParams) != 2 {
		t.Errorf("MapKeys.TypeParams = %+v", mk.TypeParams)
	}
	// Result is []K
	if len(mk.Results) != 1 {
		t.Fatalf("MapKeys results: %+v", mk.Results)
	}
	if _, ok := mk.Results[0].Type.(SliceType); !ok {
		t.Errorf("MapKeys.Results[0].Type = %T", mk.Results[0].Type)
	}

	// Default var: *Greeter.
	defv := s.Packages[appPath].Vars["Default"]
	if defv == nil {
		t.Fatalf("Default var missing")
	}
	if _, ok := defv.Type.(PointerType); !ok {
		t.Errorf("Default.Type = %T", defv.Type)
	}

	// Version const: string.
	ver := s.Packages[appPath].Consts["Version"]
	if ver == nil {
		t.Fatalf("Version const missing")
	}
	if b, ok := ver.Type.(BasicType); !ok || b.Name != "string" {
		t.Errorf("Version.Type = %#v", ver.Type)
	}
}

func endsWith(s, suffix string) bool {
	if len(s) < len(suffix) {
		return false
	}
	return s[len(s)-len(suffix):] == suffix
}
