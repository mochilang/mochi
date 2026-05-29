package wrapper

import (
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
	"mochi/package3/go/typemap"
)

// TestPhase6Wrapper is the MEP-74 phase 6 sentinel. It exercises a
// multi-package fixture mirroring a real Go module's shape (a
// public scalar API package + a sub-package with string handling
// and an error-returning func) and asserts that the emitter
// produces parseable, byte-deterministic source with the right
// //export symbols, the right C-side type lowerings, and
// runtime.KeepAlive injection on every pointer-bearing parameter.
//
// Sub-phase skip records (methods, generic funcs, chan/func value
// params, struct/map params) round-trip through Skipped with the
// documented Reason format.
func TestPhase6Wrapper(t *testing.T) {
	file := &apisurface.File{
		SchemaVersion: apisurface.SchemaVersion,
		Module:        "example.com/sentinel",
		Version:       "v1.0.0",
		Packages: []apisurface.Package{
			{
				ImportPath: "example.com/sentinel",
				Name:       "sentinel",
				Funcs: []apisurface.Func{
					{Name: "Add", Params: []apisurface.Param{{Name: "x", Type: "int"}, {Name: "y", Type: "int"}}, Results: []apisurface.Param{{Type: "int"}}},
					{Name: "Negate", Params: []apisurface.Param{{Name: "b", Type: "bool"}}, Results: []apisurface.Param{{Type: "bool"}}},
					{Name: "Sqrt", Params: []apisurface.Param{{Name: "x", Type: "float64"}}, Results: []apisurface.Param{{Type: "float64"}}},
					// Deferred to phase 6.x:
					{Name: "Listen", Params: []apisurface.Param{{Type: "chan int"}}},
					{Name: "MapKeys", TypeParams: []apisurface.TypeParam{{Name: "K"}}, Params: []apisurface.Param{{Name: "m", Type: "map[K]int"}}, Results: []apisurface.Param{{Type: "[]K"}}},
				},
			},
			{
				ImportPath: "example.com/sentinel/text",
				Name:       "text",
				Funcs: []apisurface.Func{
					{Name: "Greet", Params: []apisurface.Param{{Name: "who", Type: "string"}}, Results: []apisurface.Param{{Type: "string"}}},
					{Name: "Validate", Params: []apisurface.Param{{Name: "s", Type: "string"}}, Results: []apisurface.Param{{Type: "error"}}},
					{Name: "Encode", Params: []apisurface.Param{{Name: "s", Type: "string"}}, Results: []apisurface.Param{{Type: "[]byte"}}},
				},
			},
		},
	}
	surface, err := apisurface.Load(file, apisurface.LoadOptions{})
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	mapper := typemap.NewMapper(surface)
	e, err := NewEmitter(surface, mapper, file.Module, file.Version)
	if err != nil {
		t.Fatalf("NewEmitter: %v", err)
	}

	r, err := e.Emit()
	if err != nil {
		t.Fatalf("Emit: %v", err)
	}

	// Validate package name + file set.
	if r.ModuleName != "mochi_go_example_com_sentinel" {
		t.Errorf("ModuleName = %q", r.ModuleName)
	}
	if _, ok := r.Files["wrap.go"]; !ok {
		t.Errorf("missing wrap.go")
	}
	if _, ok := r.Files["wrap_handles.go"]; !ok {
		t.Errorf("missing wrap_handles.go")
	}

	// Both emitted files must parse.
	for name, src := range r.Files {
		if err := ParseGenerated(name, src); err != nil {
			t.Errorf("%s: %v", name, err)
		}
	}

	// Required wrappers: Add (3 scalars), Negate (bool), Sqrt (float),
	// Greet (string in/out), Validate (string in, error out), Encode
	// (string in, bytes out).
	wantSymbols := []string{
		"mochi_go_example_com_sentinel_sentinel_Add",
		"mochi_go_example_com_sentinel_sentinel_Negate",
		"mochi_go_example_com_sentinel_sentinel_Sqrt",
		"mochi_go_example_com_sentinel_text_Greet",
		"mochi_go_example_com_sentinel_text_Validate",
		"mochi_go_example_com_sentinel_text_Encode",
	}
	gotSymbols := map[string]bool{}
	for _, f := range r.Funcs {
		gotSymbols[f.Symbol] = true
	}
	for _, s := range wantSymbols {
		if !gotSymbols[s] {
			t.Errorf("missing wrapper symbol %q", s)
		}
	}

	// Required SkipNotes.
	wantSkipped := map[string]string{
		"Listen":  "phase 6.x",
		"MapKeys": "generic",
	}
	gotSkipped := map[string]string{}
	for _, n := range r.Skipped {
		gotSkipped[n.Name] = n.Reason
	}
	for name, want := range wantSkipped {
		if !strings.Contains(gotSkipped[name], want) {
			t.Errorf("skipped[%q] = %q; want substring %q", name, gotSkipped[name], want)
		}
	}

	src := r.Files["wrap.go"]
	// runtime.KeepAlive on pointer-bearing string param.
	if !strings.Contains(src, "runtime.KeepAlive(who)") {
		t.Errorf("missing keepalive for who: %s", src)
	}
	if !strings.Contains(src, "runtime.KeepAlive(s)") {
		t.Errorf("missing keepalive for s")
	}
	// Module-scoped helpers.
	if !strings.Contains(src, "mochi_go_example_com_sentinel_string_free") {
		t.Errorf("missing module-scoped string_free")
	}
	if !strings.Contains(r.Files["wrap_handles.go"], "mochi_go_example_com_sentinel_bytes_free") {
		t.Errorf("missing module-scoped bytes_free")
	}

	// Determinism: re-emit and compare byte-for-byte.
	r2, err := e.Emit()
	if err != nil {
		t.Fatalf("re-emit: %v", err)
	}
	for name, src := range r.Files {
		if r2.Files[name] != src {
			t.Errorf("non-deterministic output for %s", name)
		}
	}
}
