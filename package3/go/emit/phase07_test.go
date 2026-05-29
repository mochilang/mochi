package emit

import (
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
	"mochi/package3/go/typemap"
	"mochi/package3/go/wrapper"
)

// TestPhase7ExternEmitter is the MEP-74 phase 7 sentinel. It drives
// the full pipeline (apisurface -> typemap -> wrapper -> emit) with
// the same 2-package fixture phase 6's sentinel uses (with the
// chan/generic skip cases stripped to keep the wrapper output
// focused on the baseline). It asserts that every wrapper.EmittedFunc
// produces exactly one matching `extern fun` declaration, that
// error-bearing wrappers wrap the success type in Result<T, string>,
// that the C-side //export symbol appears as a trailing comment for
// downstream auditability, and that the source is byte-deterministic.
func TestPhase7ExternEmitter(t *testing.T) {
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
		t.Fatalf("apisurface.Load: %v", err)
	}
	mapper := typemap.NewMapper(surface)
	we, err := wrapper.NewEmitter(surface, mapper, file.Module, file.Version)
	if err != nil {
		t.Fatalf("wrapper.NewEmitter: %v", err)
	}
	wr, err := we.Emit()
	if err != nil {
		t.Fatalf("wrapper.Emit: %v", err)
	}
	if len(wr.Funcs) != 6 {
		t.Fatalf("expected 6 wrappers; got %d", len(wr.Funcs))
	}

	ee, err := NewEmitter(wr, "sentinel")
	if err != nil {
		t.Fatalf("emit.NewEmitter: %v", err)
	}
	r, err := ee.Emit()
	if err != nil {
		t.Fatalf("emit.Emit: %v", err)
	}

	if len(r.Externs) != 6 {
		t.Fatalf("expected 6 externs; got %d (%+v)", len(r.Externs), r.Externs)
	}

	wantLines := []string{
		"extern fun sentinel.Add(x: int, y: int) : int",
		"extern fun sentinel.Negate(b: bool) : bool",
		"extern fun sentinel.Sqrt(x: float) : float",
		"extern fun sentinel.Greet(who: string) : string",
		"extern fun sentinel.Validate(s: string) : Result<unit, string>",
		"extern fun sentinel.Encode(s: string) : bytes",
	}
	for _, line := range wantLines {
		if !strings.Contains(r.Source, line) {
			t.Errorf("missing extern line %q in source:\n%s", line, r.Source)
		}
	}

	// Every extern carries its C-symbol comment so the audit log
	// (phase 10) can cross-check against the wrapper.
	for _, x := range r.Externs {
		if !strings.Contains(r.Source, x.CSymbol) {
			t.Errorf("missing C-symbol comment for %s", x.CSymbol)
		}
	}

	// Determinism: re-emit and compare byte-for-byte.
	r2, _ := ee.Emit()
	if r.Source != r2.Source {
		t.Errorf("non-deterministic source")
	}

	// Ordering: sorted by C symbol, which makes the alphabetic
	// order of GoName under the same alias the visible order.
	// For sentinel.* (3 funcs) and sentinel.text.* (3 funcs) the
	// symbol prefix differs (sentinel_Add < text_Encode), so
	// Add/Negate/Sqrt come first, then Encode/Greet/Validate.
	idxAdd := strings.Index(r.Source, ".Add(")
	idxEncode := strings.Index(r.Source, ".Encode(")
	if !(idxAdd >= 0 && idxEncode >= 0 && idxAdd < idxEncode) {
		t.Errorf("unexpected ordering: add=%d encode=%d", idxAdd, idxEncode)
	}
}
