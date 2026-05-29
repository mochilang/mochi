package pyodide

import (
	"strings"
	"testing"
)

// TestPhase16Sentinel is the umbrella gate for Phase 16. The user-
// facing goal is "resolve and install Python wheels for the right
// wasm runtime, and emit a WIT world describing the surface a host
// component can dial into". The gate proves both halves end-to-end
// against representative inputs.
func TestPhase16Sentinel(t *testing.T) {
	t.Run("pyodide-vintage-pick", func(t *testing.T) {
		// numpy publishes successive vintages every Pyodide release.
		// The resolver should walk to the newest one that satisfies
		// the floor.
		candidates := []WheelCandidate{
			{Filename: "numpy-2.0.0-cp312-cp312-pyodide_2024_0_wasm32.whl"},
			{Filename: "numpy-2.0.0-cp312-cp312-pyodide_2024_1_wasm32.whl"},
			{Filename: "numpy-2.0.0-cp312-cp312-pyodide_2025_0_wasm32.whl"},
		}
		s := Selector{Target: Target{
			Runtime:    RuntimePyodide,
			PythonABI:  "cp312",
			MinVintage: Vintage{Year: 2024, Rev: 1},
		}}
		r, err := s.Select(candidates)
		if err != nil {
			t.Fatalf("Select: %v", err)
		}
		if r.Chosen == nil {
			t.Fatal("expected a winner")
		}
		if !strings.Contains(r.Chosen.Filename, "pyodide_2025_0") {
			t.Errorf("expected newest vintage, got %q", r.Chosen.Filename)
		}
	})

	t.Run("wasi-server-side-pick", func(t *testing.T) {
		candidates := []WheelCandidate{
			{Filename: "demo-1.0-cp312-cp312-pyodide_2024_0_wasm32.whl"},
			{Filename: "demo-1.0-cp312-cp312-wasi_p2_wasm32.whl"},
		}
		s := Selector{Target: Target{Runtime: RuntimeWASIPreview2, PythonABI: "cp312"}}
		r, err := s.Select(candidates)
		if err != nil {
			t.Fatalf("Select: %v", err)
		}
		if r.Chosen == nil || r.ChosenTag.Kind != TagWASIPreview2 {
			t.Errorf("expected wasi-p2 winner, got %+v", r)
		}
	})

	t.Run("wit-end-to-end", func(t *testing.T) {
		// A Python extern with a record param + record return -> WIT
		// world that a host component would compose against.
		world := WITWorld{
			Package: "mochi:py-bridge",
			Name:    "demo-bridge",
			Records: []WITRecordDecl{
				{Name: "request", Fields: []WITField{
					{Name: "url", Type: WITType{Kind: WITString}},
					{Name: "retries", Type: WITType{Kind: WITU8}},
				}},
				{Name: "response", Fields: []WITField{
					{Name: "status", Type: WITType{Kind: WITU16}},
					{Name: "body", Type: WITType{Kind: WITList, ListOf: &WITType{Kind: WITU8}}},
				}},
			},
			Exports: []WITFunc{
				{Name: "fetch", Params: []WITField{
					{Name: "req", Type: WITType{Kind: WITRef, Name: "request"}},
				}, Return: WITType{Kind: WITRef, Name: "response"}},
			},
		}
		got, err := world.Render()
		if err != nil {
			t.Fatalf("Render: %v", err)
		}
		mustContain := []string{
			"package mochi:py-bridge;",
			"world demo-bridge {",
			"  record request {",
			"    url: string,",
			"    retries: u8,",
			"  record response {",
			"    body: list<u8>,",
			"  export fetch: func(req: request) -> response;",
		}
		for _, s := range mustContain {
			if !strings.Contains(got, s) {
				t.Errorf("WIT missing %q\n--- got ---\n%s", s, got)
			}
		}
	})

	t.Run("rejects-non-wasm-platforms", func(t *testing.T) {
		// Manylinux wheels should not leak into a WASM resolve.
		candidates := []WheelCandidate{
			{Filename: "demo-1.0-cp312-cp312-manylinux_2_28_x86_64.whl"},
		}
		s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
		r, err := s.Select(candidates)
		if err != nil {
			t.Fatalf("Select: %v", err)
		}
		if r.Chosen != nil {
			t.Errorf("expected no winner, got %+v", r.Chosen)
		}
	})
}
