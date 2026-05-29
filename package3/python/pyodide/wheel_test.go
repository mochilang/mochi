package pyodide

import "testing"

func TestSelectorRejectsUnsetRuntime(t *testing.T) {
	var s Selector
	_, err := s.Select(nil)
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestSelectorPicksNewestVintage(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-pyodide_2024_0_wasm32.whl", URL: "u1"},
		{Filename: "demo-1.0-cp312-cp312-pyodide_2025_0_wasm32.whl", URL: "u2"},
		{Filename: "demo-1.0-cp312-cp312-pyodide_2024_1_wasm32.whl", URL: "u3"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen == nil {
		t.Fatal("expected a winner")
	}
	if r.Chosen.URL != "u2" {
		t.Errorf("URL = %q, want u2 (newest vintage)", r.Chosen.URL)
	}
	if r.ChosenTag.Vintage != (Vintage{Year: 2025, Rev: 0}) {
		t.Errorf("ChosenTag.Vintage = %+v", r.ChosenTag.Vintage)
	}
}

func TestSelectorRejectsBelowMinVintage(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-pyodide_2023_0_wasm32.whl"},
		{Filename: "demo-1.0-cp312-cp312-pyodide_2024_1_wasm32.whl"},
	}
	s := Selector{Target: Target{
		Runtime:    RuntimePyodide,
		PythonABI:  "cp312",
		MinVintage: Vintage{Year: 2024, Rev: 0},
	}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen == nil {
		t.Fatal("expected a winner")
	}
	if r.ChosenTag.Vintage != (Vintage{Year: 2024, Rev: 1}) {
		t.Errorf("ChosenTag.Vintage = %+v", r.ChosenTag.Vintage)
	}
	reason := r.Reasons["demo-1.0-cp312-cp312-pyodide_2023_0_wasm32.whl"]
	if reason == "" {
		t.Error("expected rejection reason for 2023 wheel")
	}
}

func TestSelectorRejectsABIMismatch(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp311-cp311-pyodide_2024_0_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen != nil {
		t.Errorf("expected no chosen, got %+v", r.Chosen)
	}
	reason := r.Reasons["demo-1.0-cp311-cp311-pyodide_2024_0_wasm32.whl"]
	if reason == "" {
		t.Error("expected ABI rejection reason")
	}
}

func TestSelectorRejectsWrongRuntime(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-wasi_p2_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen != nil {
		t.Errorf("expected no chosen, got %+v", r.Chosen)
	}
}

func TestSelectorPrefersPyodideOverEmscripten(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-emscripten_3_1_45_wasm32.whl"},
		{Filename: "demo-1.0-cp312-cp312-pyodide_2024_0_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen == nil || r.ChosenTag.Kind != TagPyodide {
		t.Errorf("expected pyodide preference, got %+v", r)
	}
}

func TestSelectorEmscriptenSortsByVersion(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-emscripten_3_1_30_wasm32.whl"},
		{Filename: "demo-1.0-cp312-cp312-emscripten_3_1_45_wasm32.whl"},
		{Filename: "demo-1.0-cp312-cp312-emscripten_3_0_99_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.ChosenTag.Emscripten != (EmscriptenVersion{Major: 3, Minor: 1, Patch: 45}) {
		t.Errorf("ChosenTag.Emscripten = %+v", r.ChosenTag.Emscripten)
	}
}

func TestSelectorWASI(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-wasi_p2_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimeWASIPreview2, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen == nil || r.ChosenTag.Kind != TagWASIPreview2 {
		t.Errorf("expected wasi, got %+v", r)
	}
}

func TestSelectorAcceptsEmptyABI(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-abi3-pyodide_2024_0_wasm32.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen == nil {
		t.Error("expected a winner when ABI is unconstrained")
	}
}

func TestSelectorRejectsBadFilenames(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "no-extension"},
		{Filename: "too-short.whl"},
		{Filename: "demo-1.0-cp312-cp312-unknown_tag.whl"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r, err := s.Select(candidates)
	if err != nil {
		t.Fatalf("Select: %v", err)
	}
	if r.Chosen != nil {
		t.Error("expected no chosen")
	}
	if len(r.Reasons) != 3 {
		t.Errorf("expected 3 reasons, got %d: %v", len(r.Reasons), r.Reasons)
	}
}

func TestSelectorDeterministicTieBreak(t *testing.T) {
	candidates := []WheelCandidate{
		{Filename: "demo-1.0-cp312-cp312-pyodide_2024_0_wasm32.whl", URL: "a"},
		{Filename: "alpha-1.0-cp312-cp312-pyodide_2024_0_wasm32.whl", URL: "b"},
	}
	s := Selector{Target: Target{Runtime: RuntimePyodide, PythonABI: "cp312"}}
	r1, _ := s.Select(candidates)
	r2, _ := s.Select(candidates)
	if r1.Chosen.URL != r2.Chosen.URL {
		t.Errorf("tie-break is not deterministic: %q vs %q", r1.Chosen.URL, r2.Chosen.URL)
	}
	if r1.Chosen.URL != "b" {
		t.Errorf("expected alpha- (alphabetical) winner, got %q", r1.Chosen.URL)
	}
}
