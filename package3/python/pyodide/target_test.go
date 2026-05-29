package pyodide

import "testing"

func TestRuntimeString(t *testing.T) {
	cases := map[Runtime]string{
		RuntimeUnknown:      "unknown",
		RuntimePyodide:      "pyodide",
		RuntimeWASIPreview2: "wasi-p2",
	}
	for r, want := range cases {
		if got := r.String(); got != want {
			t.Errorf("%d.String() = %q, want %q", r, got, want)
		}
	}
}

func TestParseRuntime(t *testing.T) {
	cases := []struct {
		in   string
		want Runtime
		ok   bool
	}{
		{"pyodide", RuntimePyodide, true},
		{"wasi-p2", RuntimeWASIPreview2, true},
		{"wasip2", RuntimeWASIPreview2, true},
		{"", RuntimeUnknown, false},
		{"native", RuntimeUnknown, false},
	}
	for _, tc := range cases {
		got, err := ParseRuntime(tc.in)
		if tc.ok && err != nil {
			t.Errorf("ParseRuntime(%q): unexpected error %v", tc.in, err)
		}
		if !tc.ok && err == nil {
			t.Errorf("ParseRuntime(%q): expected error", tc.in)
		}
		if got != tc.want {
			t.Errorf("ParseRuntime(%q) = %v, want %v", tc.in, got, tc.want)
		}
	}
}

func TestVintageOrdering(t *testing.T) {
	v2024_0 := Vintage{Year: 2024, Rev: 0}
	v2024_1 := Vintage{Year: 2024, Rev: 1}
	v2025_0 := Vintage{Year: 2025, Rev: 0}
	if !v2024_0.Less(v2024_1) {
		t.Error("2024_0 should < 2024_1")
	}
	if !v2024_1.Less(v2025_0) {
		t.Error("2024_1 should < 2025_0")
	}
	if v2025_0.Less(v2024_0) {
		t.Error("2025_0 should not < 2024_0")
	}
	if v2024_0.Less(v2024_0) {
		t.Error("equal vintages should not Less")
	}
}

func TestVintageZero(t *testing.T) {
	if !(Vintage{}).Zero() {
		t.Error("Vintage{} should be Zero")
	}
	if (Vintage{Year: 1, Rev: 0}).Zero() {
		t.Error("Year=1 should not be Zero")
	}
	if (Vintage{Year: 0, Rev: 1}).Zero() {
		t.Error("Rev=1 should not be Zero")
	}
}

func TestVintageString(t *testing.T) {
	if (Vintage{Year: 2024, Rev: 1}).String() != "2024_1" {
		t.Errorf("vintage string mismatch")
	}
}

func TestTargetValidate(t *testing.T) {
	if err := (Target{Runtime: RuntimePyodide}).Validate(); err != nil {
		t.Errorf("expected ok, got %v", err)
	}
	if err := (Target{}).Validate(); err == nil {
		t.Error("expected error on unset runtime")
	}
}
