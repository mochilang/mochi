package library

import (
	"strings"
	"testing"
)

func minimalAPI() PublicAPI {
	return PublicAPI{
		CrateName: "mochi_demo",
		Version:   "0.1.0",
		Package: PackageMeta{
			Description: "demo crate",
			License:     "MIT OR Apache-2.0",
		},
		Items: []Item{
			ItemFn{
				Name:   "add",
				Params: []Param{{"a", "i64"}, {"b", "i64"}},
				Return: "i64",
				Body:   "a + b",
			},
		},
	}
}

func TestRenderProducesRequiredFiles(t *testing.T) {
	files, err := Render(minimalAPI())
	if err != nil {
		t.Fatalf("render: %v", err)
	}
	if _, ok := files["Cargo.toml"]; !ok {
		t.Error("missing Cargo.toml")
	}
	if _, ok := files["src/lib.rs"]; !ok {
		t.Error("missing src/lib.rs")
	}
	// no header without CHeader
	if _, ok := files["include/mochi_demo.h"]; ok {
		t.Error("header should not be emitted without CHeader=true")
	}
}

func TestRenderWithCHeader(t *testing.T) {
	api := minimalAPI()
	api.CHeader = true
	api.Items = []Item{
		ItemFn{
			Name:   "mochi_demo_add",
			Params: []Param{{"a", "i64"}, {"b", "i64"}},
			Return: "i64",
			Extern: true,
			Body:   "a + b",
		},
	}
	files, err := Render(api)
	if err != nil {
		t.Fatalf("render: %v", err)
	}
	hdr, ok := files["include/mochi_demo.h"]
	if !ok {
		t.Fatal("expected header file")
	}
	if !strings.Contains(hdr, "#ifndef MOCHI_DEMO_H") {
		t.Errorf("missing include guard: %s", hdr)
	}
	if !strings.Contains(hdr, "int64_t mochi_demo_add(int64_t a, int64_t b);") {
		t.Errorf("missing extern fn decl: %s", hdr)
	}
}

func TestRenderIsDeterministic(t *testing.T) {
	api := minimalAPI()
	a, err := Render(api)
	if err != nil {
		t.Fatal(err)
	}
	b, err := Render(api)
	if err != nil {
		t.Fatal(err)
	}
	for k, v := range a {
		if b[k] != v {
			t.Errorf("nondeterministic output for %s", k)
		}
	}
}

func TestRenderRejectsInvalidCrateName(t *testing.T) {
	cases := []string{"", "1leading-digit", "no spaces", "has/slash", "has.dot"}
	for _, name := range cases {
		api := minimalAPI()
		api.CrateName = name
		if _, err := Render(api); err == nil {
			t.Errorf("expected error for crate name %q", name)
		}
	}
}

func TestRenderAcceptsValidCrateNames(t *testing.T) {
	cases := []string{"a", "anyhow", "rand_chacha", "rust-num", "Crate123"}
	for _, name := range cases {
		api := minimalAPI()
		api.CrateName = name
		if _, err := Render(api); err != nil {
			t.Errorf("unexpected error for crate name %q: %v", name, err)
		}
	}
}

func TestRenderRejectsMissingVersion(t *testing.T) {
	api := minimalAPI()
	api.Version = ""
	if _, err := Render(api); err == nil {
		t.Error("expected error for missing version")
	}
}

func TestRenderRejectsDuplicateItemName(t *testing.T) {
	api := minimalAPI()
	api.Items = []Item{
		ItemFn{Name: "x", Body: "()", Return: "()"},
		ItemFn{Name: "x", Body: "()", Return: "()"},
	}
	if _, err := Render(api); err == nil {
		t.Error("expected error for duplicate item name")
	}
}

func TestRenderRejectsItemMissingName(t *testing.T) {
	api := minimalAPI()
	api.Items = []Item{ItemFn{Name: "", Body: "()", Return: "()"}}
	if _, err := Render(api); err == nil {
		t.Error("expected error for empty item name")
	}
}

func TestRenderRejectsNilItem(t *testing.T) {
	api := minimalAPI()
	api.Items = []Item{nil}
	if _, err := Render(api); err == nil {
		t.Error("expected error for nil item")
	}
}

func TestRenderRejectsBodylessNonExternFn(t *testing.T) {
	api := minimalAPI()
	api.Items = []Item{ItemFn{Name: "f", Return: "()", Body: ""}}
	if _, err := Render(api); err == nil {
		t.Error("expected error for non-extern fn missing body")
	}
}

func TestRenderAcceptsBodylessExternFn(t *testing.T) {
	api := minimalAPI()
	api.Items = []Item{ItemFn{Name: "f", Return: "i64", Body: "", Extern: true}}
	if _, err := Render(api); err != nil {
		t.Errorf("unexpected error for bodyless extern fn: %v", err)
	}
}

func TestFilesSortedIsStable(t *testing.T) {
	api := minimalAPI()
	api.CHeader = true
	api.Items = []Item{ItemFn{Name: "f", Extern: true, Return: "i64"}}
	files, err := Render(api)
	if err != nil {
		t.Fatal(err)
	}
	sorted := files.Sorted()
	want := []string{"Cargo.toml", "include/mochi_demo.h", "src/lib.rs"}
	if len(sorted) != len(want) {
		t.Fatalf("len=%d want %d: %v", len(sorted), len(want), sorted)
	}
	for i := range want {
		if sorted[i] != want[i] {
			t.Errorf("sorted[%d]=%q want %q", i, sorted[i], want[i])
		}
	}
}

func TestRenderErrorString(t *testing.T) {
	cases := []struct {
		err     RenderError
		matches []string
	}{
		{RenderError{Reason: "x"}, []string{"library render", "x"}},
		{RenderError{Reason: "y", Item: "foo"}, []string{"y", "\"foo\""}},
	}
	for _, c := range cases {
		s := c.err.Error()
		for _, m := range c.matches {
			if !strings.Contains(s, m) {
				t.Errorf("error %q missing %q", s, m)
			}
		}
	}
}

func TestHeaderName(t *testing.T) {
	cases := map[string]string{
		"hex":         "hex",
		"rust-num":    "rust_num",
		"my-cool-lib": "my_cool_lib",
	}
	for in, want := range cases {
		if got := headerName(in); got != want {
			t.Errorf("headerName(%q)=%q want %q", in, got, want)
		}
	}
}

func TestValidCrateName(t *testing.T) {
	cases := map[string]bool{
		"":            false,
		"a":           true,
		"1bad":        false,
		"-lead":       false,
		"_lead":       false,
		"with space":  false,
		"with.dot":    false,
		"with/slash":  false,
		"abc_123":     true,
		"abc-123":     true,
		"ABC":         true,
		"ümlaut":      false,
	}
	for n, want := range cases {
		if got := validCrateName(n); got != want {
			t.Errorf("validCrateName(%q)=%v want %v", n, got, want)
		}
	}
}
