// Package cffi tests cover the constrained C subset the Phase 12
// fixture corpus uses. The table-driven cases assert that every
// sidecar .c file in the shared corpus translates without error
// and produces a TS function declaration whose Name, Params, and
// ReturnType match what `extern fun` in the .mochi side expects.
package cffi

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestTranslateCorpus(t *testing.T) {
	root := findRustCorpus(t)
	matches, err := filepath.Glob(filepath.Join(root, "*.c"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(matches) < 20 {
		t.Fatalf("corpus floor 20; found %d in %s", len(matches), root)
	}
	for _, m := range matches {
		name := filepath.Base(m)
		t.Run(strings.TrimSuffix(name, ".c"), func(t *testing.T) {
			b, err := os.ReadFile(m)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			funcs, err := Translate(string(b))
			if err != nil {
				t.Fatalf("translate: %v", err)
			}
			if len(funcs) == 0 {
				t.Fatalf("no funcs translated from %s", name)
			}
			for _, f := range funcs {
				if f.Name == "" {
					t.Errorf("%s: empty Name", name)
				}
				if f.ReturnType != "number" && f.ReturnType != "boolean" && f.ReturnType != "void" {
					t.Errorf("%s: unexpected ReturnType %q", name, f.ReturnType)
				}
				if !strings.HasPrefix(f.Body, "{") || !strings.HasSuffix(f.Body, "}") {
					t.Errorf("%s: body not brace-wrapped: %q", name, f.Body)
				}
				for _, p := range f.Params {
					if p.Name == "" {
						t.Errorf("%s: empty Param Name", name)
					}
					if p.Type != "number" && p.Type != "boolean" {
						t.Errorf("%s: unexpected Param Type %q", name, p.Type)
					}
				}
			}
		})
	}
}

func TestTranslateNamesAndArities(t *testing.T) {
	cases := []struct {
		file   string
		funcs  []string
		arity  []int
		retTyp []string
	}{
		{"abs_extern.c", []string{"c_abs"}, []int{1}, []string{"number"}},
		{"add_extern.c", []string{"c_add"}, []int{2}, []string{"number"}},
		{"clamp_extern.c", []string{"c_clamp"}, []int{3}, []string{"number"}},
		{"extern_returns_zero.c", []string{"c_zero"}, []int{0}, []string{"number"}},
		{"is_even_extern.c", []string{"c_is_even"}, []int{1}, []string{"boolean"}},
		{"is_prime_extern.c", []string{"c_is_prime"}, []int{1}, []string{"boolean"}},
		{"two_externs.c", []string{"c_add", "c_mul"}, []int{2, 2}, []string{"number", "number"}},
	}
	root := findRustCorpus(t)
	for _, tc := range cases {
		t.Run(tc.file, func(t *testing.T) {
			b, err := os.ReadFile(filepath.Join(root, tc.file))
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			funcs, err := Translate(string(b))
			if err != nil {
				t.Fatalf("translate: %v", err)
			}
			if len(funcs) != len(tc.funcs) {
				t.Fatalf("got %d funcs, want %d", len(funcs), len(tc.funcs))
			}
			for i, f := range funcs {
				if f.Name != tc.funcs[i] {
					t.Errorf("funcs[%d].Name = %q, want %q", i, f.Name, tc.funcs[i])
				}
				if len(f.Params) != tc.arity[i] {
					t.Errorf("funcs[%d].Params arity = %d, want %d", i, len(f.Params), tc.arity[i])
				}
				if f.ReturnType != tc.retTyp[i] {
					t.Errorf("funcs[%d].ReturnType = %q, want %q", i, f.ReturnType, tc.retTyp[i])
				}
			}
		})
	}
}

func TestTranslateRejectsPointers(t *testing.T) {
	_, err := Translate("int* c_bad(int* x) { return x; }")
	if err == nil {
		t.Fatalf("expected error for pointer return type")
	}
}

func TestTranslateDropsIncludes(t *testing.T) {
	src := `#include <stdint.h>
#include <stdbool.h>
int64_t c_one(void) { return 1; }`
	funcs, err := Translate(src)
	if err != nil {
		t.Fatalf("translate: %v", err)
	}
	if len(funcs) != 1 || funcs[0].Name != "c_one" {
		t.Fatalf("unexpected funcs: %+v", funcs)
	}
}

func TestTranslateBlockBody(t *testing.T) {
	src := `int64_t c_id(int64_t x) { return x; }`
	funcs, err := Translate(src)
	if err != nil {
		t.Fatalf("translate: %v", err)
	}
	if len(funcs) != 1 {
		t.Fatalf("want 1 func, got %d", len(funcs))
	}
	if !strings.Contains(funcs[0].Body, "return x;") {
		t.Errorf("body missing return: %q", funcs[0].Body)
	}
}

// findRustCorpus walks up from the cffi package directory to the
// repo root and returns the absolute path to the shared
// tests/transpiler3/rust/fixtures/phase12-ffi corpus.
func findRustCorpus(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	for i := 0; i < 6; i++ {
		dir = filepath.Dir(dir)
	}
	corpus := filepath.Join(dir, "tests", "transpiler3", "rust", "fixtures", "phase12-ffi")
	if _, err := os.Stat(corpus); err != nil {
		t.Fatalf("corpus not found at %s: %v", corpus, err)
	}
	return corpus
}
