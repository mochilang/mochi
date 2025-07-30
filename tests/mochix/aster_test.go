//go:build slow

package mochix_test

import (
	"bytes"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

func TestInspectGolden(t *testing.T) {
	root := repoRoot(t)
	asterDir := filepath.Join(root, "tests/aster/x")
	langMap := map[string]struct{ dir, ext string }{
		"elixir":  {"ex", "exs"},
		"erlang":  {"erl", "erl"},
		"haskell": {"hs", "hs"},
		"kotlin":  {"kt", "kt"},
		"ocaml":   {"ocaml", "ml"},
		"prolog":  {"prolog", "pl"},
		"ruby":    {"rb", "rb"},
		"scheme":  {"scheme", "scm"},
	}

	entries, err := os.ReadDir(asterDir)
	if err != nil {
		t.Fatal(err)
	}
	for _, ent := range entries {
		lang := ent.Name()
		if !ent.IsDir() || lang == "rkt" {
			continue
		}
		cfg, ok := langMap[lang]
		if !ok {
			cfg = struct{ dir, ext string }{lang, lang}
		}

		gfiles, err := filepath.Glob(filepath.Join(asterDir, lang, "*.json"))
		if err != nil {
			t.Fatal(err)
		}
		sort.Strings(gfiles)
		for _, gf := range gfiles {
			base := filepath.Base(gf)
			nameExt := strings.TrimSuffix(base, ".json")
			i := strings.LastIndex(nameExt, ".")
			if i < 0 {
				t.Fatalf("invalid golden filename: %s", base)
			}
			name := nameExt[:i]
			src := filepath.Join(root, "tests/transpiler/x", cfg.dir, name+"."+cfg.ext)
			if _, err := os.Stat(src); os.IsNotExist(err) {
				t.Skipf("source %s not found", src)
				continue
			}
			got, err := runMochix(t, "inspect", lang, src)
			if err != nil {
				t.Fatalf("inspect error: %v", err)
			}
			want, err := os.ReadFile(gf)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				t.Fatalf("golden mismatch for %s\n--- got ---\n%s\n--- want ---\n%s", base, got, want)
			}
		}
	}
}
