//go:build slow

package erlang_test

import (
	"bytes"
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	erl "mochi/aster/x/erlang"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestInspectGolden(t *testing.T) {
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "erl")
	outDir := filepath.Join(root, "tests", "aster", "x", "erlang")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.erl"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	var selected []string
	for _, f := range files {
		base := filepath.Base(f)
		switch base {
		case "append_builtin.erl", "avg_builtin.erl", "basic_compare.erl", "binary_precedence.erl", "bool_chain.erl":
			selected = append(selected, f)
		}
		if len(selected) == 5 {
			break
		}
	}
	files = selected

	for _, path := range files {
		name := strings.TrimSuffix(filepath.Base(path), ".erl")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(path)
			if err != nil {
				t.Fatalf("read: %v", err)
			}
			prog, err := erl.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			b, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			golden := filepath.Join(outDir, name+".erl.json")
			if shouldUpdate() {
				if err := os.WriteFile(golden, append(b, '\n'), 0o644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
			}
			want, err := os.ReadFile(golden)
			if err != nil {
				t.Fatalf("missing golden: %v", err)
			}
			want = bytes.TrimSpace(want)
			if !bytes.Equal(bytes.TrimSpace(b), want) {
				t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", b, want)
			}
		})
	}
}
