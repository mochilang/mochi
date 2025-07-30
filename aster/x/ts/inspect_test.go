//go:build slow

package ts_test

import (
	"encoding/json"
	"flag"
	"os"
	"path/filepath"
	"strings"
	"testing"

	ts "mochi/aster/x/ts"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func TestInspect_Golden(t *testing.T) {
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler", "x", "ts", "cross_join.ts")

	outDir := filepath.Join(root, "tests", "aster", "x", "ts")
	os.MkdirAll(outDir, 0o755)

	name := strings.TrimSuffix(filepath.Base(src), ".ts")
	t.Run(name, func(t *testing.T) {
		data, err := os.ReadFile(src)
		if err != nil {
			t.Fatalf("read src: %v", err)
		}
		prog, err := ts.Inspect(string(data))
		if err != nil {
			t.Fatalf("inspect: %v", err)
		}
		outData, err := json.MarshalIndent(prog, "", "  ")
		if err != nil {
			t.Fatalf("marshal: %v", err)
		}
		golden := filepath.Join(outDir, name+".ts.json")
		if *update {
			os.WriteFile(golden, append(outData, '\n'), 0o644)
			return
		}
		want, err := os.ReadFile(golden)
		if err != nil {
			t.Fatalf("read golden: %v", err)
		}
		if strings.TrimSpace(string(want)) != strings.TrimSpace(string(outData)) {
			t.Fatalf("golden mismatch\n--- Got ---\n%s\n--- Want ---\n%s", outData, want)
		}
	})
}
