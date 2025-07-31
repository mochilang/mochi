//go:build slow

package clj_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	clj "mochi/aster/x/clj"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestPrint_Golden(t *testing.T) {
	ensureBabashka(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "clj")
	outDir := filepath.Join(root, "tests", "aster", "x", "clj")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.clj"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	for _, f := range files {
		if filepath.Base(f) == "two-sum.clj" {
			selected = append(selected, f)
		}
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".clj")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := clj.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".clj.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, astJSON, 0644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}
			wantJSON, err := os.ReadFile(jsonPath)
			if err != nil {
				t.Skip("missing golden")
				return
			}
			if string(astJSON) != string(wantJSON) {
				t.Fatalf("json mismatch\n--- got ---\n%s\n--- want ---\n%s", astJSON, wantJSON)
			}
			out, err := clj.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".clj")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			cmd := exec.Command("bb", outPath)
			got, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			want, err := exec.Command("bb", src).CombinedOutput()
			if err != nil {
				t.Fatalf("run original: %v\n%s", err, want)
			}
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if string(got) != string(want) {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
