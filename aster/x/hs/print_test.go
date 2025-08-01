//go:build slow

package hs_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	hs "mochi/aster/x/hs"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestPrint_Golden(t *testing.T) {
	ensureHaskell(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "hs")
	outDir := filepath.Join(root, "tests", "aster", "x", "hs")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.hs"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 25 {
		files = files[:25]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".hs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := hs.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".hs.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, astJSON, 0o644); err != nil {
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
			out, err := hs.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".hs")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			got, errGot := exec.Command("runghc", outPath).CombinedOutput()
			want, errWant := exec.Command("runghc", src).CombinedOutput()
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0o644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if (errGot != nil) || (errWant != nil) {
				if (errGot != nil) != (errWant != nil) {
					t.Fatalf("mismatched run errors: printed=%v want=%v", errGot, errWant)
				}
				return
			}
			if string(got) != string(want) {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
