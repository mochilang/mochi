//go:build slow

package ocaml_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	ocaml "mochi/aster/x/ocaml"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureOCaml(t *testing.T) string {
	if p, err := exec.LookPath("ocaml"); err == nil {
		return p
	}
	if p, err := exec.LookPath("ocamlc"); err == nil {
		return p
	}
	t.Skip("ocaml not installed")
	return ""
}

func TestPrint_Golden(t *testing.T) {
	ocamlPath := ensureOCaml(t)
	if ocamlPath == "" {
		return
	}
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "ocaml")
	outDir := filepath.Join(root, "tests", "aster", "x", "ocaml")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.ml"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 120 {
		files = files[:120]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".ml")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := ocaml.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".ocaml.json")
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
			out, err := ocaml.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".ml")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			got, gotErr := exec.Command(ocamlPath, outPath).CombinedOutput()
			want, wantErr := exec.Command(ocamlPath, src).CombinedOutput()
			if (gotErr != nil) != (wantErr != nil) {
				t.Fatalf("run mismatch: gotErr=%v wantErr=%v\nGot:%s\nWant:%s", gotErr, wantErr, got, want)
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
