//go:build slow

package rkt_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"

	rkt "mochi/aster/x/rkt"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureRacket(t *testing.T) {
	if _, err := exec.LookPath("racket"); err != nil {
		t.Skip("racket not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureRacket(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "rkt")
	outDir := filepath.Join(root, "tests", "aster", "x", "rkt")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.rkt"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	for _, f := range files {
		if filepath.Base(f) == "two-sum.rkt" {
			selected = append(selected, f)
		}
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".rkt")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := rkt.Inspect(string(data), rkt.Options{Positions: true})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".rkt.json")
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
			out, err := rkt.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".rkt")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			cmd := exec.Command("racket", outPath)
			gotBytes, _ := cmd.CombinedOutput()
			wantBytes, _ := exec.Command("racket", src).CombinedOutput()
			got := strings.ReplaceAll(string(gotBytes), outPath, "")
			want := strings.ReplaceAll(string(wantBytes), src, "")
			re := regexp.MustCompile(`:[0-9]+:[0-9]+`)
			got = re.ReplaceAllString(got, ":")
			want = re.ReplaceAllString(want, ":")
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, []byte(got), 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if got != want {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
