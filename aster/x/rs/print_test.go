//go:build slow

package rs_test

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	rs "mochi/aster/x/rs"
)

func ensureRustc(t *testing.T) {
	if _, err := exec.LookPath("rustc"); err != nil {
		t.Skip("rustc not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureRustc(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "rs")
	outDir := filepath.Join(root, "tests", "aster", "x", "rs")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.rs"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 75 {
		files = files[:75]
	}

	for _, src := range files {
		if _, err := os.Stat(strings.TrimSuffix(src, ".rs") + ".error"); err == nil {
			continue
		}
		name := strings.TrimSuffix(filepath.Base(src), ".rs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := rs.Inspect(string(data), rs.Option{Positions: true})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".rs.json")
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
			out, err := rs.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".rs")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			tmp := t.TempDir()
			bin := filepath.Join(tmp, name)
			if outc, err := exec.Command("rustc", outPath, "-o", bin).CombinedOutput(); err != nil {
				t.Fatalf("compile printed: %v\n%s", err, outc)
			}
			got, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			tmp2 := t.TempDir()
			bin2 := filepath.Join(tmp2, name+"_orig")
			if outc, err := exec.Command("rustc", src, "-o", bin2).CombinedOutput(); err != nil {
				t.Fatalf("compile original: %v\n%s", err, outc)
			}
			want, err := exec.Command(bin2).CombinedOutput()
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
