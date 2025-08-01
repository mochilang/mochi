//go:build slow

package c_test

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	c "mochi/aster/x/c"
)

func ensureGCC(t *testing.T) {
	if _, err := exec.LookPath("gcc"); err != nil {
		t.Skip("gcc not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureGCC(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "c")
	outDir := filepath.Join(root, "tests", "aster", "x", "c")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.c"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	if len(files) > 75 {
		files = files[:75]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".c")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := c.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".c.json")
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
			out, err := c.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".c")
			if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			if shouldUpdate() {
				// already written above
			}
			tmp := t.TempDir()
			bin := filepath.Join(tmp, name)
			if outc, err := exec.Command("gcc", outPath, "-o", bin).CombinedOutput(); err != nil {
				t.Fatalf("compile printed: %v\n%s", err, outc)
			}
			cmd := exec.Command(bin)
			cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
			got, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			tmp2 := t.TempDir()
			bin2 := filepath.Join(tmp2, name+"_orig")
			if outc, err := exec.Command("gcc", src, "-o", bin2).CombinedOutput(); err != nil {
				t.Skipf("compile original: %v\n%s", err, outc)
				return
			}
			cmd2 := exec.Command(bin2)
			cmd2.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
			want, err := cmd2.CombinedOutput()
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
