//go:build slow

package cpp_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	cpp "mochi/aster/x/cpp"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureGPP(t *testing.T) {
	if _, err := exec.LookPath("g++"); err != nil {
		t.Skip("g++ not installed")
	}
}

func repoRootPrint(t *testing.T) string {
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

func TestPrint_Golden(t *testing.T) {
	ensureGPP(t)
	root := repoRootPrint(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "cpp")
	outDir := filepath.Join(root, "tests", "aster", "x", "cpp")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.cpp"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	idx := 0
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".cpp")
		if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
			continue
		}
		if name == "bench_block" {
			continue
		}
		idx++
		if idx > 25 {
			break
		}
		selected = append(selected, f)
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".cpp")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
				t.Skip("skip due to compile error")
				return
			}
			prog, err := cpp.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".cpp.json")
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
			out, err := cpp.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".cpp")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0o644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			bin := filepath.Join(outDir, name)
			if outb, err := exec.Command("g++", outPath, "-std=c++20", "-o", bin).CombinedOutput(); err != nil {
				t.Fatalf("compile printed: %v\n%s", err, outb)
			}
			defer os.Remove(bin)
			got, err := exec.Command(bin).CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			refBin := filepath.Join(outDir, name+"_ref")
			if outb, err := exec.Command("g++", src, "-std=c++20", "-o", refBin).CombinedOutput(); err != nil {
				t.Fatalf("compile original: %v\n%s", err, outb)
			}
			defer os.Remove(refBin)
			want, err := exec.Command(refBin).CombinedOutput()
			if err != nil {
				t.Fatalf("run original: %v\n%s", err, want)
			}
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0o644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if string(got) != string(want) {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
