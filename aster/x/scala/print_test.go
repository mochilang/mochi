//go:build slow

package scala_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	scala "mochi/aster/x/scala"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func repoRootPrint(t *testing.T) string {
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

func ensureScala(t *testing.T) {
	if _, err := exec.LookPath("scalac"); err != nil {
		t.Skip("scalac not installed")
	}
	if _, err := exec.LookPath("scala"); err != nil {
		t.Skip("scala not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureScala(t)
	root := repoRootPrint(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "scala")
	outDir := filepath.Join(root, "tests", "aster", "x", "scala")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.scala"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	for _, f := range files {
		if filepath.Base(f) == "two-sum.scala" {
			selected = append(selected, f)
		}
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".scala")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := scala.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".scala.json")
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
			out, err := scala.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".scala")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			tmp := t.TempDir()
			if compOut, err := exec.Command("scalac", "-d", tmp, outPath).CombinedOutput(); err != nil {
				t.Fatalf("compile printed: %v\n%s", err, compOut)
			}
			cmd := exec.Command("scala", "-cp", tmp, "Main")
			got, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			want, err := exec.Command("scala", src).CombinedOutput()
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
