package prolog_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	prolog "mochi/aster/x/prolog"
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

func ensureSWIPL(t *testing.T) {
	exe := os.Getenv("SWIPL")
	if exe == "" {
		exe = "swipl"
	}
	if _, err := exec.LookPath(exe); err != nil {
		t.Skip("swipl not installed")
	}
}

func runPL(path string) ([]byte, error) {
	exe := os.Getenv("SWIPL")
	if exe == "" {
		exe = "swipl"
	}
	cmd := exec.Command(exe, "-q", "-f", path)
	return cmd.CombinedOutput()
}

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestPrint_Golden(t *testing.T) {
	ensureSWIPL(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "pl")
	outDir := filepath.Join(root, "tests", "aster", "x", "prolog")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.pl"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".pl")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := prolog.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			js, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			js = append(js, '\n')
			jsonPath := filepath.Join(outDir, name+".prolog.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, js, 0o644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}

			printed, err := prolog.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			srcPath := filepath.Join(outDir, name+".prolog")
			if shouldUpdate() {
				if err := os.WriteFile(srcPath, []byte(printed), 0o644); err != nil {
					t.Fatalf("write src: %v", err)
				}
			}

			out, err := runPL(src)
			if err != nil {
				t.Fatalf("run src: %v\n%s", err, out)
			}
			want := strings.TrimSpace(string(out))

			gotOut, err := runPL(srcPath)
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, gotOut)
			}
			got := strings.TrimSpace(string(gotOut))
			if got != want {
				t.Fatalf("output mismatch\n--- printed ---\n%s\n--- want ---\n%s", got, want)
			}
			outPath := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				os.WriteFile(outPath, gotOut, 0o644)
			}
		})
	}
}
