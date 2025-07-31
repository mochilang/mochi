//go:build slow

package swift_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	swift "mochi/aster/x/swift"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureSwift(t *testing.T) string {
	if env := os.Getenv("SWIFT"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("swiftc"); err == nil {
		return p
	}
	if p, err := exec.LookPath("swift"); err == nil {
		return p
	}
	t.Skip("swift not found")
	return ""
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

func compileAndRunSwiftSrc(t *testing.T, swiftExe string, code []byte) ([]byte, error) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command(swiftExe, file, "-o", exe).CombinedOutput(); err != nil {
		return out, err
	}
	cmd := exec.Command(exe)
	env := append(os.Environ(), "MOCHI_NOW_SEED=1")
	cmd.Env = env
	out, err := cmd.CombinedOutput()
	if err != nil {
		return out, err
	}
	return out, nil
}

func TestPrint_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := repoRootPrint(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "swift")
	outDir := filepath.Join(root, "tests", "aster", "x", "swift")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.swift"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)
	var selected []string
	for _, f := range files {
		base := filepath.Base(f)
		if base == "two-sum.swift" {
			selected = append(selected, f)
		}
	}
	files = selected

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".swift")
		t.Run(name, func(t *testing.T) {
			if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
				t.Skipf("skip %s due to compile error", name)
				return
			}
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
                       prog, err := swift.Inspect(string(data), swift.Option{Comments: true})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".swift.json")
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
			outSrc, err := swift.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".swift")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(outSrc), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			got, err := compileAndRunSwiftSrc(t, swiftExe, []byte(outSrc))
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			want, err := compileAndRunSwiftSrc(t, swiftExe, data)
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
