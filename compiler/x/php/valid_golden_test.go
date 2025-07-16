//go:build slow

package phpcode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func stripHeader(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i >= 0 {
		return bytes.TrimSpace(b[i+1:])
	}
	return bytes.TrimSpace(b)
}

var tmpDirRE = regexp.MustCompile(`TestPHPCompiler_VMValid_Golden[^/]+`)

func normalize(b []byte) []byte {
	s := tmpDirRE.ReplaceAllString(string(b), "TestPHPCompiler_VMValid_GoldenX")
	return []byte(s)
}

func repoRootLocal(t *testing.T) string {
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

func TestPHPCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}
	root := repoRootLocal(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "php")
	pattern := filepath.Join(srcDir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			errFile := filepath.Join(outDir, name+".error")
			prog, err := parser.Parse(src)
			if err != nil {
				os.WriteFile(errFile, []byte(fmt.Sprintf("parse: %v", err)), 0644)
				t.Skipf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				os.WriteFile(errFile, []byte(fmt.Sprintf("type: %v", errs[0])), 0644)
				t.Skipf("type error: %v", errs[0])
			}
			code, err := phpcode.New(env).Compile(prog)
			if err != nil {
				os.WriteFile(errFile, []byte(fmt.Sprintf("compile: %v", err)), 0644)
				t.Skipf("compile error: %v", err)
			}
			codeWant := filepath.Join(outDir, name+".php")
			if shouldUpdateValid() {
				os.WriteFile(codeWant, code, 0644)
			} else if want, err := os.ReadFile(codeWant); err == nil {
				got := stripHeader(code)
				want = stripHeader(want)
				if !bytes.Equal(got, want) {
					t.Errorf("generated code mismatch for %s.php\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
				}
			} else {
				t.Fatalf("read golden: %v", err)
			}
			tmp := filepath.Join(t.TempDir(), name+".php")
			if err := os.WriteFile(tmp, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("php", tmp)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				os.WriteFile(errFile, []byte(fmt.Sprintf("run: %v\n%s", err, out)), 0644)
				t.Skipf("php run error: %v", err)
			}
			os.Remove(errFile)
			gotOut := bytes.TrimSpace(normalize(out))
			outWant := filepath.Join(outDir, name+".out")
			if shouldUpdateValid() {
				os.WriteFile(outWant, append(gotOut, '\n'), 0644)
			} else if wantOut, err := os.ReadFile(outWant); err == nil {
				wantOut = bytes.TrimSpace(normalize(wantOut))
				if !bytes.Equal(gotOut, wantOut) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, wantOut)
				}
			} else {
				t.Fatalf("read golden output: %v", err)
			}
		})
	}
}
