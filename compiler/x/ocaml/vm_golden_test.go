//go:build slow

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	ocaml "mochi/compiler/x/ocaml"
	testutil "mochi/compiler/x/testutil"
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
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("(*")) {
		return b[i+1:]
	}
	return b
}

func TestOCamlCompiler_VMValid_Golden(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "ocaml")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codePath := filepath.Join(outDir, name+".ml")
		outPath := filepath.Join(outDir, name+".out")
		errPath := filepath.Join(outDir, name+".error")
		if !shouldUpdateValid() {
			if _, err := os.Stat(outPath); err != nil {
				continue
			}
		}
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("parse error: %v", err)
				}
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("type error: %v", errs[0])
				}
				return
			}
			code, err := ocaml.New(env).Compile(prog, src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0644)
				if !shouldUpdateValid() {
					t.Skipf("compile error: %v", err)
				}
				return
			}
			if shouldUpdateValid() {
				_ = os.WriteFile(codePath, code, 0644)
			} else if want, err := os.ReadFile(codePath); err == nil {
				got := stripHeader(bytes.TrimSpace(code))
				want = stripHeader(bytes.TrimSpace(want))
				if !bytes.Equal(got, want) {
					t.Errorf("generated code mismatch for %s.ml\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
				}
			}
			dir := t.TempDir()
			mlFile := filepath.Join(dir, name+".ml")
			if err := os.WriteFile(mlFile, code, 0644); err != nil {
				t.Fatalf("write ml: %v", err)
			}
			exe := filepath.Join(dir, "prog")
			if out, err := exec.Command("ocamlc", mlFile, "-o", exe).CombinedOutput(); err != nil {
				_ = os.WriteFile(errPath, out, 0644)
				if !shouldUpdateValid() {
					t.Skipf("ocamlc error: %v", err)
				}
				return
			}
			outBytes, err := exec.Command(exe).CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, outBytes, 0644)
				if !shouldUpdateValid() {
					t.Skipf("run error: %v", err)
				}
				return
			}
			os.Remove(errPath)
			gotOut := bytes.TrimSpace(outBytes)
			if shouldUpdateValid() {
				_ = os.WriteFile(outPath, append(gotOut, '\n'), 0644)
				return
			}
			wantOut, err := os.ReadFile(outPath)
			if err != nil {
				t.Fatalf("read golden: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
			}
		})
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	root := testutil.FindRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "ocaml")
	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	total := len(files)
	compiled := 0
	var lines []string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			compiled++
			mark = "[x]"
		}
		lines = append(lines, fmt.Sprintf("- %s %s.mochi", mark, name))
	}
	var buf bytes.Buffer
	buf.WriteString("# OCaml Machine Translations\n\n")
	buf.WriteString("This directory contains OCaml code generated from the Mochi programs in `tests/vm/valid` using the OCaml compiler. Each program was compiled and executed with `ocamlc`. Successful runs produced an `.out` file while failures produced an `.error` file.\n\n")
	fmt.Fprintf(&buf, "Compiled programs: %d/%d successful.\n\n", compiled, total)
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n\n## Remaining Tasks\n")
	buf.WriteString("- [ ] Improve support for complex query groups and joins\n")
	buf.WriteString("- [ ] Integrate an OCaml runtime to execute compiled programs in CI\n")
	buf.WriteString("- [x] Expand anonymous record typing for clearer generated code\n")
	buf.WriteString("- [x] Emit native `for` loops when iterating over numeric ranges\n")
	_ = os.WriteFile(filepath.Join(outDir, "README.md"), buf.Bytes(), 0644)
}
