//go:build slow

package ocaml_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"

	"mochi/compiler/x/ocaml"
	"mochi/parser"
	"mochi/types"
)

func repoRoot(t *testing.T) string {
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

// writeError writes a detailed error report.
func writeError(dir, base string, src []byte, stage string, errOut []byte) {
	line := 0
	re := regexp.MustCompile(`line ([0-9]+)`)
	if m := re.FindSubmatch(errOut); m != nil {
		line, _ = strconv.Atoi(string(m[1]))
	}
	var context strings.Builder
	lines := strings.Split(string(src), "\n")
	if line > 0 {
		start := line - 3
		if start < 0 {
			start = 0
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			fmt.Fprintf(&context, "%4d | %s\n", i+1, lines[i])
		}
	}
	msg := fmt.Sprintf("stage: %s\nerror:\n%s\n\nContext (around line %d):\n%s", stage, errOut, line, context.String())
	_ = os.WriteFile(filepath.Join(dir, base+".error"), []byte(msg), 0644)
}

func TestPrograms(t *testing.T) {
	if _, err := exec.LookPath("ocamlc"); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := repoRoot(t)
	files, err := filepath.Glob(filepath.Join(root, "tests", "vm", "valid", "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "ocaml")
	_ = os.MkdirAll(outDir, 0755)
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".mochi")
		t.Run(base, func(t *testing.T) {
			src, _ := os.ReadFile(f)
			errPath := filepath.Join(outDir, base+".error")
			os.Remove(errPath)
			prog, err := parser.Parse(f)
			if err != nil {
				writeError(outDir, base, src, "parse", []byte(err.Error()))
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, base, src, "type", []byte(errs[0].Error()))
				return
			}
			code, err := ocaml.New(env).Compile(prog, f)
			if err != nil {
				writeError(outDir, base, src, "compile", []byte(err.Error()))
				return
			}
			mlPath := filepath.Join(outDir, base+".ml")
			if err := os.WriteFile(mlPath, code, 0644); err != nil {
				t.Fatalf("write ml: %v", err)
			}
			exe := filepath.Join(outDir, base)
			cmd := exec.Command("ocamlc", mlPath, "-o", exe)
			if out, err := cmd.CombinedOutput(); err != nil {
				writeError(outDir, base, code, "compile", out)
				return
			}
			runCmd := exec.Command(exe)
			var out bytes.Buffer
			runCmd.Stdout = &out
			runCmd.Stderr = &out
			if err := runCmd.Run(); err != nil {
				writeError(outDir, base, code, "run", out.Bytes())
				return
			}
			if err := os.WriteFile(filepath.Join(outDir, base+".out"), out.Bytes(), 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
			os.Remove(errPath)
		})
	}
}
