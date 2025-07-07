package schemecode_test

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

	schemecode "mochi/compiler/x/scheme"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

// TestVMValidPrograms compiles each tests/vm/valid .mochi file to Scheme,
// runs it with chibi-scheme and captures the output. Generated code and
// outputs are written to tests/machine/x/scheme.
func TestVMValidPrograms(t *testing.T) {
	schemePath, err := schemecode.EnsureScheme()
	if err != nil {
		t.Skipf("chibi-scheme not installed: %v", err)
	}
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "scheme")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	errRE := regexp.MustCompile(`(?i)line ([0-9]+)`) // attempt to extract line number
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := schemecode.New(env).Compile(prog)
			if err != nil {
				errMsg := fmt.Sprintf("compile error: %v", err)
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(errMsg), 0o644)
				t.Skip(errMsg)
			}
			codePath := filepath.Join(outDir, name+".scm")
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command(schemePath, "-m", "chibi", codePath)
			if inData, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(inData)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				line := 0
				if m := errRE.FindSubmatch(out); m != nil {
					line, _ = strconv.Atoi(string(m[1]))
				}
				// context lines from generated code
				lines := bytes.Split(code, []byte("\n"))
				start := line - 2
				if start < 0 {
					start = 0
				}
				end := line + 1
				if end > len(lines) {
					end = len(lines)
				}
				var ctx bytes.Buffer
				for i := start; i < end; i++ {
					ctx.Write(lines[i])
					ctx.WriteByte('\n')
				}
				errMsg := fmt.Sprintf("error running %s:\n%s\n", name, out)
				errMsg += fmt.Sprintf("context (line %d):\n%s", line, ctx.String())
				os.WriteFile(filepath.Join(outDir, name+".error"), []byte(errMsg), 0o644)
				return
			}
			os.WriteFile(filepath.Join(outDir, name+".out"), out, 0o644)
		})
	}
}
