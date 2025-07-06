//go:build slow

package cobolcode_test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	cobolcode "mochi/compile/x/cobol"
	"mochi/parser"
	"mochi/runtime/vm"
	cob2mochi "mochi/tools/any2mochi/x/cobol"
	"mochi/types"
)

func TestCobol_RoundtripVM(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/vm/valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	var allErrs []string

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		var errMsg string
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				errMsg = fmt.Sprintf("%s: parse error: %v", name, err)
				t.Log(errMsg)
				return
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				errMsg = fmt.Sprintf("%s: type error: %v", name, errs[0])
				t.Log(errMsg)
				return
			}
			var code []byte
			func() {
				defer func() {
					if r := recover(); r != nil {
						errMsg = fmt.Sprintf("%s: compile panic: %v", name, r)
					}
				}()
				var err error
				code, err = cobolcode.New(env).Compile(prog)
				if err != nil && errMsg == "" {
					errMsg = fmt.Sprintf("%s: compile error: %v", name, err)
				}
			}()
			if errMsg != "" {
				t.Log(errMsg)
				return
			}
			mochiSrc, err := cob2mochi.Convert(string(code))
			if err != nil {
				errMsg = fmt.Sprintf("%s: convert error: %v", name, err)
				t.Log(errMsg)
				return
			}
			prog2, err := parser.ParseString(string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("%s: parse2 error: %v", name, err)
				t.Log(errMsg)
				return
			}
			env2 := types.NewEnv(nil)
			if errs := types.Check(prog2, env2); len(errs) > 0 {
				errMsg = fmt.Sprintf("%s: type2 error: %v", name, errs[0])
				t.Log(errMsg)
				return
			}
			p2, err := vm.CompileWithSource(prog2, env2, string(mochiSrc))
			if err != nil {
				errMsg = fmt.Sprintf("%s: vm compile error: %v", name, err)
				t.Log(errMsg)
				return
			}
			var out bytes.Buffer
			m := vm.New(p2, &out)
			if rErr := m.Run(); rErr != nil {
				if ve, ok := rErr.(*vm.VMError); ok {
					errMsg = fmt.Sprintf("%s: vm run error:\n%s", name, ve.Format(p2))
				} else {
					errMsg = fmt.Sprintf("%s: vm run error: %v", name, rErr)
				}
				t.Log(errMsg)
				return
			}
			// compare output if golden file exists
			wantPath := strings.TrimSuffix(src, ".mochi") + ".out"
			if data, err := os.ReadFile(wantPath); err == nil {
				got := bytes.TrimSpace(out.Bytes())
				want := bytes.TrimSpace(data)
				if !bytes.Equal(got, want) {
					errMsg = fmt.Sprintf("%s: output mismatch", name)
					t.Log(errMsg)
					return
				}
			}
		})
		if errMsg != "" {
			allErrs = append(allErrs, errMsg)
		}
	}

	writeErrorsMarkdown(filepath.Join(root, "tests/any2mochi/cobol"), allErrs)
}

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func writeErrorsMarkdown(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		buf.WriteString("None\n")
	} else {
		for _, e := range errs {
			buf.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
}
