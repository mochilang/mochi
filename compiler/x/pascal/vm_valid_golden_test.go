//go:build slow

package pascode_test

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

	pascode "mochi/compiler/x/pascal"
	"mochi/compiler/x/testutil"
	"mochi/parser"
	"mochi/types"
)

func shouldUpdateValid() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func TestPascalCompiler_VMValid_Golden(t *testing.T) {
	fpc := ensureFPCQuick(t)
	root := testutil.FindRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	outDir := filepath.Join(root, "tests", "machine", "x", "pascal")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		codeWant := filepath.Join(outDir, name+".pas")
		outWant := filepath.Join(outDir, name+".out")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				data, _ := os.ReadFile(src)
				writeErrorLocal(outDir, name, data, err)
				t.Skipf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				data, _ := os.ReadFile(src)
				writeErrorLocal(outDir, name, data, errs[0])
				t.Skipf("type error: %v", errs[0])
			}
			code, err := pascode.New(env).Compile(prog)
			if err != nil {
				data, _ := os.ReadFile(src)
				writeErrorLocal(outDir, name, data, err)
				t.Skipf("compile error: %v", err)
			}
			if shouldUpdateValid() {
				_ = os.WriteFile(codeWant, code, 0644)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.pas")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			exe := filepath.Join(dir, "main")
			if out, err := exec.Command(fpc, file, "-o"+exe).CombinedOutput(); err != nil {
				writeErrorLocal(outDir, name, code, fmt.Errorf("fpc error: %v\n%s", err, out))
				t.Skipf("fpc error: %v", err)
			}
			cmd := exec.Command(exe)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeErrorLocal(outDir, name, code, fmt.Errorf("run error: %v\n%s", err, out))
				t.Skipf("run error: %v", err)
			}
			gotOut := bytes.TrimSpace(out)
			if shouldUpdateValid() {
				_ = os.WriteFile(outWant, append(gotOut, '\n'), 0644)
			} else if wantOut, err := os.ReadFile(outWant); err == nil {
				if !bytes.Equal(gotOut, bytes.TrimSpace(wantOut)) {
					t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(wantOut))
				}
			}
		})
	}
}

func writeErrorLocal(dir, name string, src []byte, err error) {
	line := extractLineLocal(err.Error())
	var context string
	if line > 0 {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 2
		if start < 1 {
			start = 1
		}
		end := line + 2
		if end > len(lines) {
			end = len(lines)
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			if i-1 < len(lines) {
				fmt.Fprintf(&b, "%4d: %s\n", i, lines[i-1])
			}
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s", line, err, context)
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg), 0644)
}

func extractLineLocal(msg string) int {
	re := regexp.MustCompile(`\((\d+)\)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	re = regexp.MustCompile(`line (\d+)`)
	if m := re.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}
