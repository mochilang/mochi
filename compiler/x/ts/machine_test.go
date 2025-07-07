package tscode_test

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

	tscode "mochi/compiler/x/ts"
	"mochi/parser"
	"mochi/types"
)

func TestGenerateMachineOutput(t *testing.T) {
	if err := tscode.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
	root := findRepoRoot3(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	outDir := filepath.Join(root, "tests", "machine", "x", "ts")
	_ = os.MkdirAll(outDir, 0755)

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			data, _ := os.ReadFile(src)
			prog, err := parser.Parse(src)
			if err != nil {
				writeError(outDir, name, data, err)
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				writeError(outDir, name, data, errs[0])
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := tscode.New(env, filepath.Dir(src)).Compile(prog)
			if err != nil {
				writeError(outDir, name, data, err)
				t.Fatalf("compile error: %v", err)
			}
			codePath := filepath.Join(outDir, name+".ts")
			if err := os.WriteFile(codePath, code, 0644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("deno", "run", "--quiet", "--allow-net", "--allow-read", codePath)
			cmd.Env = append(os.Environ(), "DENO_TLS_CA_STORE=system")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				writeError(outDir, name, code, fmt.Errorf("run error: %w\n%s", err, out))
				t.Fatalf("run error: %v", err)
			}
			out = bytes.TrimSpace(out)
			if err := os.WriteFile(filepath.Join(outDir, name+".out"), out, 0644); err != nil {
				t.Fatalf("write out: %v", err)
			}
		})
	}
}

func writeError(dir, name string, src []byte, err error) {
	line := extractLine(err.Error())
	var context string
	if line > 0 {
		lines := bytes.Split(src, []byte("\n"))
		start := line - 2
		if start < 1 {
			start = 1
		}
		end := line + 1
		if end > len(lines) {
			end = len(lines)
		}
		var b strings.Builder
		for i := start; i <= end; i++ {
			fmt.Fprintf(&b, "%4d: %s\n", i, lines[i-1])
		}
		context = b.String()
	}
	msg := fmt.Sprintf("line: %d\nerror: %v\n%s", line, err, context)
	path := filepath.Join(dir, name+".error")
	_ = os.WriteFile(path, []byte(msg), 0644)
}

var lineRE = regexp.MustCompile(`:(\d+):`)

func extractLine(msg string) int {
	if m := lineRE.FindStringSubmatch(msg); m != nil {
		if n, err := strconv.Atoi(m[1]); err == nil {
			return n
		}
	}
	return 0
}

func findRepoRoot3(t *testing.T) string {
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
