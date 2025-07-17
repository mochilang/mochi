//go:build slow

package pl_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pl "mochi/compiler/x/pl"
	"mochi/golden"
	"mochi/parser"
)

func TestPrologCompiler(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "pl")
	os.MkdirAll(outDir, 0o755)
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) { compileAndRun(t, src, outDir, name) })
	}
}

func TestPrologCompiler_GoldenOutput(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}

	compileFn := func(src string) ([]byte, error) {
		prog, err := parser.Parse(src)
		if err != nil {
			return nil, fmt.Errorf("\u274c parse error: %w", err)
		}
		code, err := pl.New(nil).Compile(prog)
		if err != nil {
			return nil, fmt.Errorf("\u274c compile error: %w", err)
		}

		dir := t.TempDir()
		file := filepath.Join(dir, "main.pl")
		if err := os.WriteFile(file, code, 0644); err != nil {
			return nil, fmt.Errorf("write error: %w", err)
		}
		cmd := exec.Command("swipl", "-q", file)
		if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
			cmd.Stdin = bytes.NewReader(data)
		}
		out, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("\u274c swipl error: %w\n%s", err, out)
		}
		gotOut := bytes.TrimSpace(out)
		wantOutPath := strings.TrimSuffix(src, ".mochi") + ".out"
		if want, err := os.ReadFile(wantOutPath); err == nil {
			want = bytes.TrimSpace(want)
			if !bytes.Equal(gotOut, want) {
				return nil, fmt.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", filepath.Base(wantOutPath), gotOut, want)
			}
		}
		return bytes.TrimSpace(code), nil
	}

	golden.Run(t, "tests/vm/valid", ".mochi", ".pl.out", compileFn)
}

func compileAndRun(t *testing.T, src, outDir, name string) {
	data, err := os.ReadFile(src)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)

	prog, err := parser.ParseString(string(data))
	if err != nil {
		writeError(outDir, name, string(data), err)
		return
	}
	code, err := pl.New(nil).Compile(prog)
	if err != nil {
		writeError(outDir, name, string(data), err)
		return
	}
	codePath := filepath.Join(outDir, name+".pl")
	os.WriteFile(codePath, code, 0o644)

	// compare generated code with golden
	wantCodePath := strings.TrimSuffix(src, ".mochi") + ".pl.out"
	if want, err := os.ReadFile(wantCodePath); err == nil {
		got := bytes.TrimSpace(code)
		want = bytes.TrimSpace(want)
		if !bytes.Equal(got, want) {
			t.Errorf("code mismatch for %s.pl.out", name)
		}
	}

	cmd := exec.Command("swipl", "-q", codePath)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writeError(outDir, name, string(data), fmt.Errorf("run: %v\n%s", err, buf.String()))
		return
	}
	outBytes := bytes.TrimSpace(buf.Bytes())
	outPath := filepath.Join(outDir, name+".out")
	os.WriteFile(outPath, outBytes, 0o644)

	wantOutPath := strings.TrimSuffix(src, ".mochi") + ".out"
	if want, err := os.ReadFile(wantOutPath); err == nil {
		want = bytes.TrimSpace(want)
		if !bytes.Equal(outBytes, want) {
			writeError(outDir, name, string(data), fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", outBytes, want))
			return
		}
	}
}

func writeError(dir, name, src string, err error) {
	lines := strings.Split(src, "\n")
	msg := err.Error()
	ln := 0
	if idx := strings.Index(msg, "line "); idx != -1 {
		fmt.Sscanf(msg[idx:], "line %d", &ln)
	}
	var ctx string
	if ln > 0 {
		start := ln - 2
		if start < 0 {
			start = 0
		}
		end := ln + 1
		if end > len(lines) {
			end = len(lines)
		}
		for i := start; i < end; i++ {
			ctx += lines[i] + "\n"
		}
	}
	errPath := filepath.Join(dir, name+".error")
	os.WriteFile(errPath, []byte(msg+"\n"+ctx), 0o644)
}

func findRepoRoot(t *testing.T) string {
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
