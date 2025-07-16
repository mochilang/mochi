package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	phpcode "mochi/compiler/x/php"
	"mochi/parser"
	"mochi/types"
)

func TestMochiToPHP(t *testing.T) {
	if _, err := exec.LookPath("php"); err != nil {
		t.Skip("php not installed")
	}

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Php")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}

	outs, err := filepath.Glob(filepath.Join(srcDir, "*.out"))
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(outs) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}

	for _, outPath := range outs {
		name := strings.TrimSuffix(filepath.Base(outPath), ".out")
		srcPath := filepath.Join(srcDir, name+".mochi")
		if _, err := os.Stat(srcPath); err != nil {
			t.Fatalf("missing source for %s", name)
		}
		t.Run(name, func(t *testing.T) {
			compileAndRunPHP(t, srcPath, outPath, outDir, name)
		})
	}
}

func compileAndRunPHP(t *testing.T, srcPath, wantPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writePHPError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writePHPError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	code, err := phpcode.New(env).Compile(prog)
	if err != nil {
		writePHPError(outDir, name, fmt.Errorf("compile error: %w", err))
		t.Skip("compile error")
		return
	}
	phpFile := filepath.Join(outDir, name+".php")
	if err := os.WriteFile(phpFile, code, 0o644); err != nil {
		t.Fatalf("write php: %v", err)
	}

	cmd := exec.Command("php", phpFile)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		writePHPError(outDir, name, fmt.Errorf("run error: %v\n%s", err, buf.Bytes()))
		return
	}
	got := bytes.TrimSpace(buf.Bytes())
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writePHPError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func writePHPError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}
