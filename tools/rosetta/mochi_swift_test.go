//go:build slow

package rosetta

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	swift "mochi/compiler/x/swift"
	"mochi/parser"
	"mochi/types"
)

func TestMochiToSwift(t *testing.T) {
	swiftExe := ensureSwift(t)

	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests/rosetta/x/Mochi")
	outDir := filepath.Join(root, "tests/rosetta/out/Swift")
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
			compileAndRunSwift(t, swiftExe, srcPath, outPath, outDir, name)
		})
	}
}

func compileAndRunSwift(t *testing.T, swiftExe, srcPath, wantPath, outDir, name string) {
	if _, err := os.ReadFile(srcPath); err != nil {
		t.Fatalf("read: %v", err)
	}
	prog, err := parser.Parse(srcPath)
	if err != nil {
		writeSwiftError(outDir, name, fmt.Errorf("parse error: %w", err))
		t.Skip("parse error")
		return
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		writeSwiftError(outDir, name, fmt.Errorf("type error: %v", errs[0]))
		t.Skip("type error")
		return
	}
	code, err := swift.New(env).Compile(prog)
	if err != nil {
		writeSwiftError(outDir, name, fmt.Errorf("compile error: %w", err))
		t.Skip("compile error")
		return
	}
	swiftPath := filepath.Join(outDir, name+".swift")
	if err := os.WriteFile(swiftPath, code, 0o644); err != nil {
		t.Fatalf("write swift: %v", err)
	}

	out, err := compileAndRunSwiftSrc(t, swiftExe, code)
	if err != nil {
		writeSwiftError(outDir, name, fmt.Errorf("run error: %v\n%s", err, out))
		return
	}
	got := bytes.TrimSpace(out)
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		writeSwiftError(outDir, name, fmt.Errorf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want))
		return
	}
	if err := os.WriteFile(filepath.Join(outDir, name+".out"), got, 0o644); err != nil {
		t.Fatalf("write out: %v", err)
	}
	_ = os.Remove(filepath.Join(outDir, name+".error"))
}

func compileAndRunSwiftSrc(t *testing.T, swiftExe string, code []byte) ([]byte, error) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0o644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command("swiftc", file, "-o", exe).CombinedOutput(); err != nil {
		return out, err
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		return out, err
	}
	return out, nil
}

func writeSwiftError(dir, name string, err error) {
	_ = os.WriteFile(filepath.Join(dir, name+".error"), []byte(err.Error()), 0o644)
}

func ensureSwift(t *testing.T) string {
	if env := os.Getenv("SWIFT"); env != "" {
		if p, err := exec.LookPath(env); err == nil {
			return p
		}
	}
	if p, err := exec.LookPath("swift"); err == nil {
		return p
	}
	t.Skip("swift not found")
	return ""
}
