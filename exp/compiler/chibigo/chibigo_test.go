package main_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

var chibigoPath string

func TestMain(m *testing.M) {
	root, err := repoRoot()
	if err != nil {
		panic(err)
	}
	tmpDir, err := os.MkdirTemp("", "chibigo-test-")
	if err != nil {
		panic(err)
	}
	defer os.RemoveAll(tmpDir)

	chibigoPath = filepath.Join(tmpDir, "chibigo")
	cmd := exec.Command("go", "build", "-o", chibigoPath, "./exp/compiler/chibigo")
	cmd.Dir = root
	if output, err := cmd.CombinedOutput(); err != nil {
		panic(string(output) + err.Error())
	}

	os.Exit(m.Run())
}

func TestCompileAndRun(t *testing.T) {
	dir := t.TempDir()
	mainPath := writeFile(t, dir, "main.c", `int main() { return 42; }`)
	outPath := filepath.Join(dir, "app.bin")

	runCmd(t, nil, chibigoPath, "-o", outPath, mainPath)

	if code := runBinary(t, outPath); code != 42 {
		t.Fatalf("expected exit code 42, got %d", code)
	}
}

func TestCompileMultipleFiles(t *testing.T) {
	dir := t.TempDir()
	mainPath := writeFile(t, dir, "main.c", `int helper(); int main() { return helper(); }`)
	helperPath := writeFile(t, dir, "helper.c", `int helper() { return 7; }`)
	outPath := filepath.Join(dir, "multi.bin")

	runCmd(t, nil, chibigoPath, "-o", outPath, mainPath, helperPath)

	if code := runBinary(t, outPath); code != 7 {
		t.Fatalf("expected exit code 7, got %d", code)
	}
}

func TestPreprocessOnly(t *testing.T) {
	dir := t.TempDir()
	_ = writeFile(t, dir, "value.h", `#define VALUE 9`)
	mainPath := writeFile(t, dir, "main.c", `#include "value.h"
int main() { return VALUE; }`)
	outPath := filepath.Join(dir, "preprocessed.c")

	runCmd(t, nil, chibigoPath, "-E", "-I"+dir, "-o", outPath, mainPath)

	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read output: %v", err)
	}
	if !bytes.Contains(data, []byte("VALUE")) && !bytes.Contains(data, []byte("9")) {
		t.Fatalf("expected macro expansion in output, got: %s", data)
	}
}

func TestEmitAssembly(t *testing.T) {
	cmd := exec.Command(chibigoPath, "-S", "-o-", "-xc", "-")
	cmd.Stdin = bytes.NewBufferString("int main() {}")
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("compile assembly: %v\n%s", err, output)
	}
	if !bytes.Contains(output, []byte("main:")) {
		t.Fatalf("expected assembly label for main, got: %s", output)
	}
}

func TestCompileChibiccTestCorpus(t *testing.T) {
	root, err := repoRoot()
	if err != nil {
		t.Fatalf("repo root: %v", err)
	}
	testDir := filepath.Join(root, "exp", "compiler", "chibicc", "test")
	entries, err := os.ReadDir(testDir)
	if err != nil {
		t.Fatalf("read test dir: %v", err)
	}

	skip := map[string]bool{
		"pragma-once.c": true,
	}

	for _, e := range entries {
		if e.IsDir() || filepath.Ext(e.Name()) != ".c" || skip[e.Name()] {
			continue
		}
		src := filepath.Join(testDir, e.Name())
		out := filepath.Join(t.TempDir(), e.Name()+".o")
		cmd := exec.Command(chibigoPath, "-I"+filepath.Join(root, "exp", "compiler", "chibicc", "include"), "-I"+testDir, "-c", "-o", out, src)
		if output, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("compile %s failed: %v\n%s", src, err, output)
		}
		if _, err := os.Stat(out); err != nil {
			t.Fatalf("missing output for %s: %v", src, err)
		}
	}
}
func runCmd(t *testing.T, stdin []byte, name string, args ...string) {
	t.Helper()
	cmd := exec.Command(name, args...)
	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}
	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("command %s %v failed: %v\n%s", name, args, err, output)
	}
}

func runBinary(t *testing.T, path string) int {
	t.Helper()
	cmd := exec.Command(path)
	if err := cmd.Run(); err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			return exitErr.ExitCode()
		}
		t.Fatalf("run binary: %v", err)
	}
	return 0
}

func writeFile(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0600); err != nil {
		t.Fatalf("write file: %v", err)
	}
	return path
}

func repoRoot() (string, error) {
	wd, err := os.Getwd()
	if err != nil {
		return "", err
	}
	return filepath.Clean(filepath.Join(wd, "..", "..", "..")), nil
}
