//go:build slow

package swift_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

func compileAndRunSwiftSrc(t *testing.T, swiftExe string, code []byte) ([]byte, error) {
	dir := t.TempDir()
	file := filepath.Join(dir, "main.swift")
	if err := os.WriteFile(file, code, 0644); err != nil {
		return nil, err
	}
	exe := filepath.Join(dir, "main")
	if out, err := exec.Command(swiftExe, file, "-o", exe).CombinedOutput(); err != nil {
		return out, err
	}
	out, err := exec.Command(exe).CombinedOutput()
	if err != nil {
		return out, err
	}
	return out, nil
}
