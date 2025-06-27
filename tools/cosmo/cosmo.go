//go:build cosmo

package cosmo

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
)

// cosmocc returns the path to the Cosmopolitan compiler binary.
func cosmocc() string {
	if dir := os.Getenv("COSMO_DIR"); dir != "" {
		return filepath.Join(dir, "bin", "cosmocc")
	}
	return filepath.Join("tools", "cosmo", "cosmo", "bin", "cosmocc")
}

// CompileAndRun compiles the given C code using cosmocc and returns its stdout.
func CompileAndRun(code string) (string, error) {
	tmpDir, err := os.MkdirTemp("", "cosmo")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(tmpDir)
	src := filepath.Join(tmpDir, "prog.c")
	if err := os.WriteFile(src, []byte(code), 0o644); err != nil {
		return "", err
	}
	exe := filepath.Join(tmpDir, "a.out")
	cmd := exec.Command(cosmocc(), src, "-o", exe, "-static", "-s")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	out, err := exec.Command("sh", exe).Output()
	if err != nil {
		return "", err
	}
	return string(bytes.TrimSpace(out)), nil
}

// CompileToFile compiles the source code to the given output path using cosmocc.
func CompileToFile(code, out string) error {
	tmp, err := os.CreateTemp("", "cosmo-*.c")
	if err != nil {
		return err
	}
	src := tmp.Name()
	if _, err := tmp.Write([]byte(code)); err != nil {
		tmp.Close()
		return err
	}
	tmp.Close()
	defer os.Remove(src)
	cmd := exec.Command(cosmocc(), src, "-o", out, "-static", "-s")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	return nil
}
