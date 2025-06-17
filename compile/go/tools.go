package gocode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// EnsureMochi builds the Mochi command and returns its path. It is used by
// benchmarks to compile and run programs.
func EnsureMochi() (string, error) {
	if path, err := exec.LookPath("mochi"); err == nil {
		return path, nil
	}
	fmt.Println("🔧 Mochi not found, building...")
	home := os.Getenv("HOME")
	if home == "" {
		return "", fmt.Errorf("HOME not set")
	}
	out := filepath.Join(home, "bin", "mochi")
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", err
	}
	cmd := exec.Command("go", "build", "-o", out, "./cmd/mochi")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	return out, nil
}
