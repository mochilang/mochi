package schemecode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureScheme verifies that chibi-scheme is installed. On Linux it attempts a
// best-effort installation using apt-get, while on macOS it tries Homebrew.
func EnsureScheme() (string, error) {
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("chibi-scheme not found")
}
