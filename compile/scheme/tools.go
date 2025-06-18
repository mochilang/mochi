package schemecode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureScheme checks for chibi-scheme and tries to install it via apt if missing.
func EnsureScheme() (string, error) {
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
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
		if err := cmd.Run(); err != nil {
			return "", err
		}
	}
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("chibi-scheme not found")
}
