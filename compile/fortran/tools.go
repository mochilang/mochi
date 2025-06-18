package ftncode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureFortran verifies that gfortran is available. If missing, it attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureFortran() (string, error) {
	if path, err := exec.LookPath("gfortran"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "gfortran")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				if path, err := exec.LookPath("gfortran"); err == nil {
					return path, nil
				}
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "gcc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				if path, err := exec.LookPath("gfortran"); err == nil {
					return path, nil
				}
			}
		}
	}
	if path, err := exec.LookPath("gfortran"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("gfortran not found")
}
