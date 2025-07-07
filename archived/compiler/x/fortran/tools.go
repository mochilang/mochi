//go:build archived

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
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "mingw")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if path, err := exec.LookPath("gfortran"); err == nil {
				return path, nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "gcc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if path, err := exec.LookPath("gfortran"); err == nil {
				return path, nil
			}
		}
	}
	if path, err := exec.LookPath("gfortran"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("gfortran not found")
}

// EnsureFormatter verifies that a Fortran formatting tool is installed.
// It prefers fprettify but falls back to findent. If the tools are not found,
// it attempts a best-effort installation using the system package manager.
func EnsureFormatter() (string, error) {
	if path, err := exec.LookPath("fprettify"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("findent"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			cmd = exec.Command("apt-get", "install", "-y", "findent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("pip"); err == nil {
			cmd := exec.Command("pip", "install", "fprettify")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "findent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("pip3"); err == nil {
			cmd := exec.Command("pip3", "install", "fprettify")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "findent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "findent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("pip"); err == nil {
			cmd := exec.Command("pip", "install", "fprettify")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("fprettify"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("findent"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("no Fortran formatter found")
}

// Ensure verifies that gfortran and a formatting tool are available.
// It can be called by external tools to set up the environment.
func Ensure() error {
	if _, err := EnsureFortran(); err != nil {
		return err
	}
	_, err := EnsureFormatter()
	return err
}
