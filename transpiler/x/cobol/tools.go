//go:build slow

package cobol

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureCOBOL verifies that the cobc compiler is installed, attempting
// installation with common package managers if possible.
func EnsureCOBOL() error {
	if _, err := exec.LookPath("cobc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "gnu-cobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	default:
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("cobc"); err == nil {
		return nil
	}
	return fmt.Errorf("cobc not installed")
}
