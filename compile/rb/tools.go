package rbcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureRuby verifies that the ruby binary is installed. If missing, it
// attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
func EnsureRuby() error {
	if _, err := exec.LookPath("ruby"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "ruby-full")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ruby")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("ruby"); err == nil {
		return nil
	}
	return fmt.Errorf("ruby not installed")
}
