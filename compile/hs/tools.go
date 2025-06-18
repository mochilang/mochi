package hscode

import (
	"fmt"
	"os"
	"os/exec"
	rruntime "runtime"
)

// EnsureHaskell verifies that ghc/runhaskell is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureHaskell() error {
	if _, err := exec.LookPath("runhaskell"); err == nil {
		return nil
	}
	switch rruntime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	}
	if _, err := exec.LookPath("runhaskell"); err == nil {
		return nil
	}
	return fmt.Errorf("ghc not installed")
}
