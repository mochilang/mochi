package hscode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureHaskell ensures ghc/runhaskell is installed, installing via apt if possible.
func EnsureHaskell() error {
	if _, err := exec.LookPath("runhaskell"); err == nil {
		return nil
	}
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
		return cmd.Run()
	}
	return fmt.Errorf("ghc not installed")
}
