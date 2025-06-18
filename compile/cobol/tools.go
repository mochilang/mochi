package cobolcode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureCOBOL ensures the cobc compiler is installed.
func EnsureCOBOL() error {
	if _, err := exec.LookPath("cobc"); err == nil {
		return nil
	}
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
	return fmt.Errorf("cobc not installed")
}
