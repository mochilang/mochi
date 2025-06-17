package phpcode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsurePHP ensures the php command is available, attempting installation if missing.
func EnsurePHP() error {
	if _, err := exec.LookPath("php"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("apt-get"); err == nil {
		cmd := exec.Command("apt-get", "update")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		cmd = exec.Command("apt-get", "install", "-y", "php-cli")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	}
	return fmt.Errorf("php not installed")
}
