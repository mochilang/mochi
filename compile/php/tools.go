package phpcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsurePHP ensures the php command is available, attempting installation if missing.
func EnsurePHP() error {
	if _, err := exec.LookPath("php"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "php-cli")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
		if _, err := exec.LookPath("apk"); err == nil {
			cmd := exec.Command("apk", "add", "--no-cache", "php-cli")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "php")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	}
	if _, err := exec.LookPath("php"); err == nil {
		return nil
	}
	return fmt.Errorf("php not installed")
}
