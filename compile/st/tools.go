package stcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureSmalltalk checks for the gst command and attempts installation if missing.
func EnsureSmalltalk() error {
	if _, err := exec.LookPath("gst"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🔧 Installing GNU Smalltalk via apt-get...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "gnu-smalltalk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing GNU Smalltalk via Homebrew...")
			cmd := exec.Command("brew", "install", "gnu-smalltalk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
		}
	default:
		return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
	}
	if _, err := exec.LookPath("gst"); err == nil {
		return nil
	}
	return fmt.Errorf("gst not found")
}
