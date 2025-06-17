package pycode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsurePython verifies that python3 is installed and attempts to install it via apt-get if missing.
func EnsurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	fmt.Println("🐍 Installing Python3...")
	cmd := exec.Command("apt-get", "update")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	cmd = exec.Command("apt-get", "install", "-y", "python3")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
