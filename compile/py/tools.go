package pycode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsurePython installs Python3 if missing. Useful for benchmarks and tests.
func EnsurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F40D Installing Python3 via Homebrew...")
			cmd := exec.Command("brew", "install", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F40D Installing Python3...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "python3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	return fmt.Errorf("python3 not found")
}
