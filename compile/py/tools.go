package pycode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsurePython installs Python3 if missing. Useful for benchmarks and tests.
func EnsurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
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
	return cmd.Run()
}
