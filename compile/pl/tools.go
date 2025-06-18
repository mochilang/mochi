package plcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureSWIPL verifies that the SWI-Prolog interpreter is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
func EnsureSWIPL() error {
	if _, err := exec.LookPath("swipl"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("swipl"); err == nil {
		return nil
	}
	return fmt.Errorf("swipl not found")
}
