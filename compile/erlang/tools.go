package erlcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureErlang verifies that the Erlang toolchain is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
// Tests can call this to skip if installation fails.
func EnsureErlang() error { return ensureErlang() }

func ensureErlang() error {
	if _, err := exec.LookPath("escript"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("erl"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("escript"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("erl"); err == nil {
		return nil
	}
	return fmt.Errorf("erlang not installed")
}
