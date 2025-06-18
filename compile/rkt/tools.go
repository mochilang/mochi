package rktcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureRacket verifies that the Racket binary is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or
// Homebrew on macOS.
// It is safe to call from tests.
func EnsureRacket() error {
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		fmt.Println("🔧 Installing Racket via apt-get...")
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		fmt.Println("🍺 Installing Racket via Homebrew...")
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	return fmt.Errorf("racket not found")
}
