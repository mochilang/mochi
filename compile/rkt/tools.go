package rktcode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureRacket verifies that the Racket binary is installed. If missing,
// it attempts a best-effort installation via apt-get.
// It is safe to call from tests.
func EnsureRacket() error {
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	fmt.Println("🔧 Installing Racket...")
	cmd := exec.Command("apt-get", "update")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	cmd = exec.Command("apt-get", "install", "-y", "racket")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
