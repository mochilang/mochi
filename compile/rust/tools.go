package rscode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureRust verifies that the Rust toolchain is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureRust() error {
	return ensureRust()
}

func ensureRust() error {
	if _, err := exec.LookPath("rustc"); err == nil {
		return nil
	}
	fmt.Println("\U0001F980 Installing Rust...")
	cmd := exec.Command("sh", "-c", "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
