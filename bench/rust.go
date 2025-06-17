package bench

import (
	"fmt"
	"os"
	"os/exec"
)

// ensureRust checks for the rust toolchain and installs it via rustup if missing.
func ensureRust() error {
	if _, err := exec.LookPath("rustc"); err == nil {
		return nil
	}
	fmt.Println("🦀 Installing Rust...")
	cmd := exec.Command("sh", "-c", "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

// EnsureRust verifies that the Rust toolchain is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureRust() error {
	return ensureRust()
}
