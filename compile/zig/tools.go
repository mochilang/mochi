package zigcode

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureZig verifies that the Zig compiler is installed.
func EnsureZig() (string, error) {
	if path, err := exec.LookPath("zig"); err == nil {
		return path, nil
	}
	if _, err := os.Stat("../../zig-x86_64-linux-0.14.1/zig"); err == nil {
		return "../../zig-x86_64-linux-0.14.1/zig", nil
	}
	return "", fmt.Errorf("zig not installed")
}
