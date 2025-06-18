package zigcode

import (
	"fmt"
	"os/exec"
)

// EnsureZig verifies that the Zig compiler is installed.
func EnsureZig() (string, error) {
	if path, err := exec.LookPath("zig"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("zig compiler not found")
}
