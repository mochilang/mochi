package cppcode

import (
	"fmt"
	"os/exec"
)

// EnsureCPP verifies that a C++ compiler is installed.
func EnsureCPP() (string, error) {
	if path, err := exec.LookPath("g++"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("c++"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("C++ compiler not found")
}
