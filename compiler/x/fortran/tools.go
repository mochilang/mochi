//go:build slow

package ftncode

import (
	"fmt"
	"os/exec"
)

// EnsureFortran checks for the gfortran compiler in PATH.
// If not found, an error is returned so tests can skip.
func EnsureFortran() (string, error) {
	if path, err := exec.LookPath("gfortran"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("gfortran not found")
}
