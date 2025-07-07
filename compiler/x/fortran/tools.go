package ftncode

import (
	"fmt"
	"os/exec"
)

// EnsureFortran returns the path to gfortran if installed.
func EnsureFortran() (string, error) {
	if path, err := exec.LookPath("gfortran"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("gfortran not found")
}
