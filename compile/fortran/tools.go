package ftncode

import (
    "fmt"
    "os"
    "os/exec"
)

// EnsureFortran checks for gfortran and tries to install it via apt if missing.
func EnsureFortran() (string, error) {
    if path, err := exec.LookPath("gfortran"); err == nil {
        return path, nil
    }
    if _, err := exec.LookPath("apt-get"); err == nil {
        cmd := exec.Command("apt-get", "update")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err != nil {
            return "", err
        }
        cmd = exec.Command("apt-get", "install", "-y", "gfortran")
        cmd.Stdout = os.Stdout
        cmd.Stderr = os.Stderr
        if err := cmd.Run(); err != nil {
            return "", err
        }
    }
    if path, err := exec.LookPath("gfortran"); err == nil {
        return path, nil
    }
    return "", fmt.Errorf("gfortran not found")
}
