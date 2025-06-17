package javacode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureJavac verifies that the Java compiler is installed. If missing, it attempts
// a best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureJavac() error {
	if _, err := exec.LookPath("javac"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("javac"); err == nil {
		return nil
	}
	return fmt.Errorf("javac not found")
}
