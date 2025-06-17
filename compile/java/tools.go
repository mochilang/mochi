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

// EnsureJava verifies that the Java runtime can actually execute. Some
// systems, notably macOS, may provide a placeholder `java` binary that exits
// with an error if no JRE is installed. This function attempts to run
// `java -version` and falls back to EnsureJavac if it fails. It returns an
// error if the runtime remains unavailable.
func EnsureJava() error {
	cmd := exec.Command("java", "-version")
	if err := cmd.Run(); err == nil {
		return nil
	}
	if err := EnsureJavac(); err != nil {
		return err
	}
	if err := exec.Command("java", "-version").Run(); err != nil {
		return fmt.Errorf("java runtime not found")
	}
	return nil
}
