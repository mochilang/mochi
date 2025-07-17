//go:build slow

package javacode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"

	"mochi/compiler/meta"
)

// EnsureJavac verifies that the Java compiler is installed. If missing it tries
// a best-effort installation using common package managers.
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
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("javac"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("javac"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("javac"); err == nil {
		return nil
	}
	return fmt.Errorf("javac not found")
}

// FormatJava ensures a header is prepended to src and tabs are replaced with
// spaces. A trailing newline is also ensured.
func FormatJava(src []byte) []byte {
	src = append(meta.Header("//"), src...)
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	lines := bytes.Split(src, []byte("\n"))
	for i, line := range lines {
		lines[i] = bytes.TrimRight(line, " ")
	}
	src = bytes.Join(lines, []byte("\n"))
	if len(src) == 0 || src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
