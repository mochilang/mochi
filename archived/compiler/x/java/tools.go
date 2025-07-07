//go:build archived

package javacode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureJava verifies that the Java runtime is installed. It attempts a
// best-effort installation using apt-get on Linux, Homebrew on macOS or
// Chocolatey/Scoop on Windows.
func EnsureJava() error {
	if _, err := exec.LookPath("java"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jre-headless")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "openjdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("java"); err == nil {
		return nil
	}
	return fmt.Errorf("java not found")
}

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

// EnsureGoogleJavaFormat installs the google-java-format command if needed and
// returns the path to the executable.
func EnsureGoogleJavaFormat() (string, error) {
	if p, err := exec.LookPath("google-java-format"); err == nil {
		return p, nil
	}

	version := "1.21.0"
	dir := filepath.Join(os.TempDir(), "google-java-format-"+version)
	jar := filepath.Join(dir, "google-java-format.jar")
	if _, err := os.Stat(jar); os.IsNotExist(err) {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return "", err
		}
		url := fmt.Sprintf("https://github.com/google/google-java-format/releases/download/v%s/google-java-format-%s-all-deps.jar", version, version)
		cmd := exec.Command("curl", "-fsSL", "-o", jar, url)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return "", err
		}
	}

	bin := filepath.Join(dir, "google-java-format")
	if runtime.GOOS == "windows" {
		bin += ".bat"
	}
	if _, err := os.Stat(bin); os.IsNotExist(err) {
		script := fmt.Sprintf("#!/bin/sh\nexec java -jar %s \"$@\"\n", jar)
		if runtime.GOOS == "windows" {
			script = fmt.Sprintf("@echo off\r\njava -jar %s %%*\r\n", jar)
		}
		if err := os.WriteFile(bin, []byte(script), 0755); err != nil {
			return "", err
		}
	}
	return bin, nil
}

// FormatJava formats the given Java source using the `google-java-format`
// command if available. If the formatter is unavailable or fails, tabs are
// expanded to four spaces and a trailing newline is ensured.
func FormatJava(src []byte) []byte {
	path, err := EnsureGoogleJavaFormat()
	if err == nil {
		cmd := exec.Command(path, "-")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
			if len(src) == 0 || src[len(src)-1] != '\n' {
				src = append(src, '\n')
			}
			return src
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
