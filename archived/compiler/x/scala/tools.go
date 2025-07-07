//go:build archived

package scalacode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureScala verifies that the Scala compiler is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureScala() error {
	if _, err := exec.LookPath("scalac"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "scala")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		return fmt.Errorf("scalac missing; install via Homebrew or sbt")
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "scala")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
		cmd := exec.Command("bash", "-c", "curl -sS https://get.sdkman.io | bash && source $HOME/.sdkman/bin/sdkman-init.sh && sdk install scala")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "scala")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "scala")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
	}
	return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}

// FormatScala runs scalafmt on the given source code if available.
// If scalafmt is not found or fails, the input is returned unchanged.
func FormatScala(src []byte) []byte {
	if path, err := exec.LookPath("scalafmt"); err == nil {
		cmd := exec.Command(path, "--stdin")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if cmd.Run() == nil {
			src = out.Bytes()
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

// Ensure provides a simple entry point for other packages to verify the Scala
// toolchain is available.
func Ensure() error { return EnsureScala() }
