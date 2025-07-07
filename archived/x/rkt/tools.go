//go:build archived

package rktcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureRacket verifies that the Racket binary is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or
// Homebrew on macOS.
// It is safe to call from tests.
func EnsureRacket() error {
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		fmt.Println("🔧 Installing Racket via apt-get...")
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		fmt.Println("🍺 Installing Racket via Homebrew...")
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		fmt.Println("🔧 Installing Racket via Chocolatey/Scoop...")
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "racket")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("racket"); err == nil {
		return nil
	}
	return fmt.Errorf("racket not found")
}

// EnsureRacketFmt checks that the raco formatter is available.
// It falls back to EnsureRacket which installs the full toolchain
// using common package managers when possible.
func EnsureRacketFmt() error {
	if _, err := exec.LookPath("raco"); err == nil {
		return nil
	}
	if err := EnsureRacket(); err != nil {
		return err
	}
	if _, err := exec.LookPath("raco"); err == nil {
		return nil
	}
	return fmt.Errorf("raco not found")
}

// FormatRacket runs "raco fmt" on the given source code when available.
// If the formatter is missing or fails, tabs are replaced with two spaces
// and a trailing newline is ensured.
func FormatRacket(src []byte) []byte {
	if err := EnsureRacketFmt(); err == nil {
		cmd := exec.Command("raco", "fmt", "--stdin")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			res := out.Bytes()
			if len(res) == 0 || res[len(res)-1] != '\n' {
				res = append(res, '\n')
			}
			return res
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
