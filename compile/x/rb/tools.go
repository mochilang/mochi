package rbcode

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"runtime"
)

// EnsureRuby verifies that the ruby binary is installed. If missing, it
// attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
func EnsureRuby() error {
	if _, err := exec.LookPath("ruby"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "ruby-full")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ruby")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "ruby")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("ruby"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "ruby")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("ruby"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("ruby"); err == nil {
		return nil
	}
	return fmt.Errorf("ruby not installed")
}

// EnsureRubocop installs the rubocop gem if it is not already available.
func EnsureRubocop() error {
	if _, err := exec.LookPath("rubocop"); err == nil {
		return nil
	}
	if err := EnsureRuby(); err != nil {
		return err
	}
	if _, err := exec.LookPath("gem"); err == nil {
		fmt.Println("\U0001F48E Installing RuboCop gem...")
		cmd := exec.Command("gem", "install", "--no-document", "rubocop")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		_ = cmd.Run()
	}
	if _, err := exec.LookPath("rubocop"); err == nil {
		return nil
	}
	return fmt.Errorf("rubocop not installed")
}

// FormatRB runs RuboCop in auto-correct mode on the provided Ruby source code
// if available. When RuboCop is unavailable or formatting fails, the input is
// returned unchanged with a trailing newline ensured.
func FormatRB(src []byte) []byte {
	if err := EnsureRubocop(); err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	cmd := exec.Command("rubocop", "-A", "--stdin", "stdin.rb", "--stderr")
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = io.Discard
	if err := cmd.Run(); err == nil && out.Len() > 0 {
		res := out.Bytes()
		if len(res) == 0 || res[len(res)-1] != '\n' {
			res = append(res, '\n')
		}
		return res
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
