//go:build archived

package erlcode

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

// EnsureErlang verifies that the Erlang toolchain is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
// Tests can call this to skip if installation fails.
func EnsureErlang() error { return ensureErlang() }

func ensureErlang() error {
	if _, err := exec.LookPath("escript"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("erl"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("erl"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "erlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("erl"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("escript"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("erl"); err == nil {
		return nil
	}
	return fmt.Errorf("erlang not installed")
}

// EnsureFormatter verifies that the `erlfmt` tool is installed. It attempts a
// best-effort installation using common package managers. Tests can call this
// to skip if installation fails.
func EnsureFormatter() error { return ensureErlfmt() }

func ensureErlfmt() error {
	if _, err := exec.LookPath("erlfmt"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "erlfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "erlfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "erlfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("erlfmt"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "erlfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("erlfmt"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("erlfmt"); err == nil {
		return nil
	}
	return fmt.Errorf("erlfmt not installed")
}

// Format attempts to pretty-print Erlang code using the official
// `erlfmt` tool when available. If formatting fails or the tool isn't
// installed, the input is returned unchanged with a trailing newline.
func Format(src []byte) []byte {
	_ = EnsureFormatter()
	path, err := exec.LookPath("erlfmt")
	if err == nil {
		cmd := exec.Command(path, "-")
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
	s := strings.ReplaceAll(string(src), "\t", "    ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	res := buf.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res
}
