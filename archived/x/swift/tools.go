//go:build archived

package swiftcode

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

// EnsureSwift verifies that the Swift toolchain is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureSwift() error {
	if _, err := exec.LookPath("swiftc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swift")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		return fmt.Errorf("swift toolchain missing; install Xcode command line tools")
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "swiftlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
		cmd := exec.Command("bash", "-c", "curl -sSL https://swift.org/install.sh | bash")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "swift")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "swift")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
	}
	return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}

// EnsureSwiftFormat verifies that the swift-format tool is installed.
// It tries a best-effort installation using common package managers.
func EnsureSwiftFormat() error {
	if _, err := exec.LookPath("swift-format"); err == nil {
		return nil
	}
	// best-effort install: ensure swift is present first
	_ = EnsureSwift()
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swift-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "swift-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "swift-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "swift-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("swift-format"); err == nil {
		return nil
	}
	return fmt.Errorf("swift-format not found")
}

// Format attempts to pretty-print Swift code using the official
// `swift-format` tool. If the tool isn't installed or formatting
// fails, the input is returned unchanged.
func Format(code []byte) []byte {
	if err := EnsureSwiftFormat(); err == nil {
		fmtPath, _ := exec.LookPath("swift-format")
		cmd := exec.Command(fmtPath, "format", "-")
		cmd.Stdin = bytes.NewReader(code)
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
	// fallback: convert tabs to spaces and trim trailing whitespace
	s := strings.ReplaceAll(string(code), "\t", "  ")
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
