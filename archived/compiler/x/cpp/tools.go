//go:build archived

package cppcode

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

// EnsureCPP verifies that a C++ compiler is installed. If missing, it attempts
// to install one using apt-get on Linux or Homebrew/Xcode tools on macOS.
func EnsureCPP() (string, error) {
	if path, err := exec.LookPath("g++"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("clang++"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("c++"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		fmt.Println("🔧 Installing g++...")
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "build-essential")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("xcode-select"); err == nil {
			fmt.Println("🔧 Installing Xcode Command Line Tools...")
			_ = exec.Command("xcode-select", "--install").Run()
		}
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing LLVM via Homebrew...")
			_ = exec.Command("brew", "install", "llvm").Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🔧 Installing g++ via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "mingw")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🔧 Installing g++ via Scoop...")
			cmd := exec.Command("scoop", "install", "mingw")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("g++"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("clang++"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("c++"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("C++ compiler not found")
}

// EnsureClangFormat checks for clang-format and attempts to install it if missing.
func EnsureClangFormat() error {
	if _, err := exec.LookPath("clang-format"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F527 Installing clang-format...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "clang-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F37A Installing clang-format via Homebrew...")
			cmd := exec.Command("brew", "install", "clang-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("\U0001F527 Installing clang-format via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "llvm")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("\U0001F527 Installing clang-format via Scoop...")
			cmd := exec.Command("scoop", "install", "llvm")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("clang-format"); err == nil {
		return nil
	}
	return fmt.Errorf("clang-format not found")
}

// FormatCPP runs clang-format on the given source code if available.
// If clang-format is not found or fails, the input is returned unchanged.
func FormatCPP(src []byte) []byte {
	if err := EnsureClangFormat(); err == nil {
		if path, err := exec.LookPath("clang-format"); err == nil {
			cmd := exec.Command(path, "-style=LLVM")
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
	}
	return simpleFormatCPP(src)
}

// simpleFormatCPP indents braces with two spaces when clang-format is unavailable.
func simpleFormatCPP(src []byte) []byte {
	var out bytes.Buffer
	indent := 0
	scanner := bufio.NewScanner(bytes.NewReader(src))
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if strings.HasPrefix(line, "#") {
			out.WriteString(line)
			out.WriteByte('\n')
			continue
		}
		if strings.HasPrefix(line, "}") && indent > 0 {
			indent--
		}
		for i := 0; i < indent; i++ {
			out.WriteString("  ")
		}
		out.WriteString(line)
		out.WriteByte('\n')
		if strings.HasSuffix(line, "{") {
			indent++
		}
	}
	res := out.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res
}
