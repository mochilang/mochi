package cppcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
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

// FormatCPP runs clang-format on the given source code if available.
// If clang-format is not found or fails, the input is returned unchanged.
func FormatCPP(src []byte) []byte {
	path, err := exec.LookPath("clang-format")
	if err != nil {
		return src
	}
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
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
