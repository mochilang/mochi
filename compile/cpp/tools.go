package cppcode

import (
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
