//go:build slow

package ctrans

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureCC verifies that a C compiler is available, attempting installation
// via common package managers if necessary.
func EnsureCC() (string, error) {
	if env := os.Getenv("CC"); env != "" {
		if path, err := exec.LookPath(env); err == nil {
			return path, nil
		}
	}
	if path, err := exec.LookPath("cc"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("gcc"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("clang"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		fmt.Println("🔧 Installing GCC...")
		cmd := exec.Command("apt-get", "update")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return "", err
		}
		cmd = exec.Command("apt-get", "install", "-y", "build-essential")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return "", err
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
			fmt.Println("🔧 Installing GCC via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "mingw")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🔧 Installing GCC via Scoop...")
			cmd := exec.Command("scoop", "install", "mingw")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("cc"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("gcc"); err == nil {
		return path, nil
	}
	if path, err := exec.LookPath("clang"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("C compiler not found")
}
