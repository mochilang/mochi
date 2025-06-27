package ccode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureCC verifies that a C compiler is installed. It attempts to
// install GCC on Linux or LLVM on macOS if missing.
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

// EnsureClangFormat verifies that clang-format is installed. It attempts a
// best-effort installation using common package managers.
func EnsureClangFormat() error {
	if _, err := exec.LookPath("clang-format"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "clang-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "clang-format")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "llvm")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
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

// FormatC runs clang-format on the given source code if available. If the
// formatter is missing or fails, the input is returned unchanged.
func FormatC(src []byte) []byte {
	if err := EnsureClangFormat(); err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	path, err := exec.LookPath("clang-format")
	if err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
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
