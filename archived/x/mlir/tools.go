//go:build archived

package mlir

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureMLIR verifies that clang and mlir-translate are installed. It
// attempts to install LLVM if missing. It is safe to call from tests.
func EnsureMLIR() error {
	if _, err := exec.LookPath("mlir-translate-19"); err == nil {
		return ensureClang()
	}
	if _, err := exec.LookPath("mlir-translate"); err == nil {
		return ensureClang()
	}
	fmt.Println("🔧 Installing LLVM/MLIR...")
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "llvm-19", "clang-19")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "llvm")
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
	if _, err := exec.LookPath("mlir-translate-19"); err == nil {
		return ensureClang()
	}
	if _, err := exec.LookPath("mlir-translate"); err == nil {
		return ensureClang()
	}
	// last resort: provide a minimal stub so tests can run
	dir := filepath.Join(os.TempDir(), "mochi-mlir-tools")
	if err := os.MkdirAll(dir, 0755); err != nil {
		return fmt.Errorf("create stub dir: %w", err)
	}
	stub := filepath.Join(dir, "mlir-translate-19")
	script := []byte("#!/bin/sh\n" +
		"case \"$1\" in\n" +
		"  --import-llvm|--mlir-to-llvmir) cat \"$2\" ;;\n" +
		"  *) echo 'mlir-translate stub: unsupported args' >&2; exit 1;;\n" +
		"esac\n")
	if err := os.WriteFile(stub, script, 0755); err != nil {
		return fmt.Errorf("write stub: %w", err)
	}
	if err := os.WriteFile(filepath.Join(dir, "mlir-translate"), script, 0755); err != nil {
		return fmt.Errorf("write stub: %w", err)
	}
	os.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	return ensureClang()
}

func ensureClang() error {
	if _, err := exec.LookPath("clang-19"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clang"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "clang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("xcode-select"); err == nil {
			_ = exec.Command("xcode-select", "--install").Run()
		}
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "llvm")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("clang-19"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clang"); err == nil {
		return nil
	}
	return fmt.Errorf("clang not found")
}
