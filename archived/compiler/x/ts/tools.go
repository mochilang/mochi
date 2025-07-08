//go:build slow

package tscode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureDeno verifies that the Deno binary is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureDeno() error {
	return ensureDeno()
}

func ensureDeno() error {
	if _, err := exec.LookPath("deno"); err == nil {
		return nil
	}
	fmt.Println("\U0001F985 Installing Deno...")
	if runtime.GOOS == "windows" {
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "deno")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "deno")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	}
	home := os.Getenv("HOME")
	if home == "" {
		home = "/tmp"
	}
	installDir := filepath.Join(home, ".deno")
	cmd := exec.Command("sh", "-c", fmt.Sprintf("curl -fsSL https://deno.land/install.sh | DENO_INSTALL=%s sh", installDir))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	denoSrc := filepath.Join(installDir, "bin", "deno")
	if _, err := os.Stat(denoSrc); err == nil {
		if err := exec.Command("install", "-m", "755", denoSrc, "/usr/local/bin/deno").Run(); err == nil {
			return nil
		}
		dest := filepath.Join(home, "bin", "deno")
		if err := os.MkdirAll(filepath.Dir(dest), 0755); err == nil {
			if err := os.Rename(denoSrc, dest); err == nil {
				return nil
			}
		}
	}
	return fmt.Errorf("failed to install deno")
}

// EnsureFormatter verifies that either Deno or npx Prettier is available so
// generated TypeScript can be formatted. It is safe to call from tests.
func EnsureFormatter() error {
	if err := ensureDeno(); err == nil {
		return nil
	}
	if _, err := exec.LookPath("npx"); err == nil {
		return nil
	}
	return fmt.Errorf("deno or npx not found")
}

// Ensure provides a simple entry point for other packages to verify
// the TypeScript toolchain is available.
func Ensure() error { return EnsureFormatter() }

// EnsureTSLanguageServer installs the TypeScript language server if missing.
// The implementation tries npm or pnpm.
func EnsureTSLanguageServer() error {
	if _, err := exec.LookPath("typescript-language-server"); err == nil {
		return nil
	}
	installers := []struct {
		bin  string
		args []string
	}{
		{"npm", []string{"install", "-g", "typescript", "typescript-language-server"}},
		{"pnpm", []string{"add", "-g", "typescript", "typescript-language-server"}},
	}
	for _, inst := range installers {
		if _, err := exec.LookPath(inst.bin); err == nil {
			fmt.Println("\U0001F985 Installing TS language server via", inst.bin, "...")
			cmd := exec.Command(inst.bin, inst.args...)
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			break
		}
	}
	if _, err := exec.LookPath("typescript-language-server"); err == nil {
		return nil
	}
	return fmt.Errorf("typescript-language-server not found")
}
