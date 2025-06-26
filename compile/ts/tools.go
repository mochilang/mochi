package tscode

import (
	"bytes"
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

// formatWithDeno runs `deno fmt` to format TypeScript source.
func formatWithDeno(src []byte) ([]byte, error) {
	if err := ensureDeno(); err != nil {
		return nil, err
	}
	cmd := exec.Command("deno", "fmt", "--ext", "ts", "-")
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	return out.Bytes(), nil
}
