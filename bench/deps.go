package bench

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// EnsureDeps verifies that Mochi, Deno and Python3 are installed.
// If Mochi or Deno are missing, it attempts to install them.
// Python3 will be installed via apt-get if missing.
func EnsureDeps() (string, error) {
	mochiBin, err := ensureMochi()
	if err != nil {
		return "", err
	}
	if err := ensurePython(); err != nil {
		return "", err
	}
	if err := ensureDeno(); err != nil {
		return "", err
	}
	if err := ensureDotnet(); err != nil {
		return "", err
	}
	return mochiBin, nil
}

func ensureMochi() (string, error) {
	if path, err := exec.LookPath("mochi"); err == nil {
		return path, nil
	}
	fmt.Println("🔧 Mochi not found, building...")
	home := os.Getenv("HOME")
	if home == "" {
		return "", fmt.Errorf("HOME not set")
	}
	out := filepath.Join(home, "bin", "mochi")
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", err
	}
	cmd := exec.Command("go", "build", "-o", out, "./cmd/mochi")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	return out, nil
}

func ensurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	fmt.Println("🐍 Installing Python3...")
	cmd := exec.Command("apt-get", "update")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	cmd = exec.Command("apt-get", "install", "-y", "python3")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func ensureDeno() error {
	if _, err := exec.LookPath("deno"); err == nil {
		return nil
	}
	fmt.Println("🦕 Installing Deno...")
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
	// attempt to move deno to /usr/local/bin
	denoSrc := filepath.Join(installDir, "bin", "deno")
	if _, err := os.Stat(denoSrc); err == nil {
		if err := exec.Command("install", "-m", "755", denoSrc, "/usr/local/bin/deno").Run(); err == nil {
			return nil
		}
		// fallback to HOME/bin
		dest := filepath.Join(home, "bin", "deno")
		if err := os.MkdirAll(filepath.Dir(dest), 0755); err == nil {
			if err := os.Rename(denoSrc, dest); err == nil {
				return nil
			}
		}
	}
	return fmt.Errorf("failed to install deno")
}

// EnsureDeno verifies that the Deno binary is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureDeno() error {
	return ensureDeno()
}
