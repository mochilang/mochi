package bench

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// EnsureDotnet verifies that the dotnet CLI is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureDotnet() error {
	return ensureDotnet()
}

func ensureDotnet() error {
	if _, err := exec.LookPath("dotnet"); err == nil {
		return nil
	}
	fmt.Println("🔧 Installing dotnet...")
	home := os.Getenv("HOME")
	if home == "" {
		home = "/tmp"
	}
	installDir := filepath.Join(home, ".dotnet")
	cmd := exec.Command("bash", "-c", fmt.Sprintf("curl -fsSL https://dot.net/v1/dotnet-install.sh | bash -s -- --install-dir %s", installDir))
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	dotnetSrc := filepath.Join(installDir, "dotnet")
	if _, err := os.Stat(dotnetSrc); err == nil {
		if err := exec.Command("install", "-m", "755", dotnetSrc, "/usr/local/bin/dotnet").Run(); err == nil {
			return nil
		}
		dest := filepath.Join(home, "bin", "dotnet")
		if err := os.MkdirAll(filepath.Dir(dest), 0755); err == nil {
			if err := os.Rename(dotnetSrc, dest); err == nil {
				return nil
			}
		}
	}
	return fmt.Errorf("failed to install dotnet")
}
