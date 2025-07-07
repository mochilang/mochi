//go:build archived

package fscode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureDotnet verifies that the dotnet CLI is installed and attempts to
// install it if missing. It is safe to call from tests.
func EnsureDotnet() error { return ensureDotnet() }

func ensureDotnet() error {
	if _, err := exec.LookPath("dotnet"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "--cask", "dotnet-sdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("dotnet"); err == nil {
				return nil
			}
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			for _, pkg := range []string{"dotnet-sdk-8.0", "dotnet-sdk-7.0", "dotnet-sdk-6.0"} {
				cmd = exec.Command("apt-get", "install", "-y", pkg)
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
				if _, err := exec.LookPath("dotnet"); err == nil {
					return nil
				}
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "dotnet-sdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("dotnet"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "dotnet-sdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("dotnet"); err == nil {
				return nil
			}
		}
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

// EnsureFantomas verifies that the Fantomas formatter is installed.
// It attempts a best-effort installation via "dotnet tool install" if missing.
// This function is safe to call from tests.
func EnsureFantomas() error { return ensureFantomas() }

func ensureFantomas() error {
	if _, err := exec.LookPath("fantomas"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("dotnet"); err != nil {
		if err := ensureDotnet(); err != nil {
			return err
		}
	}
	if err := exec.Command("dotnet", "fantomas", "--version").Run(); err == nil {
		return nil
	}
	fmt.Println("🔧 Installing Fantomas formatter via dotnet tool...")
	cmd := exec.Command("dotnet", "tool", "install", "-g", "fantomas")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		cmd = exec.Command("dotnet", "tool", "update", "-g", "fantomas")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		_ = cmd.Run()
	}
	if err := exec.Command("dotnet", "fantomas", "--version").Run(); err == nil {
		return nil
	}
	if _, err := exec.LookPath("fantomas"); err == nil {
		return nil
	}
	return fmt.Errorf("fantomas not installed")
}

// FormatFS runs the Fantomas formatter on the given source code if available.
// If Fantomas is not found or fails, the input is returned unchanged.
func FormatFS(src []byte) []byte {
	formatter := ""
	args := []string{}
	if path, err := exec.LookPath("fantomas"); err == nil {
		formatter = path
	} else if path, err := exec.LookPath("dotnet"); err == nil {
		formatter = path
		args = append(args, "fantomas")
	}
	if formatter != "" {
		tmp, err := os.CreateTemp("", "fsfmt-*.fs")
		if err == nil {
			_ = tmp.Close()
			if err := os.WriteFile(tmp.Name(), src, 0644); err == nil {
				cmd := exec.Command(formatter, append(args, tmp.Name())...)
				if err := cmd.Run(); err == nil {
					if data, err := os.ReadFile(tmp.Name()); err == nil {
						os.Remove(tmp.Name())
						if len(data) == 0 || data[len(data)-1] != '\n' {
							data = append(data, '\n')
						}
						return data
					}
				}
			}
			os.Remove(tmp.Name())
		}
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
