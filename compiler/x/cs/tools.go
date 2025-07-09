package cscode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
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

// EnsureFormatter makes sure either the dotnet CLI or the csharpier tool
// is available for formatting generated code. It tries to install dotnet
// if missing and falls back to checking for csharpier on the PATH.
func EnsureFormatter() error { return ensureFormatter() }

func ensureFormatter() error {
	if err := ensureDotnet(); err == nil {
		return nil
	}
	if _, err := exec.LookPath("csharpier"); err == nil {
		return nil
	}
	if dotnet, err := exec.LookPath("dotnet"); err == nil {
		cmd := exec.Command(dotnet, "tool", "install", "-g", "csharpier")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		_ = cmd.Run()
		if _, err := exec.LookPath("csharpier"); err == nil {
			return nil
		}
	}
	return fmt.Errorf("no C# formatter available")
}

// FormatCS attempts to format the given C# source using the `dotnet format`
// command if available. If the formatter is unavailable or fails, the input is
// returned with tabs expanded to four spaces.
func FormatCS(src []byte) []byte {
	if os.Getenv("MOCHI_SKIP_FORMATCS") != "" {
		src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	_ = ensureFormatter()
	if dotnet, err := exec.LookPath("dotnet"); err == nil {
		dir, err := os.MkdirTemp("", "mochi_cs_fmt")
		if err == nil {
			defer os.RemoveAll(dir)
			proj := `<Project Sdk="Microsoft.NET.Sdk"><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>`
			_ = os.WriteFile(filepath.Join(dir, "fmt.csproj"), []byte(proj), 0644)
			file := filepath.Join(dir, "Program.cs")
			if err := os.WriteFile(file, src, 0644); err == nil {
				cmd := exec.Command(dotnet, "format", "fmt.csproj", "--no-restore", "--verbosity", "quiet")
				cmd.Dir = dir
				if err := cmd.Run(); err == nil {
					if out, err := os.ReadFile(file); err == nil {
						src = out
					}
				}
			}
		}
	} else if path, err := exec.LookPath("csharpier"); err == nil {
		cmd := exec.Command(path, "--write-stdout")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			src = out.Bytes()
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
