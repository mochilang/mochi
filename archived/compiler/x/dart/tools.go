//go:build archived

package dartcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureDart verifies that the Dart binary is installed and attempts to install
// it if missing. It is safe to call from tests.
func EnsureDart() error {
	return ensureDart()
}

func ensureDart() error {
	if _, err := exec.LookPath("dart"); err == nil {
		return nil
	}
	fmt.Println("\U0001F3AF Installing Dart...")
	home := os.Getenv("HOME")
	if home == "" {
		home = "/tmp"
	}
	installDir := filepath.Join(home, ".dart")
	if err := os.MkdirAll(installDir, 0755); err != nil {
		return err
	}
	arch := "x64"
	if runtime.GOARCH == "arm64" {
		arch = "arm64"
	}
	osName := "linux"
	if runtime.GOOS == "darwin" {
		osName = "macos"
	} else if runtime.GOOS == "windows" {
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "dart-sdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("dart"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "dart-sdk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("dart"); err == nil {
				return nil
			}
		}
		osName = "windows"
	} else if runtime.GOOS != "linux" {
		return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
	}
	file := fmt.Sprintf("dartsdk-%s-%s-release.zip", osName, arch)
	url := fmt.Sprintf("https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/%s", file)
	zipPath := filepath.Join(installDir, file)
	cmd := exec.Command("curl", "-fsSL", "-o", zipPath, url)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	cmd = exec.Command("unzip", "-q", zipPath, "-d", installDir)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	dartSrc := filepath.Join(installDir, "dart-sdk", "bin", "dart")
	if err := exec.Command("install", "-m", "755", dartSrc, "/usr/local/bin/dart").Run(); err == nil {
		return nil
	}
	dest := filepath.Join(home, "bin", "dart")
	if err := os.MkdirAll(filepath.Dir(dest), 0755); err == nil {
		if err := os.Rename(dartSrc, dest); err == nil {
			return nil
		}
	}
	return fmt.Errorf("failed to install dart")
}

// FormatDart runs `dart format` on the given source code if the dart binary is
// available. If formatting fails or dart is not installed, the input is
// returned unchanged. A trailing newline is ensured in the returned slice.
func FormatDart(src []byte) []byte {
	path, err := exec.LookPath("dart")
	if err == nil {
		cmd := exec.Command(path, "format", "--output", "show", "-")
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
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}

// Ensure provides a generic entry point for verifying required tools.
// It simply calls EnsureDart so other packages can depend on dartcode.Ensure.
func Ensure() error { return EnsureDart() }
