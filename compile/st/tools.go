package stcode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureSmalltalk checks for the gst command and attempts installation if missing.
func EnsureSmalltalk() error {
	if _, err := exec.LookPath("gst"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🔧 Installing GNU Smalltalk via apt-get...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			cmd = exec.Command("apt-get", "install", "-y", "gnu-smalltalk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("gst"); err != nil {
			if err := buildSmalltalk(); err != nil {
				return err
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing GNU Smalltalk via Homebrew...")
			cmd := exec.Command("brew", "install", "gnu-smalltalk")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
		}
	default:
		return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
	}
	if _, err := exec.LookPath("gst"); err == nil {
		return nil
	}
	return fmt.Errorf("gst not found")
}

// buildSmalltalk downloads and builds GNU Smalltalk from source as a fallback.
func buildSmalltalk() error {
	if os.Getenv("MOCHI_ST_BUILD") == "" {
		return fmt.Errorf("gnu smalltalk missing")
	}
	url := "https://ftp.gnu.org/gnu/smalltalk/smalltalk-3.2.5.tar.gz"
	tarPath := filepath.Join(os.TempDir(), "smalltalk-3.2.5.tar.gz")
	fmt.Println("🔧 Building GNU Smalltalk from source...")
	cmd := exec.Command("curl", "-fsSL", "-o", tarPath, url)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	srcDir := filepath.Join(os.TempDir(), "smalltalk-src")
	if err := os.RemoveAll(srcDir); err != nil {
		return err
	}
	if err := os.MkdirAll(srcDir, 0755); err != nil {
		return err
	}
	cmd = exec.Command("tar", "-xzf", tarPath, "-C", srcDir, "--strip-components", "1")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	configure := exec.Command("./configure", "--prefix=/usr/local")
	configure.Dir = srcDir
	configure.Stdout = os.Stdout
	configure.Stderr = os.Stderr
	if err := configure.Run(); err != nil {
		return err
	}
	makeCmd := exec.Command("make", "-j4")
	makeCmd.Dir = srcDir
	makeCmd.Stdout = os.Stdout
	makeCmd.Stderr = os.Stderr
	if err := makeCmd.Run(); err != nil {
		return err
	}
	installCmd := exec.Command("make", "install")
	installCmd.Dir = srcDir
	installCmd.Stdout = os.Stdout
	installCmd.Stderr = os.Stderr
	if err := installCmd.Run(); err != nil {
		return err
	}
	return nil
}
