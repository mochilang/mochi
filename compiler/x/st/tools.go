//go:build slow

package st

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
			_ = run(exec.Command("apt-get", "update"))
			if err := run(exec.Command("apt-get", "install", "-y", "gnu-smalltalk")); err != nil {
				fmt.Println("⚠️ package missing; adding jammy repo")
				repo := []byte("deb http://archive.ubuntu.com/ubuntu jammy main universe\n")
				_ = os.WriteFile("/etc/apt/sources.list.d/jammy.list", repo, 0644)
				_ = run(exec.Command("apt-get", "update"))
				_ = run(exec.Command("apt-get", "install", "-y", "gnu-smalltalk"))
			}
			if _, err := exec.LookPath("gst"); err == nil {
				return nil
			}
			fmt.Println("⚠️ gst still missing; building from source")
		}
		if err := buildSmalltalkFromSource(); err != nil {
			return err
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing GNU Smalltalk via Homebrew...")
			if err := run(exec.Command("brew", "install", "gnu-smalltalk")); err != nil {
				return err
			}
		} else {
			return fmt.Errorf("brew not found")
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🔧 Installing GNU Smalltalk via Chocolatey...")
			_ = run(exec.Command("choco", "install", "-y", "gnu-smalltalk"))
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🔧 Installing GNU Smalltalk via Scoop...")
			_ = run(exec.Command("scoop", "install", "gnu-smalltalk"))
		} else {
			return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
		}
	default:
		return fmt.Errorf("unsupported OS: %s", runtime.GOOS)
	}
	if _, err := exec.LookPath("gst"); err == nil {
		return nil
	}
	return fmt.Errorf("gst not found")
}

func run(cmd *exec.Cmd) error {
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func buildSmalltalkFromSource() error {
	fmt.Println("🔨 Building GNU Smalltalk from source...")
	dir, err := os.MkdirTemp("", "smalltalk-build")
	if err != nil {
		return err
	}
	defer os.RemoveAll(dir)

	tarball := filepath.Join(dir, "smalltalk.tar.gz")
	url := os.Getenv("SMALLTALK_TARBALL")
	if url == "" {
		url = "https://ftpmirror.gnu.org/smalltalk/smalltalk-3.2.5.tar.gz"
	}
	if err := run(exec.Command("curl", "-L", "-o", tarball, url)); err != nil {
		return err
	}
	if err := run(exec.Command("tar", "xf", tarball, "-C", dir, "--strip-components=1")); err != nil {
		return err
	}

	if _, err := exec.LookPath("apt-get"); err == nil {
		_ = run(exec.Command("apt-get", "update"))
		_ = run(exec.Command("apt-get", "install", "-y",
			"autoconf", "automake", "libtool", "bison", "flex",
			"build-essential", "texinfo", "pkg-config"))
	}

	cmd := exec.Command("autoreconf", "-fi")
	cmd.Dir = dir
	if err := run(cmd); err != nil {
		return err
	}
	cmd = exec.Command("./configure", "--prefix=/usr/local")
	cmd.Dir = dir
	if err := run(cmd); err != nil {
		return err
	}
	cmd = exec.Command("make", "-j", "2")
	cmd.Dir = dir
	if err := run(cmd); err != nil {
		return err
	}
	cmd = exec.Command("make", "install")
	cmd.Dir = dir
	if err := run(cmd); err != nil {
		return err
	}
	return nil
}

// EnsureFormatter checks for the gst-format command, attempting to
// install it via EnsureSmalltalk if missing. It returns an error if the
// formatter remains unavailable.
func EnsureFormatter() error {
	if _, err := exec.LookPath("gst-format"); err == nil {
		return nil
	}
	if err := EnsureSmalltalk(); err != nil {
		return err
	}
	if _, err := exec.LookPath("gst-format"); err == nil {
		return nil
	}
	return fmt.Errorf("gst-format not found")
}
