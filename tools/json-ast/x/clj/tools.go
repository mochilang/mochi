package clj

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureBabashka verifies that the babashka command is available. It attempts a
// best-effort installation using common package managers when missing.
func EnsureBabashka() error {
	if _, err := exec.LookPath(babashkaCmd); err == nil {
		return nil
	}
	fmt.Println("🔧 Installing Babashka...")
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "babashka")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("apk"); err == nil {
			cmd := exec.Command("apk", "add", "--no-cache", "babashka")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "babashka")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "babashka")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "babashka")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath(babashkaCmd); err == nil {
		return nil
	}
	return fmt.Errorf("babashka not installed")
}
