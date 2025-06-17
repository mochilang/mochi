package ktcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"

	javacode "mochi/compile/java"
)

// EnsureKotlin verifies that the Kotlin compiler is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureKotlin() error {
	if _, err := exec.LookPath("java"); err != nil {
		switch runtime.GOOS {
		case "darwin":
			if _, err := exec.LookPath("brew"); err == nil {
				cmd := exec.Command("brew", "install", "openjdk")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			}
		case "linux":
			if _, err := exec.LookPath("apt-get"); err == nil {
				cmd := exec.Command("apt-get", "update")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				if err := cmd.Run(); err == nil {
					cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jdk")
					cmd.Stdout = os.Stdout
					cmd.Stderr = os.Stderr
					_ = cmd.Run()
				}
			}
		}
		if err := javacode.EnsureJavac(); err != nil {
			return err
		}
	}
	if _, err := exec.LookPath("kotlinc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		return fmt.Errorf("kotlinc missing; install via Homebrew or Android Studio")
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
		cmd := exec.Command("bash", "-c", "curl -sS https://get.sdkman.io | bash && source $HOME/.sdkman/bin/sdkman-init.sh && sdk install kotlin")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	}
	return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}
