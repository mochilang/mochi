package bench

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureSwift verifies that the Swift toolchain is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on
// Linux.
func EnsureSwift() error {
	if _, err := exec.LookPath("swiftc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swift")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		return fmt.Errorf("swift toolchain missing; install Xcode command line tools")
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "swiftlang")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
		// fallback: try official installer script
		cmd := exec.Command("bash", "-c", "curl -sSL https://swift.org/install.sh | bash")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	}
	return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}
