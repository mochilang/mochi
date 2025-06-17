package elixir

import (
	"fmt"
	"os"
	"os/exec"
)

// EnsureElixir verifies that the Elixir binary is installed and attempts to
// install it using common package managers if missing.
func EnsureElixir() error {
	if _, err := exec.LookPath("elixir"); err == nil {
		return nil
	}
	fmt.Println("🔧 Installing Elixir...")
	if _, err := exec.LookPath("apt-get"); err == nil {
		cmd := exec.Command("apt-get", "update")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		cmd = exec.Command("apt-get", "install", "-y", "elixir")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		return nil
	}
	if _, err := exec.LookPath("brew"); err == nil {
		cmd := exec.Command("brew", "install", "elixir")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		return nil
	}
	if _, err := exec.LookPath("asdf"); err == nil {
		exec.Command("asdf", "plugin-add", "--skip-existing", "erlang").Run()
		exec.Command("asdf", "plugin-add", "--skip-existing", "elixir").Run()
		cmd := exec.Command("asdf", "install", "elixir", "latest")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		cmd = exec.Command("asdf", "global", "elixir", "latest")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return err
		}
		return nil
	}
	return fmt.Errorf("elixir not installed and no supported installer found")
}
