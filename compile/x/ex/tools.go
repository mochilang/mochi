package excode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
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
	if runtime.GOOS == "windows" {
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "elixir")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "elixir")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
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

// Format runs Elixir's Code formatter on the provided source code. If the
// `elixir` binary is not available, the code is returned unchanged. Formatting
// errors are returned.
func Format(code []byte) ([]byte, error) {
	if _, err := exec.LookPath("elixir"); err != nil {
		return code, nil
	}
	cmd := exec.Command("elixir", "-e", "IO.read(:stdio, :eof) |> Code.format_string!() |> Enum.join(\"\") |> IO.write()")
	cmd.Stdin = bytes.NewReader(code)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &out
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("format error: %w\n%s", err, out.Bytes())
	}
	return out.Bytes(), nil
}
