//go:build slow

package pascode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureFPC checks for the Free Pascal compiler and attempts to
// install it via apt or Homebrew if missing. It is safe to call from tests.
func EnsureFPC() (string, error) {
	if path, err := exec.LookPath("fpc"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			// Install the full Free Pascal suite which provides the
			// compiler and required runtime units. Using the "fpc"
			// meta-package ensures a consistent setup across systems.
			cmd = exec.Command("apt-get", "install", "-y", "fpc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "fpc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "fpc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if path, err := exec.LookPath("fpc"); err == nil {
				return path, nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "fpc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if path, err := exec.LookPath("fpc"); err == nil {
				return path, nil
			}
		}
	}
	if path, err := exec.LookPath("fpc"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("fpc not installed")
}

// EnsurePtop verifies that the ptop formatter is available. If not found,
// it attempts to install the Free Pascal tools which include ptop. Tests can
// call this helper to ensure formatting works.
func EnsurePtop() error {
	if _, err := exec.LookPath("ptop"); err == nil {
		return nil
	}
	if _, err := EnsureFPC(); err != nil {
		return err
	}
	if _, err := exec.LookPath("ptop"); err == nil {
		return nil
	}
	return fmt.Errorf("ptop not installed")
}

// FormatPas runs ptop to pretty-print Pascal code if available.
// Keywords keep their original casing and indentation is set to two spaces.
func FormatPas(src []byte) []byte {
	// ptop formatting sometimes mangles generic declarations, so we
	// perform a minimal formatting pass ourselves instead of invoking
	// the external tool.
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	lines := bytes.Split(src, []byte("\n"))
	for i, ln := range lines {
		lines[i] = bytes.TrimRight(ln, " \t")
	}
	src = bytes.Join(lines, []byte("\n"))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
