//go:build archived

package cobolcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
	"strings"
)

// EnsureCOBOL ensures the cobc compiler is installed.
func EnsureCOBOL() error {
	if _, err := exec.LookPath("cobc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "gnu-cobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	default:
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "gnucobol")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			if _, err := exec.LookPath("cobc"); err == nil {
				return nil
			}
		}
	}
	return fmt.Errorf("cobc not installed")
}

// EnsureCobfmt ensures the cobfmt formatter is installed. It attempts a
// best-effort installation using common package managers.
func EnsureCobfmt() error {
	if _, err := exec.LookPath("cobfmt"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F527 Installing cobfmt...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "cobfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F37A Installing cobfmt via Homebrew...")
			cmd := exec.Command("brew", "install", "cobfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("\U0001F527 Installing cobfmt via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "cobfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("\U0001F527 Installing cobfmt via Scoop...")
			cmd := exec.Command("scoop", "install", "cobfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("cobfmt"); err == nil {
		return nil
	}
	return fmt.Errorf("cobfmt not found")
}

// FormatCOBOL runs a formatter on the given source if available. The
// `cobfmt` tool is tried first. If no formatter is found or formatting
// fails, the input is returned unchanged with a trailing newline ensured.
func FormatCOBOL(src []byte) []byte {
	if os.Getenv("MOCHI_SKIP_COBFMT") != "1" {
		if err := EnsureCobfmt(); err == nil {
			path, _ := exec.LookPath("cobfmt")
			cmd := exec.Command(path)
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
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	// Trim trailing spaces on each line for readability
	lines := strings.Split(string(src), "\n")
	for i, ln := range lines {
		lines[i] = strings.TrimRight(ln, " \t")
	}
	return []byte(strings.Join(lines, "\n"))
}
