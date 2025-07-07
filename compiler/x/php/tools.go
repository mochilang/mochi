package phpcode

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"runtime"
)

// EnsurePHP ensures the php command is available, attempting installation if missing.
func EnsurePHP() error {
	if _, err := exec.LookPath("php"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "php-cli")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
		if _, err := exec.LookPath("apk"); err == nil {
			cmd := exec.Command("apk", "add", "--no-cache", "php-cli")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "php")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "php")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			break
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "php")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			break
		}
	}
	if _, err := exec.LookPath("php"); err == nil {
		return nil
	}
	return fmt.Errorf("php not installed")
}

// EnsurePHPCBF ensures the phpcbf command is available, attempting installation if missing.
func EnsurePHPCBF() error {
	if _, err := exec.LookPath("phpcbf"); err == nil {
		return nil
	}
	if err := EnsurePHP(); err != nil {
		return err
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "php-codesniffer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
		if _, err := exec.LookPath("apk"); err == nil {
			cmd := exec.Command("apk", "add", "--no-cache", "php-codesniffer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "php-code-sniffer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "phpcodesniffer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "phpcodesniffer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("phpcbf"); err == nil {
		return nil
	}
	return fmt.Errorf("phpcbf not installed")
}

// FormatPHP runs phpcbf with PSR12 on the provided PHP source code. If phpcbf
// is unavailable or fails, the input is returned unchanged with a trailing
// newline ensured.
func FormatPHP(src []byte) []byte {
	if err := EnsurePHPCBF(); err != nil {
		if len(src) > 0 && src[len(src)-1] != '\n' {
			src = append(src, '\n')
		}
		return src
	}
	cmd := exec.Command("phpcbf", "-q", "--standard=PSR12", "-")
	cmd.Stdin = bytes.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = io.Discard
	if err := cmd.Run(); err == nil {
		res := out.Bytes()
		if len(res) == 0 || res[len(res)-1] != '\n' {
			res = append(res, '\n')
		}
		return res
	}
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
