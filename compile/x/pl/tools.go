package plcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureSWIPL verifies that the SWI-Prolog interpreter is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS.
func EnsureSWIPL() error {
	if bin := os.Getenv("SWIPL_BIN"); bin != "" {
		if _, err := os.Stat(bin); err == nil {
			return nil
		}
	}
	if _, err := exec.LookPath("swipl"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "swi-prolog")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("swipl"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "swipl")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("swipl"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("swipl"); err == nil {
		return nil
	}
	return fmt.Errorf("swipl not found")
}

// FormatPL attempts to tidy Prolog source code using SWI-Prolog's
// `prolog_tidy` library. If SWI-Prolog is unavailable or formatting
// fails, the input is returned unchanged with a trailing newline.
func FormatPL(src []byte) []byte {
	if err := EnsureSWIPL(); err == nil {
		if path, err := exec.LookPath("swipl"); err == nil {
			cmd := exec.Command(path, "-q", "-t", "halt", "-g", "use_module(library(prolog_tidy)),prolog_tidy:tidy_stream(user_input,user_output)")
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
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
