package hscode

import (
        "bytes"
        "fmt"
        "os"
        "os/exec"
        rruntime "runtime"
)

// EnsureHaskell verifies that ghc/runhaskell is installed. It attempts a
// best-effort installation using apt-get on Linux or Homebrew on macOS.
func EnsureHaskell() error {
	if _, err := exec.LookPath("runhaskell"); err == nil {
		return nil
	}
	switch rruntime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				break
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "ghc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
        if _, err := exec.LookPath("runhaskell"); err == nil {
                return nil
        }
        return fmt.Errorf("ghc not installed")
}

// FormatHS runs `ormolu` or `fourmolu` on the given source code if available.
// If formatting fails or neither formatter is installed, the input is returned
// unchanged. A trailing newline is ensured in the returned slice.
func FormatHS(src []byte) []byte {
        path, err := exec.LookPath("ormolu")
        if err != nil {
                if p, err := exec.LookPath("fourmolu"); err == nil {
                        path = p
                } else {
                        if len(src) > 0 && src[len(src)-1] != '\n' {
                                src = append(src, '\n')
                        }
                        return src
                }
        }
        cmd := exec.Command(path, "--stdin-input-file", "Main.hs")
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
        if len(src) > 0 && src[len(src)-1] != '\n' {
                src = append(src, '\n')
        }
        return src
}
