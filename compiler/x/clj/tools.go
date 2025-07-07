package cljcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureClojure verifies that the clojure command line tool is installed.
// It attempts a best-effort installation using apt-get on Linux or Homebrew on
// macOS. Tests can call this to skip if installation fails.
func EnsureClojure() error {
	if _, err := exec.LookPath("clojure"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clj"); err == nil {
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
			cmd = exec.Command("apt-get", "install", "-y", "clojure", "libdata-json-clojure", "libsnakeyaml-engine-java")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "clojure/tools/clojure")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "clojure")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("clojure"); err == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "clojure")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath("clojure"); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath("clojure"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("clj"); err == nil {
		return nil
	}
	return fmt.Errorf("clojure not installed")
}

// EnsureCljfmt verifies that the cljfmt formatting tool is installed. It tries
// to install the latest release if the binary is missing and returns the path to
// the executable.
func EnsureCljfmt() (string, error) {
	if path, err := exec.LookPath("cljfmt"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "linux", "darwin":
		cmd := exec.Command("bash", "-c", "curl -fsSL https://raw.githubusercontent.com/weavejester/cljfmt/HEAD/install.sh | bash")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		_ = cmd.Run()
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "cljfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "cljfmt")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("cljfmt"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("cljfmt not installed")
}

// Format runs cljfmt on the provided Clojure source. If cljfmt isn't available
// the input is returned unchanged.
func Format(src []byte) ([]byte, error) {
	cljfmt, err := exec.LookPath("cljfmt")
	if err == nil {
		f, ferr := os.CreateTemp("", "mochi-*.clj")
		if ferr == nil {
			name := f.Name()
			if _, ferr = f.Write(src); ferr == nil {
				f.Close()
				cmd := exec.Command(cljfmt, "fix", name)
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				if cmd.Run() == nil {
					if formatted, rerr := os.ReadFile(name); rerr == nil {
						src = formatted
					}
				}
			}
			os.Remove(name)
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src, nil
}
