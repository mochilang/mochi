package cljcode

import (
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

// EnsureCljfmt verifies that the cljfmt formatting tool is installed. It simply
// checks that the `cljfmt` binary is present in the PATH.
func EnsureCljfmt() (string, error) {
	if path, err := exec.LookPath("cljfmt"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("cljfmt not installed")
}

// Format runs cljfmt on the provided Clojure source. If cljfmt isn't available
// the input is returned unchanged.
func Format(src []byte) ([]byte, error) {
	cljfmt, err := EnsureCljfmt()
	if err != nil {
		return src, nil
	}
	f, err := os.CreateTemp("", "mochi-*.clj")
	if err != nil {
		return src, err
	}
	name := f.Name()
	if _, err := f.Write(src); err != nil {
		f.Close()
		os.Remove(name)
		return src, err
	}
	f.Close()
	defer os.Remove(name)
	cmd := exec.Command(cljfmt, "fix", name)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return src, err
	}
	formatted, err := os.ReadFile(name)
	if err != nil {
		return src, err
	}
	return formatted, nil
}
