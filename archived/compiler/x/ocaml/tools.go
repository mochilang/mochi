//go:build archived

package mlcode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureOCaml verifies that the OCaml compiler is installed. If missing,
// it attempts a best-effort installation using apt-get on Linux or Homebrew
// on macOS.
func EnsureOCaml() error {
	if _, err := exec.LookPath("ocamlc"); err == nil {
		if checkYojson() == nil {
			_ = ensureOCamlFormat()
			return nil
		}
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
			cmd = exec.Command("apt-get", "install", "-y", "ocaml", "ocaml-findlib", "libyojson-ocaml-dev")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			_ = ensureOCamlFormat()
			if err := checkYojson(); err == nil {
				return nil
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ocaml", "yojson", "ocaml-findlib", "ocamlformat")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if err := checkYojson(); err == nil {
				return nil
			}
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "ocaml", "opam")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			_ = exec.Command("opam", "install", "-y", "yojson", "ocamlformat").Run()
			if checkYojson() == nil {
				return nil
			}
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "ocaml", "opam")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			_ = exec.Command("opam", "install", "-y", "yojson", "ocamlformat").Run()
			if checkYojson() == nil {
				return nil
			}
		}
	}
	if err := checkYojson(); err == nil {
		_ = ensureOCamlFormat()
		return nil
	}
	return fmt.Errorf("yojson not found")
}

func checkYojson() error {
	if _, err := exec.LookPath("ocamlfind"); err == nil {
		if err := exec.Command("ocamlfind", "query", "yojson").Run(); err == nil {
			return nil
		}
	}
	return fmt.Errorf("yojson missing")
}

func ensureOCamlFormat() error {
	if checkOCamlFormat() == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "install", "-y", "ocamlformat")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			cmd = exec.Command("apt-get", "install", "-y", "ocp-indent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "ocamlformat", "ocp-indent")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("opam"); err == nil {
			_ = exec.Command("opam", "install", "-y", "ocamlformat", "ocp-indent").Run()
		}
	}
	return checkOCamlFormat()
}

func checkOCamlFormat() error {
	if _, err := exec.LookPath("ocamlformat"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("ocp-indent"); err == nil {
		return nil
	}
	return fmt.Errorf("ocamlformat missing")
}
