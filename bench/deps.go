package bench

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	cscode "mochi/compile/cs"
	dartcode "mochi/compile/dart"
	pycode "mochi/compile/py"
	rscode "mochi/compile/rust"
	swiftcode "mochi/compile/swift"
	tscode "mochi/compile/ts"
)

// EnsureDeps verifies that Mochi, Deno and Python3 are installed.
// If Mochi or Deno are missing, it attempts to install them.
// Python3 will be installed via apt-get if missing.
func EnsureDeps() (string, error) {
	mochiBin, err := ensureMochi()
	if err != nil {
		return "", err
	}
	if err := pycode.EnsurePython(); err != nil {
		return "", err
	}
	if err := rscode.EnsureRust(); err != nil {
		return "", err
	}
	if err := tscode.EnsureDeno(); err != nil {
		return "", err
	}
	if err := cscode.EnsureDotnet(); err != nil {
		return "", err
	}
	if err := dartcode.EnsureDart(); err != nil {
		return "", err
	}
	if err := swiftcode.EnsureSwift(); err != nil {
		return "", err
	}
	return mochiBin, nil
}

func ensureMochi() (string, error) {
	if path, err := exec.LookPath("mochi"); err == nil {
		return path, nil
	}
	fmt.Println("🔧 Mochi not found, building...")
	home := os.Getenv("HOME")
	if home == "" {
		return "", fmt.Errorf("HOME not set")
	}
	out := filepath.Join(home, "bin", "mochi")
	if err := os.MkdirAll(filepath.Dir(out), 0755); err != nil {
		return "", err
	}
	cmd := exec.Command("go", "build", "-o", out, "./cmd/mochi")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return "", err
	}
	return out, nil
}
