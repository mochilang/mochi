package python

import (
	"os"
	"os/exec"
	"sync"
)

var (
	bin  string
	once sync.Once
)

// Binary returns the path to the Python interpreter used by Mochi.
// It checks the MOCHI_PYTHON environment variable, then searches for
// "python3" and "python" in PATH. The result is cached for future calls.
func Binary() string {
	once.Do(func() {
		if env := os.Getenv("MOCHI_PYTHON"); env != "" {
			bin = env
			return
		}
		if path, err := exec.LookPath("python3"); err == nil {
			bin = path
			return
		}
		if path, err := exec.LookPath("python"); err == nil {
			bin = path
			return
		}
		bin = "python3"
	})
	return bin
}

// Cmd returns an exec.Cmd for the configured Python interpreter.
func Cmd(args ...string) *exec.Cmd {
	return exec.Command(Binary(), args...)
}
