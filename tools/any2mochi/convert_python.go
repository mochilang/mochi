package any2mochi

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// ConvertPython converts Python source code to Mochi using a lightweight
// translator implemented in tools/any2mochi/py_simple.py.
func ConvertPython(src string) ([]byte, error) {
	tmp, err := os.CreateTemp("", "py-src-*.py")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	return ConvertPythonFile(tmp.Name())
}

// ConvertPythonFile reads the Python file at path and converts it to Mochi using
// the helper Python script. Any errors from the script are returned verbatim.
func ConvertPythonFile(path string) ([]byte, error) {
	root, err := repoRoot()
	if err != nil {
		root = "."
	}
	script := filepath.Join(root, "tools", "any2mochi", "py_simple.py")
	cmd := exec.Command("python3", script, path)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("%s", out)
	}
	return out, nil
}
