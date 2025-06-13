package python_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/runtime/ffi/python"
)

func TestExport(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
	}
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	os.Setenv("PYTHONPATH", wd)
	defer os.Unsetenv("PYTHONPATH")

	dir, err := os.MkdirTemp("", "py_export_test")
	if err != nil {
		t.Fatalf("temp dir: %v", err)
	}
	defer os.RemoveAll(dir)

	if err := python.Export("testmod", dir); err != nil {
		t.Fatalf("export failed: %v", err)
	}

	outPath := filepath.Join(dir, "testmod.mochi")
	data, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read file: %v", err)
	}
	text := string(data)
	if !strings.HasPrefix(text, "import python \"testmod\" as testmod") {
		t.Fatalf("unexpected header: %s", text)
	}
	if !strings.Contains(text, "extern fun testmod.add") {
		t.Fatalf("missing function declaration")
	}
}
