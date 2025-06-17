package notebook_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestNotebookMagic(t *testing.T) {
	t.Skip("Notebook integration test disabled in CI")
	tmpDir := t.TempDir()
	mochiPath := filepath.Join(tmpDir, "mochi")

	// build the mochi CLI
	buildCmd := exec.Command("go", "build", "-o", mochiPath, "./cmd/mochi")
	buildCmd.Env = append(os.Environ(), "CGO_ENABLED=0")
	buildCmd.Dir = filepath.Join("..", "..")
	if out, err := buildCmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}

	// ensure ipython is installed
	pipCmd := exec.Command("python3", "-m", "pip", "install", "ipython")
	if out, err := pipCmd.CombinedOutput(); err != nil {
		t.Fatalf("pip install failed: %v\n%s", err, out)
	} else {
		t.Log(string(out))
	}

	// run a small ipython script using the mochi magic
	script := `import sys
from IPython.terminal.interactiveshell import TerminalInteractiveShell
from IPython.utils.capture import capture_output
sys.path.insert(0, 'tools/notebook')
import mochi_magic
sh = TerminalInteractiveShell.instance()
mochi_magic.load_ipython_extension(sh)
with capture_output() as cap:
    sh.run_cell("%%mochi\nprint(\"hello\")")
print(cap.stdout.strip())`
	runCmd := exec.Command("python3", "-")
	runCmd.Dir = filepath.Join("..", "..")
	runCmd.Env = append(os.Environ(), "PATH="+tmpDir+string(os.PathListSeparator)+os.Getenv("PATH"))
	runCmd.Stdin = strings.NewReader(script)
	out, err := runCmd.CombinedOutput()
	if err != nil {
		t.Fatalf("python run failed: %v\n%s", err, out)
	}
	got := strings.TrimSpace(string(out))
	if got != "hello" {
		t.Fatalf("unexpected output: %q", got)
	}
}
