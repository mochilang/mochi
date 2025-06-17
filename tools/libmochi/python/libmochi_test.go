package libmochi_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func TestLibMochi(t *testing.T) {
	t.Skip("LibMochi integration test disabled in CI")
	tmpDir := t.TempDir()
	mochiPath := filepath.Join(tmpDir, "mochi")

	buildCmd := exec.Command("go", "build", "-o", mochiPath, "./cmd/mochi")
	buildCmd.Env = append(os.Environ(), "CGO_ENABLED=0")
	buildCmd.Dir = filepath.Join("..", "..", "..")
	if out, err := buildCmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}

	pipCmd := exec.Command("python3", "-m", "pip", "install", "-e", "tools/libmochi/python")
	pipCmd.Dir = filepath.Join("..", "..", "..")
	if out, err := pipCmd.CombinedOutput(); err != nil {
		t.Fatalf("pip install failed: %v\n%s", err, out)
	} else {
		t.Log(string(out))
	}

	script := `from libmochi import run, call, eval
print(run('print("hi")').strip())
print(call('fun add(a:int, b:int): int { return a + b }', 'add', 2, 3))
print(eval('print("ffi")').strip())`
	runCmd := exec.Command("python3", "-")
	runCmd.Dir = filepath.Join("..", "..", "..")
	runCmd.Env = append(os.Environ(), "PATH="+tmpDir+string(os.PathListSeparator)+os.Getenv("PATH"))
	runCmd.Stdin = strings.NewReader(script)
	out, err := runCmd.CombinedOutput()
	if err != nil {
		t.Fatalf("python run failed: %v\n%s", err, out)
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	if len(lines) != 3 {
		t.Fatalf("unexpected output: %q", string(out))
	}
	if lines[0] != "hi" {
		t.Fatalf("run() output: %q", lines[0])
	}
	if lines[1] != "5" {
		t.Fatalf("call() output: %q", lines[1])
	}
	if lines[2] != "ffi" {
		t.Fatalf("eval() output: %q", lines[2])
	}
}
