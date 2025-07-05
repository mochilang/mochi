//go:build slow

package libmochi_test

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/tools/libmochi/go"
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

	opts := &libmochi.RunOptions{Binary: mochiPath}

	out, err := libmochi.Run("print(\"hi\")", opts)
	if err != nil {
		t.Fatalf("Run failed: %v", err)
	}
	if strings.TrimSpace(out) != "hi" {
		t.Fatalf("Run output: %q", out)
	}

	res, err := libmochi.Call("fun add(a:int, b:int): int { return a + b }", "add", []any{2, 3}, opts)
	if err != nil {
		t.Fatalf("Call failed: %v", err)
	}
	if num, ok := res.(float64); !ok || num != 5 {
		t.Fatalf("Call result: %#v", res)
	}
}
