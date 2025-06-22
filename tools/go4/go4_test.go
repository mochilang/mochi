package main

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func buildGo4(t *testing.T, dir string) string {
	t.Helper()
	exe := filepath.Join(dir, "go4")
	cmd := exec.Command("go", "build", "-o", exe, "go4.go")
	cmd.Env = append(os.Environ(), "CGO_ENABLED=0")
	cmd.Dir = filepath.Join("..", "..", "tools", "go4")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}
	return exe
}

func runCmd(t *testing.T, exe string, dir string, args ...string) string {
	t.Helper()
	cmd := exec.Command(exe, args...)
	cmd.Dir = dir
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		t.Fatalf("%v failed: %v\n%s", args, err, buf.String())
	}
	return buf.String()
}

func TestGo4Toolchain(t *testing.T) {
	tmp := t.TempDir()
	exe := buildGo4(t, tmp)

	helloSrc := `package main
import "fmt"
func main() { fmt.Println(3 + 4) }
`
	helloPath := filepath.Join(tmp, "hello.go")
	if err := os.WriteFile(helloPath, []byte(helloSrc), 0644); err != nil {
		t.Fatalf("write hello.go: %v", err)
	}

	dir := filepath.Join("..", "..", "tools", "go4")

	if out := strings.TrimSpace(runCmd(t, exe, dir, helloPath)); out != "7" {
		t.Fatalf("unexpected output: %q", out)
	}

	if out := runCmd(t, exe, dir, "-s", helloPath); !strings.Contains(out, "PUSH") {
		t.Fatalf("unexpected -s output: %q", out)
	}

	if out := strings.TrimSpace(runCmd(t, exe, dir, "go4.go", helloPath)); out != "7" {
		t.Fatalf("self run output: %q", out)
	}

	if out := strings.TrimSpace(runCmd(t, exe, dir, "go4.go", "go4.go", helloPath)); out != "7" {
		t.Fatalf("double self run output: %q", out)
	}
}
