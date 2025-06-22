package main

import (
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func buildGo4(t *testing.T) string {
	t.Helper()
	tmp := t.TempDir()
	bin := filepath.Join(tmp, "go4")
	cmd := exec.Command("go", "build", "-o", bin, "go4.go")
	cmd.Dir = "."
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s", err, out)
	}
	return bin
}

func runGo4(t *testing.T, bin string, args ...string) string {
	t.Helper()
	cmd := exec.Command(bin, args...)
	cmd.Dir = "."
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go4 run failed: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

func TestGo4SelfRuns(t *testing.T) {
	bin := buildGo4(t)
	want := "hello from go4"

	if got := runGo4(t, bin, "hello.go"); got != want {
		t.Fatalf("unexpected output: %q", got)
	}
	if got := runGo4(t, bin, "go4.go", "hello.go"); got != want {
		t.Fatalf("unexpected output with go4.go: %q", got)
	}
	if got := runGo4(t, bin, "go4.go", "go4.go", "hello.go"); got != want {
		t.Fatalf("unexpected output with two go4.go: %q", got)
	}

	out := runGo4(t, bin, "-s", "hello.go")
	if !strings.Contains(out, "PRINT") {
		t.Fatalf("-s output missing PRINT instruction: %q", out)
	}
}
