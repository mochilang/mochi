package main

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
)

// TestGoIngestCLI builds the binary and runs it against a tiny
// in-memory fixture, asserting the output parses as ApiSurface JSON.
func TestGoIngestCLI(t *testing.T) {
	if testing.Short() {
		t.Skip("short mode")
	}

	dir := t.TempDir()
	// Build the binary.
	bin := filepath.Join(dir, "go-ingest")
	if out, err := exec.Command("go", "build", "-o", bin, ".").CombinedOutput(); err != nil {
		t.Fatalf("go build: %v\n%s", err, out)
	}

	// Write a fixture module.
	mod := filepath.Join(dir, "fixture")
	if err := os.MkdirAll(mod, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(mod, "go.mod"), []byte("module fixture.test/m\n\ngo 1.21\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	body := `package m

// Greeting returns hi.
func Greeting() string { return "hi" }
`
	if err := os.WriteFile(filepath.Join(mod, "lib.go"), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}

	// Run the binary.
	out := filepath.Join(dir, "out.json")
	cmd := exec.Command(bin, "-module", "fixture.test/m", "-version", "v0.1.0", "-dir", mod, "-output", out)
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("go-ingest: %v", err)
	}

	data, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.HasSuffix(data, []byte("\n")) {
		t.Errorf("output missing trailing newline")
	}
	var raw map[string]any
	if err := json.Unmarshal(data, &raw); err != nil {
		t.Fatalf("invalid JSON: %v", err)
	}
	if raw["module"] != "fixture.test/m" {
		t.Errorf("module = %v", raw["module"])
	}
	if raw["version"] != "v0.1.0" {
		t.Errorf("version = %v", raw["version"])
	}
	if v, ok := raw["schema_version"].(float64); !ok || int(v) != 1 {
		t.Errorf("schema_version = %v", raw["schema_version"])
	}
}

func TestGoIngestRequiresModule(t *testing.T) {
	err := run([]string{}, os.Stdout, os.Stderr)
	if err == nil {
		t.Errorf("run with no args: want error")
	}
}
