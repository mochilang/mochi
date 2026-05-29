package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRunRebar3Compile_NotFound(t *testing.T) {
	d := NewDriver(Options{})
	err := d.RunRebar3Compile(t.TempDir(), Rebar3Options{Rebar3Bin: "/nonexistent/rebar3"})
	if err == nil {
		t.Fatal("expected error when rebar3 binary not found")
	}
	var notFound *ErrRebar3NotFound
	if !isErrRebar3NotFound(err, &notFound) {
		t.Errorf("want ErrRebar3NotFound, got %T: %v", err, err)
	}
	if notFound.Bin != "/nonexistent/rebar3" {
		t.Errorf("Bin = %q, want /nonexistent/rebar3", notFound.Bin)
	}
}

func isErrRebar3NotFound(err error, out **ErrRebar3NotFound) bool {
	if e, ok := err.(*ErrRebar3NotFound); ok {
		if out != nil {
			*out = e
		}
		return true
	}
	return false
}

func TestRunRebar3Compile_PathLookupFailure(t *testing.T) {
	// Use a name that definitely won't be on PATH.
	d := NewDriver(Options{})
	err := d.RunRebar3Compile(t.TempDir(), Rebar3Options{Rebar3Bin: "mochi_no_such_rebar3_xyzzy"})
	if err == nil {
		t.Fatal("expected error when rebar3 not on PATH")
	}
	if !isErrRebar3NotFound(err, nil) {
		t.Errorf("want ErrRebar3NotFound, got %T: %v", err, err)
	}
}

func TestErrRebar3NotFound_ErrorMessage(t *testing.T) {
	e := &ErrRebar3NotFound{Bin: "rebar3"}
	msg := e.Error()
	if !strings.Contains(msg, "rebar3") {
		t.Errorf("error message should mention binary name, got: %q", msg)
	}
}

func TestMaterializeShimErl_WritesFile(t *testing.T) {
	d := NewDriver(Options{})
	shimsDir := t.TempDir()
	content := "-module(cowboy_mochi_shim).\n-export([start/0]).\n"

	path, err := d.MaterializeShimErl(shimsDir, "cowboy", content)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	want := filepath.Join(shimsDir, "cowboy", "src", "cowboy_mochi_shim.erl")
	if path != want {
		t.Errorf("path = %q, want %q", path, want)
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read back: %v", err)
	}
	if string(got) != content {
		t.Errorf("content mismatch\ngot:  %q\nwant: %q", got, content)
	}
}

func TestMaterializeShimErl_CreatesDirectories(t *testing.T) {
	d := NewDriver(Options{})
	shimsDir := filepath.Join(t.TempDir(), "nested", "shims")
	_, err := d.MaterializeShimErl(shimsDir, "ranch", "content")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	expected := filepath.Join(shimsDir, "ranch", "src", "ranch_mochi_shim.erl")
	if _, err := os.Stat(expected); os.IsNotExist(err) {
		t.Errorf("expected file %s to exist", expected)
	}
}

func TestMaterializeShimErl_Overwrite(t *testing.T) {
	d := NewDriver(Options{})
	shimsDir := t.TempDir()
	first := "version 1\n"
	second := "version 2\n"

	if _, err := d.MaterializeShimErl(shimsDir, "jsx", first); err != nil {
		t.Fatal(err)
	}
	path, err := d.MaterializeShimErl(shimsDir, "jsx", second)
	if err != nil {
		t.Fatal(err)
	}
	got, _ := os.ReadFile(path)
	if string(got) != second {
		t.Errorf("overwrite failed: got %q, want %q", got, second)
	}
}

func TestMaterializeRunnerErl_ContainsExpectedSections(t *testing.T) {
	d := NewDriver(Options{})
	shimsDir := t.TempDir()

	path, err := d.MaterializeRunnerErl(shimsDir)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	expected := filepath.Join(shimsDir, "mochi_port_runner", "src", "mochi_port_runner.erl")
	if path != expected {
		t.Errorf("path = %q, want %q", path, expected)
	}

	data, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read: %v", err)
	}
	src := string(data)

	// Must declare the module and export main/0.
	if !strings.Contains(src, "-module(mochi_port_runner)") {
		t.Error("missing -module declaration")
	}
	if !strings.Contains(src, "-export([main/0])") {
		t.Error("missing -export([main/0])")
	}
	// Must handle the {call, SeqID, Fun, Args} pattern.
	if !strings.Contains(src, "{call, SeqID, Fun, Args}") {
		t.Error("missing call dispatch clause")
	}
	// Must send {reply, SeqID, Result}.
	if !strings.Contains(src, "{reply, SeqID, Result}") {
		t.Error("missing reply tuple emit")
	}
	// Must handle errors.
	if !strings.Contains(src, "{error, SeqID") {
		t.Error("missing error tuple emit")
	}
	// Must handle port close (exit on {Port, closed} or EXIT signal).
	if !strings.Contains(src, "closed") {
		t.Error("missing {Port, closed} handling")
	}
	// Must use {packet, 4} framing.
	if !strings.Contains(src, "{packet, 4}") {
		t.Error("missing {packet, 4} in fd port open")
	}
}

func TestMaterializeRunnerErl_Idempotent(t *testing.T) {
	d := NewDriver(Options{})
	shimsDir := t.TempDir()

	path1, err := d.MaterializeRunnerErl(shimsDir)
	if err != nil {
		t.Fatal(err)
	}
	path2, err := d.MaterializeRunnerErl(shimsDir)
	if err != nil {
		t.Fatal(err)
	}
	if path1 != path2 {
		t.Errorf("idempotent paths differ: %q vs %q", path1, path2)
	}
}

func TestRunRebar3Compile_WithFakeRebar3(t *testing.T) {
	// Build a fake rebar3 script that exits 0, so we can test the happy path
	// without needing a real rebar3 installation.
	fakeDir := t.TempDir()
	fakeRebar3 := filepath.Join(fakeDir, "rebar3")

	script := "#!/bin/sh\nexit 0\n"
	if err := os.WriteFile(fakeRebar3, []byte(script), 0o755); err != nil {
		t.Fatalf("write fake rebar3: %v", err)
	}

	workspaceDir := t.TempDir()
	d := NewDriver(Options{})
	err := d.RunRebar3Compile(workspaceDir, Rebar3Options{Rebar3Bin: fakeRebar3})
	if err != nil {
		t.Errorf("unexpected error with fake rebar3: %v", err)
	}
}

func TestRunRebar3Compile_WithFakeRebar3_Failure(t *testing.T) {
	fakeDir := t.TempDir()
	fakeRebar3 := filepath.Join(fakeDir, "rebar3")
	script := "#!/bin/sh\nexit 1\n"
	if err := os.WriteFile(fakeRebar3, []byte(script), 0o755); err != nil {
		t.Fatalf("write fake rebar3: %v", err)
	}

	workspaceDir := t.TempDir()
	d := NewDriver(Options{})
	err := d.RunRebar3Compile(workspaceDir, Rebar3Options{Rebar3Bin: fakeRebar3})
	if err == nil {
		t.Error("expected error when rebar3 exits non-zero")
	}
}

func TestRunRebar3Compile_Deterministic_SetsEnv(t *testing.T) {
	// Verify that Deterministic mode injects SOURCE_DATE_EPOCH=0.
	fakeDir := t.TempDir()
	fakeRebar3 := filepath.Join(fakeDir, "rebar3")
	// Script: exits 0 if SOURCE_DATE_EPOCH is set; exits 2 otherwise.
	script := `#!/bin/sh
if [ -z "$SOURCE_DATE_EPOCH" ]; then
  exit 2
fi
exit 0
`
	if err := os.WriteFile(fakeRebar3, []byte(script), 0o755); err != nil {
		t.Fatalf("write fake rebar3: %v", err)
	}

	workspaceDir := t.TempDir()
	d := NewDriver(Options{Deterministic: true})
	err := d.RunRebar3Compile(workspaceDir, Rebar3Options{Rebar3Bin: fakeRebar3})
	if err != nil {
		t.Errorf("expected SOURCE_DATE_EPOCH to be set in deterministic mode: %v", err)
	}
}
