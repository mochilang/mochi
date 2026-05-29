package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase28DriverErrorPaths exercises Driver.Build's failure branches:
//
//   - parse error: malformed Mochi source
//   - typecheck error: type mismatch
//   - missing file: source path does not exist
//   - unknown target: out-of-range Target value
//
// Each subtest asserts the error is non-nil and the message mentions the
// failing stage so an end user can diagnose without a stack trace.
func TestPhase28DriverErrorPaths(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	t.Run("parse_error", func(t *testing.T) {
		srcDir := t.TempDir()
		src := filepath.Join(srcDir, "bad.mochi")
		if err := os.WriteFile(src, []byte("let x ="), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		d := &Driver{}
		err := d.Build(src, t.TempDir(), TargetRubySource)
		if err == nil {
			t.Fatalf("expected parse error, got nil")
		}
		if !strings.Contains(err.Error(), "parse") {
			t.Fatalf("error message should mention parse stage: %v", err)
		}
	})

	t.Run("typecheck_error", func(t *testing.T) {
		srcDir := t.TempDir()
		src := filepath.Join(srcDir, "typebad.mochi")
		if err := os.WriteFile(src, []byte("let x: int = \"not an int\"\n"), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		d := &Driver{}
		err := d.Build(src, t.TempDir(), TargetRubySource)
		if err == nil {
			t.Fatalf("expected typecheck error, got nil")
		}
		if !strings.Contains(err.Error(), "typecheck") {
			t.Fatalf("error message should mention typecheck stage: %v", err)
		}
	})

	t.Run("missing_file", func(t *testing.T) {
		d := &Driver{}
		err := d.Build("/nonexistent/path/does_not_exist.mochi", t.TempDir(), TargetRubySource)
		if err == nil {
			t.Fatalf("expected file error, got nil")
		}
		if !strings.Contains(err.Error(), "read") && !strings.Contains(err.Error(), "no such file") && !strings.Contains(err.Error(), "parse") {
			t.Fatalf("error message should mention read/no such file: %v", err)
		}
	})

	t.Run("unknown_target", func(t *testing.T) {
		srcDir := t.TempDir()
		src := filepath.Join(srcDir, "ok.mochi")
		if err := os.WriteFile(src, []byte("print(\"hi\")\n"), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		d := &Driver{}
		err := d.Build(src, t.TempDir(), Target(999))
		if err == nil {
			t.Fatalf("expected unknown-target error, got nil")
		}
		if !strings.Contains(err.Error(), "not implemented") {
			t.Fatalf("error message should mention not implemented: %v", err)
		}
	})

	t.Run("out_dir_is_file", func(t *testing.T) {
		// Coverage for emit failure: pass a file path as outDir; the
		// emit pass calls MkdirAll/WriteFile and at least one of them
		// fails. Confirms the Driver propagates I/O errors verbatim,
		// rather than swallowing them as a successful build.
		srcDir := t.TempDir()
		src := filepath.Join(srcDir, "ok.mochi")
		if err := os.WriteFile(src, []byte("print(\"hi\")\n"), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		outFile := filepath.Join(t.TempDir(), "not_a_dir")
		if err := os.WriteFile(outFile, []byte("blocker"), 0o644); err != nil {
			t.Fatalf("seed blocker file: %v", err)
		}
		d := &Driver{}
		err := d.Build(src, outFile, TargetRubySource)
		if err == nil {
			t.Fatalf("expected emit/mkdir error, got nil")
		}
	})
}
