package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

func checkBashSyntax(t *testing.T, path string) {
	t.Helper()
	bash, err := exec.LookPath("bash")
	if err != nil {
		t.Skipf("bash not on PATH; skipping syntax check")
	}
	cmd := exec.Command(bash, "-n", path)
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("bash -n %s failed: %v\n%s", path, err, out)
	}
}

// TestPhase25TargetTebako covers TargetTebako: emit a Tebako packing layout
// (root/<name>.rb + root/Gemfile + executable press.sh) so the user can
// produce a single-file native binary with `./press.sh` against any host
// that has Docker installed.
func TestPhase25TargetTebako(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_tebako.mochi")
	if err := os.WriteFile(src, []byte("print(\"hi from tebako\")\n"), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, TargetTebako); err != nil {
		t.Fatalf("Build TargetTebako: %v", err)
	}

	scriptPath := filepath.Join(out, "root", "hello_tebako.rb")
	gemfilePath := filepath.Join(out, "root", "Gemfile")
	pressPath := filepath.Join(out, "press.sh")
	for _, p := range []string{scriptPath, gemfilePath, pressPath} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("missing artefact %s: %v", p, err)
		}
	}

	scriptBytes, _ := os.ReadFile(scriptPath)
	if !strings.Contains(string(scriptBytes), "hi from tebako") {
		t.Fatalf("script missing literal:\n%s", scriptBytes)
	}
	gfBytes, _ := os.ReadFile(gemfilePath)
	if !strings.Contains(string(gfBytes), `gem "mochi-runtime"`) {
		t.Fatalf("Gemfile missing mochi-runtime line:\n%s", gfBytes)
	}

	pressBytes, _ := os.ReadFile(pressPath)
	pressStr := string(pressBytes)
	for _, want := range []string{
		"#!/usr/bin/env bash",
		"set -euo pipefail",
		"tebako",
		"--entry-point=hello_tebako.rb",
		"--output=/mnt/w/hello_tebako",
		"--root=/mnt/w/root",
		"MOCHI_TEBAKO_IMAGE",
		"MOCHI_TEBAKO_RUBY",
	} {
		if !strings.Contains(pressStr, want) {
			t.Fatalf("press.sh missing %q:\n%s", want, pressStr)
		}
	}

	info, err := os.Stat(pressPath)
	if err != nil {
		t.Fatalf("stat press.sh: %v", err)
	}
	if info.Mode().Perm()&0o111 == 0 {
		t.Fatalf("press.sh is not executable: mode=%v", info.Mode())
	}

	// Verify the script is valid bash via syntax check (`bash -n`). No Docker
	// or Tebako is invoked.
	checkBashSyntax(t, pressPath)
}
