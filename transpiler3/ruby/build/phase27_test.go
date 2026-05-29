package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase27TargetMRuby covers TargetMRuby: emit a script + build_config.rb
// + mruby_build.sh that compiles to mruby bytecode (and optionally a binary
// when MRUBY_HOME points at a checkout). The mruby toolchain is not invoked.
func TestPhase27TargetMRuby(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_mruby.mochi")
	if err := os.WriteFile(src, []byte("print(\"hi from mruby\")\n"), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{}
	if err := d.Build(src, out, TargetMRuby); err != nil {
		t.Fatalf("Build TargetMRuby: %v", err)
	}

	scriptPath := filepath.Join(out, "hello_mruby.rb")
	cfgPath := filepath.Join(out, "build_config.rb")
	buildPath := filepath.Join(out, "mruby_build.sh")
	for _, p := range []string{scriptPath, cfgPath, buildPath} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("missing artefact %s: %v", p, err)
		}
	}

	scriptBytes, _ := os.ReadFile(scriptPath)
	if !strings.Contains(string(scriptBytes), "hi from mruby") {
		t.Fatalf("script missing literal:\n%s", scriptBytes)
	}

	cfgBytes, _ := os.ReadFile(cfgPath)
	cfgStr := string(cfgBytes)
	for _, want := range []string{
		"MRuby::Build.new",
		"toolchain :gcc",
		"gembox 'default'",
		"conf.bins = ['hello_mruby']",
		"conf.gem core: 'mruby-bin-mrbc'",
		"conf.gem core: 'mruby-bin-mruby'",
		"conf.gem '#{__dir__}'",
		"# frozen_string_literal: true",
	} {
		if !strings.Contains(cfgStr, want) {
			t.Fatalf("build_config.rb missing %q:\n%s", want, cfgStr)
		}
	}

	buildBytes, _ := os.ReadFile(buildPath)
	buildStr := string(buildBytes)
	for _, want := range []string{
		"#!/usr/bin/env bash",
		"set -euo pipefail",
		`"$MRBC" -o "hello_mruby.mrb" "hello_mruby.rb"`,
		"MRUBY_HOME",
		"MOCHI_MRBC",
		"rake",
	} {
		if !strings.Contains(buildStr, want) {
			t.Fatalf("mruby_build.sh missing %q:\n%s", want, buildStr)
		}
	}

	info, err := os.Stat(buildPath)
	if err != nil {
		t.Fatalf("stat mruby_build.sh: %v", err)
	}
	if info.Mode().Perm()&0o111 == 0 {
		t.Fatalf("mruby_build.sh is not executable: mode=%v", info.Mode())
	}

	checkBashSyntax(t, buildPath)
}
