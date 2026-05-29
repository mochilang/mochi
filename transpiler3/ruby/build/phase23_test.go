package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase23TargetRubyBundle covers TargetRubyBundle: emit a Gemfile +
// script and verify the Gemfile parses cleanly through Bundler::Definition.
func TestPhase23TargetRubyBundle(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_bundle.mochi")
	if err := os.WriteFile(src, []byte("print(\"hi from bundle\")\n"), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{}
	if err := d.Build(src, out, TargetRubyBundle); err != nil {
		t.Fatalf("Build TargetRubyBundle: %v", err)
	}

	scriptPath := filepath.Join(out, "hello_bundle.rb")
	gemfilePath := filepath.Join(out, "Gemfile")
	for _, p := range []string{scriptPath, gemfilePath} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("missing artefact %s: %v", p, err)
		}
	}

	scriptBytes, _ := os.ReadFile(scriptPath)
	if !strings.Contains(string(scriptBytes), "hi from bundle") {
		t.Fatalf("script does not contain literal:\n%s", scriptBytes)
	}
	gfBytes, _ := os.ReadFile(gemfilePath)
	for _, want := range []string{
		`source "https://rubygems.org"`,
		`gem "mochi-runtime"`,
		`ruby ">= 3.2"`,
		`gem "mochi-runtime", ">= 0.1"`,
		`# frozen_string_literal: true`,
	} {
		if !strings.Contains(string(gfBytes), want) {
			t.Fatalf("Gemfile missing %q:\n%s", want, gfBytes)
		}
	}

	// Validate Gemfile syntax via Bundler::Definition without contacting the
	// network. Bundler is in stdlib for Ruby 3.2+.
	probe := `require "bundler"
defn = Bundler::Definition.build(Pathname.new(ARGV[0]), nil, false)
deps = defn.dependencies.map(&:name).sort
abort("missing mochi-runtime: #{deps.inspect}") unless deps.include?("mochi-runtime")
puts "ok"
`
	cmd := exec.Command(tc.Ruby, "-e", probe, gemfilePath)
	cmd.Dir = out
	probeOut, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("bundler probe failed: %v\noutput: %s", err, probeOut)
	}
	if !strings.Contains(string(probeOut), "ok") {
		t.Fatalf("bundler probe wrong output: %q", probeOut)
	}
}
