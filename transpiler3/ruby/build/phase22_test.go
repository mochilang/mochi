package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase22TargetRubyGem covers the TargetRubyGem build target: emit a
// gem layout (<name>.gemspec + lib/<name>.rb) and verify gem build produces
// a valid .gem artefact.
func TestPhase22TargetRubyGem(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}
	// gem ships beside ruby in every Homebrew slot.
	gemPath := filepath.Join(filepath.Dir(tc.Ruby), "gem")
	if _, err := os.Stat(gemPath); err != nil {
		// Fall back to PATH.
		if p, err := exec.LookPath("gem"); err == nil {
			gemPath = p
		} else {
			t.Skipf("gem not found near %s or on PATH", tc.Ruby)
		}
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_gem.mochi")
	if err := os.WriteFile(src, []byte("print(\"hi from gem\")\n"), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, TargetRubyGem); err != nil {
		t.Fatalf("Build TargetRubyGem: %v", err)
	}

	libPath := filepath.Join(out, "lib", "hello_gem.rb")
	specPath := filepath.Join(out, "hello_gem.gemspec")
	for _, p := range []string{libPath, specPath} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("missing artefact %s: %v", p, err)
		}
	}

	libBytes, _ := os.ReadFile(libPath)
	if !strings.Contains(string(libBytes), "hi from gem") {
		t.Fatalf("lib file does not contain literal:\n%s", libBytes)
	}
	specBytes, _ := os.ReadFile(specPath)
	for _, want := range []string{`s.name`, `"hello_gem"`, `lib/hello_gem.rb`, `mochi-runtime`} {
		if !strings.Contains(string(specBytes), want) {
			t.Fatalf("gemspec missing %q:\n%s", want, specBytes)
		}
	}

	cmd := exec.Command(gemPath, "build", "hello_gem.gemspec")
	cmd.Dir = out
	gemOut, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("gem build failed: %v\noutput: %s", err, gemOut)
	}
	gemFile := filepath.Join(out, "hello_gem-0.1.0.gem")
	if _, err := os.Stat(gemFile); err != nil {
		t.Fatalf("expected .gem at %s, got: %v\nbuild output:\n%s", gemFile, err, gemOut)
	}
}
