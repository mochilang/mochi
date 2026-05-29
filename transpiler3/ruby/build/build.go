package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/ruby/emit"
	"mochi/transpiler3/ruby/lower"
	"mochi/types"
)

// Target selects the Ruby packaging format.
type Target int

const (
	// TargetRubySource emits a single runnable .rb script with the runtime
	// available via $LOAD_PATH ("ruby -I runtime/lib script.rb"). Phase 0 only
	// supports this target.
	TargetRubySource Target = iota

	// TargetRubyGem produces an installable gem (Phase 13).
	TargetRubyGem

	// TargetRubyBundle bundles the script and its dependencies (Phase 14).
	TargetRubyBundle

	// TargetIRubyKernel registers an IRuby Jupyter kernel (Phase 15).
	TargetIRubyKernel

	// TargetTebako packages a self-contained binary via Tebako (Phase 16).
	TargetTebako

	// TargetTruffleNative produces a TruffleRuby AOT native image (Phase 17).
	TargetTruffleNative

	// TargetMRuby produces an mruby binary for embedded use (Phase 18).
	TargetMRuby
)

// Toolchain holds resolved paths to Ruby tools and the detected version.
type Toolchain struct {
	Ruby  string // absolute path to ruby binary
	Bundle string // absolute path to bundle binary (optional)
	Major int    // Ruby major version (3, 4, ...)
	Minor int    // Ruby minor version
}

// resolveToolchain finds Ruby on PATH or via $MOCHI_RUBY / well-known Homebrew
// prefixes and returns a Toolchain. Returns an error if no Ruby 3.2+ is found
// (3.2 is the minimum for Data.define).
func resolveToolchain() (*Toolchain, error) {
	rubyPath, err := findRuby()
	if err != nil {
		return nil, err
	}
	major, minor, err := rubyVersion(rubyPath)
	if err != nil {
		return nil, err
	}
	if major < 3 || (major == 3 && minor < 2) {
		return nil, fmt.Errorf("ruby 3.2+ required (for Data.define); found %d.%d at %s", major, minor, rubyPath)
	}
	tc := &Toolchain{
		Ruby:  rubyPath,
		Major: major,
		Minor: minor,
	}
	if bp, err := exec.LookPath("bundle"); err == nil {
		tc.Bundle = bp
	}
	return tc, nil
}

// findRuby returns a Ruby 3.2+ binary, preferring (in order):
//   1. $MOCHI_RUBY (env override)
//   2. Homebrew slots: /opt/homebrew/opt/ruby{,@3.4,@3.3,@3.2}/bin/ruby
//   3. exec.LookPath("ruby")
//
// We prefer a known-newer slot first so a stale system Ruby on PATH (macOS
// ships /usr/bin/ruby 2.6) does not preempt a Homebrew install.
func findRuby() (string, error) {
	if p := os.Getenv("MOCHI_RUBY"); p != "" {
		if _, err := os.Stat(p); err == nil {
			return p, nil
		}
	}
	candidates := []string{
		"/opt/homebrew/opt/ruby/bin/ruby",
		"/opt/homebrew/opt/ruby@3.4/bin/ruby",
		"/opt/homebrew/opt/ruby@3.3/bin/ruby",
		"/opt/homebrew/opt/ruby@3.2/bin/ruby",
		"/usr/local/opt/ruby/bin/ruby",
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			if maj, _, err := rubyVersion(c); err == nil && maj >= 3 {
				return c, nil
			}
		}
	}
	p, err := exec.LookPath("ruby")
	if err != nil {
		return "", fmt.Errorf("ruby not found on PATH: %w", err)
	}
	return p, nil
}

// rubyVersion runs `ruby --version` and returns the parsed major and minor.
func rubyVersion(rubyPath string) (int, int, error) {
	out, err := exec.Command(rubyPath, "--version").Output()
	if err != nil {
		return 0, 0, fmt.Errorf("ruby --version: %w", err)
	}
	// "ruby 3.4.8 (2026-04-15 ...) [arm64-darwin24]"
	fields := strings.Fields(string(out))
	if len(fields) < 2 {
		return 0, 0, fmt.Errorf("unexpected ruby --version output: %q", string(out))
	}
	v := fields[1]
	parts := strings.SplitN(v, ".", 3)
	if len(parts) < 2 {
		return 0, 0, fmt.Errorf("cannot parse ruby version from %q", v)
	}
	maj, err := strconv.Atoi(parts[0])
	if err != nil {
		return 0, 0, fmt.Errorf("cannot parse ruby major from %q", v)
	}
	min, err := strconv.Atoi(parts[1])
	if err != nil {
		return 0, 0, fmt.Errorf("cannot parse ruby minor from %q", v)
	}
	return maj, min, nil
}

// Driver is the Ruby transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/ruby/ location.
	CacheDir string
	tc       *Toolchain
}

func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".cache", "mochi", "ruby")
}

// Build compiles src to the given target artefact at out. For
// TargetRubySource, out is treated as a directory; the emitted .rb file
// (named after the Mochi module derived from src) is written underneath.
func (d *Driver) Build(src, out string, target Target) error {
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}

	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("ruby build: read %s: %w", src, err)
	}
	_ = srcBytes

	ast, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("ruby build: parse: %w", err)
	}
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("ruby build: typecheck: %w", errs[0])
	}
	prog, err := clower.Lower(ast)
	if err != nil {
		return fmt.Errorf("ruby build: aotir lower: %w", err)
	}

	modName := lower.ModuleName(src)
	fileBase := strings.TrimSuffix(filepath.Base(src), ".mochi")
	sf, err := lower.Lower(prog, fileBase, modName)
	if err != nil {
		return fmt.Errorf("ruby build: ruby lower: %w", err)
	}

	switch target {
	case TargetRubySource:
		if err := os.MkdirAll(out, 0o755); err != nil {
			return err
		}
		if _, err := emit.Emit(sf, out); err != nil {
			return fmt.Errorf("ruby build: emit: %w", err)
		}
		return nil
	}
	return fmt.Errorf("ruby build: target %d not implemented in Phase 0", target)
}
