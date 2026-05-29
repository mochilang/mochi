package build

import (
	"encoding/json"
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
	"mochi/transpiler3/ruby/rtree"
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
	return &Toolchain{
		Ruby:  rubyPath,
		Major: major,
		Minor: minor,
	}, nil
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
		if _, err := os.Stat(p); err != nil {
			return "", fmt.Errorf("MOCHI_RUBY points to %q which is not accessible: %w", p, err)
		}
		return p, nil
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
	tc *Toolchain
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
	case TargetRubyGem:
		return buildGem(sf, fileBase, out)
	case TargetRubyBundle:
		return buildBundle(sf, fileBase, out)
	case TargetIRubyKernel:
		return buildIRubyNotebook(sf, fileBase, out)
	case TargetTebako:
		return buildTebakoPackage(sf, fileBase, out)
	case TargetTruffleNative:
		return buildTruffleNative(sf, fileBase, out)
	case TargetMRuby:
		return buildMRuby(sf, fileBase, out)
	}
	return fmt.Errorf("ruby build: target %d not implemented", target)
}

// buildGem writes a minimal but valid gem layout under out:
//
//	out/<name>.gemspec
//	out/lib/<name>.rb
//
// The gemspec lists exactly the emitted lib file and pins runtime_dependency
// "mochi-runtime" so an installed gem can `require "mochi/runtime"`. The
// version defaults to 0.1.0; callers can rewrite the file before `gem build`
// if they need a different release.
func buildGem(sf *rtree.SourceFile, name, out string) error {
	libDir := filepath.Join(out, "lib")
	if err := os.MkdirAll(libDir, 0o755); err != nil {
		return err
	}
	libPath := filepath.Join(libDir, name+".rb")
	if err := os.WriteFile(libPath, []byte(sf.RubySource()), 0o644); err != nil {
		return fmt.Errorf("ruby build: write lib: %w", err)
	}
	spec := fmt.Sprintf(`# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name        = %q
  s.version     = "0.1.0"
  s.summary     = "Mochi-generated gem"
  s.description = "Generated by the Mochi Ruby transpiler (MEP-56)."
  s.authors     = ["Mochi"]
  s.license     = "Apache-2.0"
  s.required_ruby_version = ">= 3.2"
  s.files       = ["lib/%s.rb"]
  s.require_paths = ["lib"]
  s.add_runtime_dependency "mochi-runtime", ">= 0.1"
end
`, name, name)
	specPath := filepath.Join(out, name+".gemspec")
	if err := os.WriteFile(specPath, []byte(spec), 0o644); err != nil {
		return fmt.Errorf("ruby build: write gemspec: %w", err)
	}
	return nil
}

// buildBundle writes a Bundler-managed script layout under out:
//
//	out/Gemfile
//	out/<name>.rb
//
// Once dependencies are resolved with `bundle install`, the script runs as
// `bundle exec ruby <name>.rb`. The Gemfile pins mochi-runtime so the
// generated code can `require "mochi/runtime"` exactly as in the source
// target. Source line uses rubygems.org so a vanilla bundle install works
// out of the box for end users.
func buildBundle(sf *rtree.SourceFile, name, out string) error {
	if err := os.MkdirAll(out, 0o755); err != nil {
		return err
	}
	scriptPath := filepath.Join(out, name+".rb")
	if err := os.WriteFile(scriptPath, []byte(sf.RubySource()), 0o644); err != nil {
		return fmt.Errorf("ruby build: write script: %w", err)
	}
	gemfile := `# frozen_string_literal: true
source "https://rubygems.org"

ruby ">= 3.2"

gem "mochi-runtime", ">= 0.1"
`
	gfPath := filepath.Join(out, "Gemfile")
	if err := os.WriteFile(gfPath, []byte(gemfile), 0o644); err != nil {
		return fmt.Errorf("ruby build: write Gemfile: %w", err)
	}
	return nil
}

// buildIRubyNotebook emits a Jupyter notebook (.ipynb) carrying the lowered
// Ruby as a single code cell, with kernelspec pinned to the IRuby kernel.
// Open the notebook with `jupyter notebook <name>.ipynb` (or any nbformat 4
// reader) once `iruby register` has installed the kernel.
func buildIRubyNotebook(sf *rtree.SourceFile, name, out string) error {
	if err := os.MkdirAll(out, 0o755); err != nil {
		return err
	}
	source := sf.RubySource()
	cellLines := splitKeepNewline(source)
	nb := map[string]any{
		"nbformat":       4,
		"nbformat_minor": 5,
		"metadata": map[string]any{
			"kernelspec": map[string]any{
				"name":         "ruby",
				"display_name": "Ruby (IRuby)",
				"language":     "ruby",
			},
			"language_info": map[string]any{
				"name":           "ruby",
				"file_extension": ".rb",
			},
		},
		"cells": []any{
			map[string]any{
				"cell_type":       "code",
				"execution_count": nil,
				"metadata":        map[string]any{},
				"outputs":         []any{},
				"source":          cellLines,
			},
		},
	}
	buf, err := json.MarshalIndent(nb, "", " ")
	if err != nil {
		return fmt.Errorf("ruby build: encode notebook: %w", err)
	}
	nbPath := filepath.Join(out, name+".ipynb")
	if err := os.WriteFile(nbPath, buf, 0o644); err != nil {
		return fmt.Errorf("ruby build: write notebook: %w", err)
	}
	return nil
}

// buildTebakoPackage emits a Tebako-ready packing layout:
//
//	<out>/root/<name>.rb
//	<out>/root/Gemfile
//	<out>/press.sh        (chmod +x)
//
// The user runs `./press.sh` (with Docker available) to drive
// `tebako press` against the root tree and obtain a single-file native
// binary at <out>/<name>. The press script pins the Tebako container
// image and Ruby version so the build is reproducible.
func buildTebakoPackage(sf *rtree.SourceFile, name, out string) error {
	rootDir := filepath.Join(out, "root")
	if err := os.MkdirAll(rootDir, 0o755); err != nil {
		return err
	}
	scriptPath := filepath.Join(rootDir, name+".rb")
	if err := os.WriteFile(scriptPath, []byte(sf.RubySource()), 0o644); err != nil {
		return fmt.Errorf("ruby build: write tebako script: %w", err)
	}
	gemfile := `# frozen_string_literal: true
source "https://rubygems.org"

ruby ">= 3.2"

gem "mochi-runtime", ">= 0.1"
`
	if err := os.WriteFile(filepath.Join(rootDir, "Gemfile"), []byte(gemfile), 0o644); err != nil {
		return fmt.Errorf("ruby build: write tebako Gemfile: %w", err)
	}
	press := fmt.Sprintf(`#!/usr/bin/env bash
# Drive Tebako press against the bundled root/ tree to produce a single-file
# native executable. Requires Docker and the tamatebako container image.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
IMAGE="${MOCHI_TEBAKO_IMAGE:-ghcr.io/tamatebako/tebako-ubuntu-20.04:latest}"
RUBY_VERSION="${MOCHI_TEBAKO_RUBY:-3.3.7}"

docker run --rm \
  -v "$HERE":/mnt/w \
  -t "$IMAGE" \
  press \
    --root=/mnt/w/root \
    --entry-point=%s.rb \
    --output=/mnt/w/%s \
    --Ruby="$RUBY_VERSION"
`, name, name)
	pressPath := filepath.Join(out, "press.sh")
	if err := os.WriteFile(pressPath, []byte(press), 0o755); err != nil {
		return fmt.Errorf("ruby build: write press.sh: %w", err)
	}
	return nil
}

// buildTruffleNative emits a TruffleRuby native-image build layout:
//
//	<out>/<name>.rb
//	<out>/native_build.sh    (chmod +x)
//
// native_build.sh drives `native-image --language:ruby` (or, on older
// GraalVM, `truffleruby --native`) to produce a standalone AOT binary
// next to the script. The MOCHI_GRAALVM_HOME env var lets users point
// at a specific GraalVM installation.
func buildTruffleNative(sf *rtree.SourceFile, name, out string) error {
	if err := os.MkdirAll(out, 0o755); err != nil {
		return err
	}
	scriptPath := filepath.Join(out, name+".rb")
	if err := os.WriteFile(scriptPath, []byte(sf.RubySource()), 0o644); err != nil {
		return fmt.Errorf("ruby build: write truffle script: %w", err)
	}
	script := fmt.Sprintf(`#!/usr/bin/env bash
# Build a TruffleRuby native-image of %[1]s.rb. Requires GraalVM with the
# Ruby component installed (`+"`gu install ruby`"+`).
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
GRAAL_HOME="${MOCHI_GRAALVM_HOME:-$GRAALVM_HOME}"
if [[ -z "$GRAAL_HOME" ]]; then
  echo "MOCHI_GRAALVM_HOME or GRAALVM_HOME must be set" >&2
  exit 1
fi

NATIVE_IMAGE="$GRAAL_HOME/bin/native-image"
if [[ ! -x "$NATIVE_IMAGE" ]]; then
  echo "native-image not found at $NATIVE_IMAGE" >&2
  exit 1
fi

cd "$HERE"
"$NATIVE_IMAGE" \
  --language:ruby \
  --no-fallback \
  --initialize-at-build-time \
  -o "%[1]s" \
  "%[1]s.rb"
`, name)
	if err := os.WriteFile(filepath.Join(out, "native_build.sh"), []byte(script), 0o755); err != nil {
		return fmt.Errorf("ruby build: write native_build.sh: %w", err)
	}
	return nil
}

// buildMRuby emits an mruby compile layout suitable for embedding:
//
//	<out>/<name>.rb
//	<out>/build_config.rb
//	<out>/mruby_build.sh     (chmod +x)
//
// mruby_build.sh compiles the script to mruby bytecode (.mrb) via mrbc
// and, if MRUBY_HOME points at a checkout, also drives a full
// build_config.rb-based build into a self-contained binary. MRUBY_HOME
// overrides the default expected location.
func buildMRuby(sf *rtree.SourceFile, name, out string) error {
	if err := os.MkdirAll(out, 0o755); err != nil {
		return err
	}
	scriptPath := filepath.Join(out, name+".rb")
	if err := os.WriteFile(scriptPath, []byte(sf.RubySource()), 0o644); err != nil {
		return fmt.Errorf("ruby build: write mruby script: %w", err)
	}
	cfg := fmt.Sprintf(`# frozen_string_literal: true
MRuby::Build.new do |conf|
  toolchain :gcc
  conf.gembox 'default'
  conf.gem core: 'mruby-bin-mrbc'
  conf.gem core: 'mruby-bin-mruby'
  conf.gem '#{__dir__}'
  conf.bins = ['%s']
end
`, name)
	if err := os.WriteFile(filepath.Join(out, "build_config.rb"), []byte(cfg), 0o644); err != nil {
		return fmt.Errorf("ruby build: write build_config.rb: %w", err)
	}
	script := fmt.Sprintf(`#!/usr/bin/env bash
# Compile %[1]s.rb to mruby bytecode and (optionally) drive a full mruby
# build via build_config.rb. Requires MRUBY_HOME to point at an mruby
# source checkout when producing a binary.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
MRUBY_HOME="${MRUBY_HOME:-}"
MRBC="${MOCHI_MRBC:-mrbc}"

cd "$HERE"
"$MRBC" -o "%[1]s.mrb" "%[1]s.rb"

if [[ -n "$MRUBY_HOME" && -d "$MRUBY_HOME" ]]; then
  cp "$HERE/build_config.rb" "$MRUBY_HOME/build_config.rb"
  ( cd "$MRUBY_HOME" && rake )
  cp "$MRUBY_HOME/build/host/bin/%[1]s" "$HERE/%[1]s" 2>/dev/null || true
fi
`, name)
	if err := os.WriteFile(filepath.Join(out, "mruby_build.sh"), []byte(script), 0o755); err != nil {
		return fmt.Errorf("ruby build: write mruby_build.sh: %w", err)
	}
	return nil
}

// splitKeepNewline mirrors the Jupyter "source" field convention: each list
// element ends in a "\n" except (optionally) the last one.
func splitKeepNewline(s string) []string {
	if s == "" {
		return []string{}
	}
	parts := strings.SplitAfter(s, "\n")
	if last := len(parts) - 1; parts[last] == "" {
		parts = parts[:last]
	}
	return parts
}
