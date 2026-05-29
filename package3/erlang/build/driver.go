// Package build is the top-level entry point for the MEP-66 Erlang bridge
// build pipeline. Phase 0 ships the cache-dir / work-dir scaffolding plus
// the rebar.config and rebar3.lock synthesisers. Later phases attach the
// Hex.pm index client, the BEAM abstract-code ingest, the shim emitters,
// and the rebar3 compilation step.
//
// Lifecycle:
//
//	d := build.NewDriver(build.Options{...})
//	if err := d.PrepareWorkspace(); err != nil { ... }
//	// phases 1-5: ingest, resolve, synthesise shims
//	cfg, err := d.SynthRebar3Config(deps, otpVsn)
//	if err := d.RunRebar3Compile(workdir); err != nil { ... }
//	d.Cleanup()  // removes the scratch work-dir; cache-dir is preserved.
package build

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// Driver is the top-level pipeline controller for the MEP-66 Erlang bridge.
type Driver struct {
	opts Options
}

// Options configure a Driver. All fields are optional; NewDriver applies
// sensible defaults.
type Options struct {
	// CacheDir is the persistent content-addressed cache for downloaded
	// Hex.pm tarballs and BEAM abstract-code extracts.
	// Default: $XDG_CACHE_HOME/mochi/erlang-deps/ or ~/.cache/mochi/erlang-deps/.
	CacheDir string

	// WorkDir is the scratch directory used for a single build. Default:
	// a fresh subdirectory under $TMPDIR/mochi-erlang-XXXX/.
	WorkDir string

	// NoCache disables the persistent cache. Every build re-fetches from
	// Hex.pm.
	NoCache bool

	// Verbose enables extra diagnostic output in the bridge's own logging.
	Verbose bool

	// Deterministic activates reproducible-build flags. The bridge sets
	// SOURCE_DATE_EPOCH=0 and refuses wall-clock-derived state.
	Deterministic bool

	// OTPVersion is the minimum OTP version written to rebar.config as
	// {minimum_otp_vsn, ...}. Default "25".
	OTPVersion string
}

// NewDriver constructs a Driver. The work-dir is allocated lazily on the
// first call to PrepareWorkspace.
func NewDriver(opts Options) *Driver {
	if opts.CacheDir == "" {
		opts.CacheDir = defaultCacheDir()
	}
	if opts.OTPVersion == "" {
		opts.OTPVersion = "25"
	}
	return &Driver{opts: opts}
}

// CacheDir returns the resolved persistent cache directory.
func (d *Driver) CacheDir() string {
	if d.opts.NoCache {
		return ""
	}
	return d.opts.CacheDir
}

// WorkDir returns the scratch work directory. Empty until PrepareWorkspace
// has been called.
func (d *Driver) WorkDir() string { return d.opts.WorkDir }

// Verbose returns whether the driver was configured for verbose output.
func (d *Driver) Verbose() bool { return d.opts.Verbose }

// Deterministic returns whether the driver was configured for reproducible
// builds.
func (d *Driver) Deterministic() bool { return d.opts.Deterministic }

// PrepareWorkspace allocates the scratch work directory (if not already set)
// and creates the cache directory structure. Idempotent.
func (d *Driver) PrepareWorkspace() error {
	if d.opts.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-erlang-")
		if err != nil {
			return fmt.Errorf("build: allocate work-dir: %w", err)
		}
		d.opts.WorkDir = dir
	} else {
		if err := os.MkdirAll(d.opts.WorkDir, 0o755); err != nil {
			return fmt.Errorf("build: create work-dir %s: %w", d.opts.WorkDir, err)
		}
	}
	if !d.opts.NoCache {
		if err := os.MkdirAll(d.opts.CacheDir, 0o755); err != nil {
			return fmt.Errorf("build: create cache-dir %s: %w", d.opts.CacheDir, err)
		}
	}
	return nil
}

// DepEntry describes one Erlang/OTP dependency for the synthesised
// rebar.config and rebar3.lock.
type DepEntry struct {
	// Name is the Hex.pm package name (Erlang atom form, e.g. "cowboy").
	Name string
	// Version is the exact resolved version string from mochi.lock
	// (e.g. "2.12.0").
	Version string
	// InnerSHA256 is the SHA-256 hex string of the inner contents.tar.gz,
	// used as the hash in rebar3.lock. Empty until phase 1 populates it.
	InnerSHA256 string
	// Deps lists the transitive dependency names (without versions) for
	// the rebar.config deps ordering.
	Deps []string
}

// SynthRebar3Config generates a rebar.config for the erlang_workspace/
// directory. deps are the resolved packages from mochi.lock; otpVsn is
// the minimum OTP version written as {minimum_otp_vsn, "..."}.
//
// The output is a valid Erlang term that rebar3 parses directly.
func (d *Driver) SynthRebar3Config(deps []DepEntry, otpVsn string) (string, error) {
	if otpVsn == "" {
		otpVsn = d.opts.OTPVersion
	}
	var sb strings.Builder
	sb.WriteString("{erl_opts, [debug_info]}.\n\n")

	// {deps, [...]}
	sb.WriteString("{deps, [\n")
	for i, dep := range deps {
		sb.WriteString(fmt.Sprintf("  {%s, %q}", dep.Name, dep.Version))
		if i < len(deps)-1 {
			sb.WriteString(",")
		}
		sb.WriteString("\n")
	}
	sb.WriteString("]}.\n\n")

	// {minimum_otp_vsn, "..."}
	sb.WriteString(fmt.Sprintf("{minimum_otp_vsn, %q}.\n", otpVsn))

	return sb.String(), nil
}

// SynthRebar3Lock generates a rebar3.lock file from the resolved deps.
// The lock file is a list of Erlang tuples:
//
//	[{<<"name">>, {hex, <<"name">>, <<"version">>, <<"sha256">>, [<<"dep1">>,...], <<"default">>}}, ...]
func (d *Driver) SynthRebar3Lock(deps []DepEntry) (string, error) {
	// Sort by name for deterministic output.
	sorted := make([]DepEntry, len(deps))
	copy(sorted, deps)
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].Name < sorted[j].Name
	})

	var sb strings.Builder
	sb.WriteString("[\n")
	for i, dep := range sorted {
		// Build the sub-dep list.
		subDeps := "[]"
		if len(dep.Deps) > 0 {
			var parts []string
			for _, d := range dep.Deps {
				parts = append(parts, fmt.Sprintf("<<%q>>", d))
			}
			subDeps = "[" + strings.Join(parts, ", ") + "]"
		}
		hash := dep.InnerSHA256
		if hash == "" {
			hash = strings.Repeat("0", 64)
		}
		sb.WriteString(fmt.Sprintf("  {<<%q>>, {hex, <<%q>>, <<%q>>, <<%q>>, %s, <<\"default\">>}}",
			dep.Name, dep.Name, dep.Version, hash, subDeps))
		if i < len(sorted)-1 {
			sb.WriteString(",")
		}
		sb.WriteString("\n")
	}
	sb.WriteString("].\n")
	return sb.String(), nil
}

// WriteRebar3Config writes the synthesised rebar.config to
// <workdir>/erlang_workspace/rebar.config, creating the directory as needed.
// Returns the path written.
func (d *Driver) WriteRebar3Config(deps []DepEntry) (string, error) {
	if d.opts.WorkDir == "" {
		return "", fmt.Errorf("build: WriteRebar3Config called before PrepareWorkspace")
	}
	wsDir := filepath.Join(d.opts.WorkDir, "erlang_workspace")
	if err := os.MkdirAll(wsDir, 0o755); err != nil {
		return "", fmt.Errorf("build: create workspace dir: %w", err)
	}
	cfg, err := d.SynthRebar3Config(deps, "")
	if err != nil {
		return "", err
	}
	cfgPath := filepath.Join(wsDir, "rebar.config")
	if err := os.WriteFile(cfgPath, []byte(cfg), 0o644); err != nil {
		return "", fmt.Errorf("build: write rebar.config: %w", err)
	}
	// Write rebar3.lock alongside.
	lock, err := d.SynthRebar3Lock(deps)
	if err != nil {
		return "", err
	}
	lockPath := filepath.Join(wsDir, "rebar3.lock")
	if err := os.WriteFile(lockPath, []byte(lock), 0o644); err != nil {
		return "", fmt.Errorf("build: write rebar3.lock: %w", err)
	}
	return cfgPath, nil
}

// ErlangShimsDir returns the path to the erlang_shims/ directory under
// the work directory. Created lazily.
func (d *Driver) ErlangShimsDir() (string, error) {
	if d.opts.WorkDir == "" {
		return "", fmt.Errorf("build: ErlangShimsDir called before PrepareWorkspace")
	}
	dir := filepath.Join(d.opts.WorkDir, "erlang_shims")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("build: create erlang_shims dir: %w", err)
	}
	return dir, nil
}

// Cleanup removes the scratch work directory. The cache directory is
// preserved. Safe to call multiple times.
func (d *Driver) Cleanup() error {
	if d.opts.WorkDir == "" {
		return nil
	}
	if !strings.HasPrefix(filepath.Base(d.opts.WorkDir), "mochi-erlang-") {
		// Work-dir was user-supplied, not allocated by the driver. Don't remove it.
		return nil
	}
	if err := os.RemoveAll(d.opts.WorkDir); err != nil {
		return fmt.Errorf("build: cleanup work-dir %s: %w", d.opts.WorkDir, err)
	}
	d.opts.WorkDir = ""
	return nil
}

// defaultCacheDir returns the bridge's default content-addressed cache root.
func defaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "erlang-deps")
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".cache", "mochi", "erlang-deps")
	}
	return filepath.Join(os.TempDir(), "mochi-cache", "erlang-deps")
}
