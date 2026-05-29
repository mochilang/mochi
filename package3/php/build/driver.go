// Package build is the orchestration layer for the MEP-75 PHP bridge pipeline.
// Phase 0 ships only the cache-key + work-dir scaffolding; later phases attach
// the Packagist sparse-index client, the PHP reflection ingest, the extern
// emitter, and the MEP-55 build invocation.
//
// Lifecycle:
//
//	d := build.NewDriver(build.Options{...})
//	w, err := d.PrepareWorkspace()
//	// phase 1-14: ingest, reflect, typemap, externemit, glue, build
//	d.Cleanup()  // remove the scratch work-dir; cache-dir is preserved.
package build

import (
	"fmt"
	"os"
	"path/filepath"
)

// Driver is the top-level entry point for the MEP-75 PHP bridge build pipeline.
type Driver struct {
	opts Options
}

// Options configure a Driver. All fields are optional; sensible defaults are
// applied by NewDriver.
type Options struct {
	// CacheDir is the persistent content-addressed cache root. Default:
	// $XDG_CACHE_HOME/mochi/php-deps/ or ~/.cache/mochi/php-deps/.
	CacheDir string
	// WorkDir is the scratch directory used for a single build. Default:
	// a fresh subdirectory of $TMPDIR/mochi-php-XXXX/.
	WorkDir string
	// PHPBin is the PHP CLI binary to invoke for reflection. Default: "php".
	PHPBin string
	// NoCache disables the cache entirely. Every build re-fetches and
	// re-reflects from scratch. Useful for cache-correctness tests.
	NoCache bool
	// Verbose turns on extra diagnostics in the bridge's own logging.
	Verbose bool
	// Deterministic activates the reproducible-build flags. The bridge
	// passes SOURCE_DATE_EPOCH=0 and refuses to touch any wall-clock-derived state.
	Deterministic bool
}

// NewDriver constructs a Driver with the given options. The work-dir is
// allocated lazily on the first call to PrepareWorkspace so a Driver that is
// never used does not leak a directory.
func NewDriver(opts Options) *Driver {
	if opts.CacheDir == "" {
		opts.CacheDir = defaultCacheDir()
	}
	if opts.PHPBin == "" {
		opts.PHPBin = "php"
	}
	return &Driver{opts: opts}
}

// CacheDir returns the resolved persistent cache directory. May be empty if
// NoCache is set.
func (d *Driver) CacheDir() string {
	if d.opts.NoCache {
		return ""
	}
	return d.opts.CacheDir
}

// WorkDir returns the resolved scratch work directory. Empty if
// PrepareWorkspace has not yet been called.
func (d *Driver) WorkDir() string { return d.opts.WorkDir }

// PHPBin returns the PHP CLI binary path.
func (d *Driver) PHPBin() string { return d.opts.PHPBin }

// Verbose returns whether the driver was configured for verbose output.
func (d *Driver) Verbose() bool { return d.opts.Verbose }

// Deterministic returns whether the driver was configured for reproducible
// builds.
func (d *Driver) Deterministic() bool { return d.opts.Deterministic }

// PrepareWorkspace allocates the scratch work directory (if not already set)
// and ensures the cache directory exists. It returns a Workspace value that
// callers use to track shim files, glue stubs, and the vendor sandbox.
//
// PrepareWorkspace is idempotent: calling it twice with the same Driver
// re-uses the existing work-dir.
func (d *Driver) PrepareWorkspace() (*Workspace, error) {
	if d.opts.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-php-")
		if err != nil {
			return nil, fmt.Errorf("driver: allocate work-dir: %w", err)
		}
		d.opts.WorkDir = dir
	} else {
		if err := os.MkdirAll(d.opts.WorkDir, 0o755); err != nil {
			return nil, fmt.Errorf("driver: create work-dir %s: %w", d.opts.WorkDir, err)
		}
	}
	if !d.opts.NoCache {
		if err := os.MkdirAll(d.opts.CacheDir, 0o755); err != nil {
			return nil, fmt.Errorf("driver: create cache-dir %s: %w", d.opts.CacheDir, err)
		}
	}
	return &Workspace{Root: d.opts.WorkDir}, nil
}

// Cleanup removes the scratch work-dir. The persistent cache-dir is preserved.
// Safe to call even if PrepareWorkspace was never called.
func (d *Driver) Cleanup() error {
	if d.opts.WorkDir == "" {
		return nil
	}
	if err := os.RemoveAll(d.opts.WorkDir); err != nil {
		return fmt.Errorf("driver: cleanup work-dir: %w", err)
	}
	d.opts.WorkDir = ""
	return nil
}

// defaultCacheDir returns the XDG-aware cache directory for PHP bridge deps.
func defaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "php-deps")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return filepath.Join(os.TempDir(), "mochi-php-cache")
	}
	return filepath.Join(home, ".cache", "mochi", "php-deps")
}

// Workspace records the paths of the scratch work directory and the
// sub-directories created during a bridge build.
type Workspace struct {
	// Root is the top-level scratch directory, e.g. /tmp/mochi-php-123456/.
	Root string
	// ShimDir is where synthesised .mochi extern shim files are written.
	ShimDir string
	// GlueDir is where PHP-side forwarding stubs are written.
	GlueDir string
	// VendorDir is where the autoload bridge vendor/ directory is materialised.
	VendorDir string
}

// EnsureSubDirs creates ShimDir, GlueDir, and VendorDir under Root.
// Called during phase 5-7 after PrepareWorkspace.
func (w *Workspace) EnsureSubDirs() error {
	for _, sub := range []struct {
		ptr  *string
		name string
	}{
		{&w.ShimDir, "shims"},
		{&w.GlueDir, "glue"},
		{&w.VendorDir, "vendor"},
	} {
		if *sub.ptr == "" {
			*sub.ptr = filepath.Join(w.Root, sub.name)
		}
		if err := os.MkdirAll(*sub.ptr, 0o755); err != nil {
			return fmt.Errorf("workspace: create %s: %w", sub.name, err)
		}
	}
	return nil
}
