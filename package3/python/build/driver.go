package build

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Driver is the top-level entry point for the MEP-71 bridge build pipeline.
// Phase 0 ships only the cache-key + work-dir scaffolding; later phases attach
// the simple-index client, the PEP 561 stub ingest, the wrapper synthesiser,
// and the pip / uv invocation that materialises the venv.
//
// Lifecycle:
//
//	d := build.NewDriver(build.Options{...})
//	v, err := d.PrepareVenv()
//	// phase 1-8: ingest, synthesise, install, build
//	d.Cleanup()  // remove the scratch work-dir; cache-dir is preserved.
type Driver struct {
	opts Options
}

// Options configure a Driver. All fields are optional; sensible defaults are
// applied by NewDriver.
type Options struct {
	// CacheDir is the persistent content-addressed cache root for downloaded
	// wheels and resolved metadata. Default:
	// $XDG_CACHE_HOME/mochi/python-deps/ or ~/.cache/mochi/python-deps/.
	CacheDir string
	// WorkDir is the scratch directory used for a single build. Default:
	// a fresh subdirectory of $TMPDIR/mochi-python-XXXX/.
	WorkDir string
	// NoCache disables the cache entirely. Every build re-fetches and
	// re-resolves from scratch. Useful for cache-correctness tests.
	NoCache bool
	// Verbose mirrors pip / uv's --verbose flag and turns on extra
	// diagnostics in the bridge's own logging.
	Verbose bool
	// Deterministic activates the reproducible-build flags. The bridge
	// passes SOURCE_DATE_EPOCH=0, PYTHONHASHSEED=0, and refuses to touch
	// any wall-clock-derived state. Required by the lockfile gate.
	Deterministic bool
}

// NewDriver constructs a Driver with the given options. The work-dir is
// allocated lazily on the first call to PrepareVenv so a Driver that is never
// used does not leak a directory.
func NewDriver(opts Options) *Driver {
	if opts.CacheDir == "" {
		opts.CacheDir = defaultCacheDir()
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

// WorkDir returns the resolved scratch work directory. Empty if PrepareVenv
// has not yet been called.
func (d *Driver) WorkDir() string { return d.opts.WorkDir }

// Verbose returns whether the driver was configured for verbose output.
func (d *Driver) Verbose() bool { return d.opts.Verbose }

// Deterministic returns whether the driver was configured for reproducible
// builds.
func (d *Driver) Deterministic() bool { return d.opts.Deterministic }

// PrepareVenv allocates the scratch work directory (if not already set) and
// populates it with the venv root pyproject.toml. The returned Venv reflects
// the bridge's recommended defaults; callers add members and shared deps
// before the final build step writes the TOML.
//
// PrepareVenv is idempotent: calling it twice with the same Driver re-uses
// the existing work-dir.
func (d *Driver) PrepareVenv() (*Venv, error) {
	if d.opts.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-python-")
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
	return DefaultVenv(), nil
}

// WriteVenvRoot serialises the venv root pyproject.toml into the work-dir
// under python_workspace/pyproject.toml. The caller must have invoked
// PrepareVenv first.
//
// The directory layout written by phase 0:
//
//	<work-dir>/
//	  python_workspace/
//	    pyproject.toml         # the venv root
//	    .gitignore             # ignores .venv/ and __pycache__/
//	    .venv/                 # pip / uv's output (created on first install)
//
// Member modules are written by their respective phases (wrapper modules by
// phase 5, user module by MEP-71 phase 10, runtime by phase 8).
func (d *Driver) WriteVenvRoot(v *Venv) (string, error) {
	if d.opts.WorkDir == "" {
		return "", fmt.Errorf("driver: WriteVenvRoot called before PrepareVenv")
	}
	if err := v.Validate(); err != nil {
		return "", err
	}
	root := filepath.Join(d.opts.WorkDir, "python_workspace")
	if err := os.MkdirAll(root, 0o755); err != nil {
		return "", fmt.Errorf("driver: create venv root %s: %w", root, err)
	}
	manifestPath := filepath.Join(root, "pyproject.toml")
	manifest := v.RenderPyprojectToml()
	if err := os.WriteFile(manifestPath, []byte(manifest), 0o644); err != nil {
		return "", fmt.Errorf("driver: write pyproject.toml: %w", err)
	}
	gitignorePath := filepath.Join(root, ".gitignore")
	if err := os.WriteFile(gitignorePath, []byte(".venv/\n__pycache__/\n*.pyc\n"), 0o644); err != nil {
		return "", fmt.Errorf("driver: write .gitignore: %w", err)
	}
	return root, nil
}

// Cleanup removes the scratch work directory. The cache directory is
// preserved across calls. Cleanup is safe to call multiple times.
func (d *Driver) Cleanup() error {
	if d.opts.WorkDir == "" {
		return nil
	}
	if !strings.HasPrefix(filepath.Base(d.opts.WorkDir), "mochi-python-") {
		// The work-dir was set by the caller, not allocated by the driver.
		// Don't remove a directory we didn't create.
		return nil
	}
	if err := os.RemoveAll(d.opts.WorkDir); err != nil {
		return fmt.Errorf("driver: cleanup work-dir %s: %w", d.opts.WorkDir, err)
	}
	d.opts.WorkDir = ""
	return nil
}

// defaultCacheDir returns the bridge's default content-addressed cache root.
// It honours $XDG_CACHE_HOME when set, otherwise falls back to ~/.cache/.
// If neither is available (e.g., in a sandbox with no home), the result is
// $TMPDIR/mochi-cache/python-deps.
func defaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "python-deps")
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".cache", "mochi", "python-deps")
	}
	return filepath.Join(os.TempDir(), "mochi-cache", "python-deps")
}
