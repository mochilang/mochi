package build

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Driver is the top-level entry point for the MEP-73 bridge build pipeline.
// Phase 0 ships only the cache-key + work-dir scaffolding; later phases attach
// the sparse-index client, the rustdoc ingest, the wrapper synthesiser, and
// the cargo invocation.
//
// Lifecycle:
//
//	d := build.NewDriver(build.Options{...})
//	w, err := d.PrepareWorkspace()
//	// phase 1-7: ingest, synthesise, build
//	d.Cleanup()  // remove the scratch work-dir; cache-dir is preserved.
type Driver struct {
	opts Options
}

// Options configure a Driver. All fields are optional; sensible defaults are
// applied by NewDriver.
type Options struct {
	// CacheDir is the persistent content-addressed cache root. Default:
	// $XDG_CACHE_HOME/mochi/rust-deps/ or ~/.cache/mochi/rust-deps/.
	CacheDir string
	// WorkDir is the scratch directory used for a single build. Default:
	// a fresh subdirectory of $TMPDIR/mochi-rust-XXXX/.
	WorkDir string
	// NoCache disables the cache entirely. Every build re-fetches and
	// re-builds from scratch. Useful for cache-correctness tests.
	NoCache bool
	// Verbose mirrors cargo's --verbose flag and turns on extra diagnostics
	// in the bridge's own logging.
	Verbose bool
	// Deterministic activates the reproducible-build flags. The bridge
	// passes SOURCE_DATE_EPOCH=0 and rustc -C strip=symbols, and refuses
	// to touch any wall-clock-derived state.
	Deterministic bool
}

// NewDriver constructs a Driver with the given options. The work-dir is
// allocated lazily on the first call to PrepareWorkspace so a Driver that is
// never used does not leak a directory.
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

// WorkDir returns the resolved scratch work directory. Empty if
// PrepareWorkspace has not yet been called.
func (d *Driver) WorkDir() string { return d.opts.WorkDir }

// Verbose returns whether the driver was configured for verbose output.
func (d *Driver) Verbose() bool { return d.opts.Verbose }

// Deterministic returns whether the driver was configured for reproducible
// builds.
func (d *Driver) Deterministic() bool { return d.opts.Deterministic }

// PrepareWorkspace allocates the scratch work directory (if not already set)
// and populates it with the workspace root Cargo.toml. The returned Workspace
// reflects the bridge's recommended defaults; callers add members and shared
// deps before the final build step writes the TOML.
//
// PrepareWorkspace is idempotent: calling it twice with the same Driver
// re-uses the existing work-dir.
func (d *Driver) PrepareWorkspace() (*Workspace, error) {
	if d.opts.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-rust-")
		if err != nil {
			return nil, fmt.Errorf("driver: allocate work-dir: %w", err)
		}
		d.opts.WorkDir = dir
	} else {
		if err := os.MkdirAll(d.opts.WorkDir, 0o755); err != nil {
			return nil, fmt.Errorf("driver: create work-dir %s: %w", d.opts.WorkDir, err)
		}
	}
	// Cache dir is a may-exist; ignore the err when NoCache.
	if !d.opts.NoCache {
		if err := os.MkdirAll(d.opts.CacheDir, 0o755); err != nil {
			return nil, fmt.Errorf("driver: create cache-dir %s: %w", d.opts.CacheDir, err)
		}
	}
	return DefaultWorkspace(), nil
}

// WriteWorkspaceRoot serialises the workspace root Cargo.toml into the
// work-dir under rust_workspace/Cargo.toml. The caller must have invoked
// PrepareWorkspace first.
//
// The directory layout written by phase 0:
//
//	<work-dir>/
//	  rust_workspace/
//	    Cargo.toml         # the workspace root
//	    .gitignore         # ignores target/
//	    target/            # cargo's output (created on first build)
//
// Member crates are written by their respective phases (wrapper crates by
// phase 4, user crate by MEP-53's emit, runtime by phase 7).
func (d *Driver) WriteWorkspaceRoot(w *Workspace) (string, error) {
	if d.opts.WorkDir == "" {
		return "", fmt.Errorf("driver: WriteWorkspaceRoot called before PrepareWorkspace")
	}
	if err := w.Validate(); err != nil {
		return "", err
	}
	root := filepath.Join(d.opts.WorkDir, "rust_workspace")
	if err := os.MkdirAll(root, 0o755); err != nil {
		return "", fmt.Errorf("driver: create workspace root %s: %w", root, err)
	}
	manifestPath := filepath.Join(root, "Cargo.toml")
	manifest := w.RenderRootCargoToml()
	if err := os.WriteFile(manifestPath, []byte(manifest), 0o644); err != nil {
		return "", fmt.Errorf("driver: write Cargo.toml: %w", err)
	}
	gitignorePath := filepath.Join(root, ".gitignore")
	if err := os.WriteFile(gitignorePath, []byte("target/\n"), 0o644); err != nil {
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
	if !strings.HasPrefix(filepath.Base(d.opts.WorkDir), "mochi-rust-") {
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
// $TMPDIR/mochi-cache/rust-deps.
func defaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "rust-deps")
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".cache", "mochi", "rust-deps")
	}
	return filepath.Join(os.TempDir(), "mochi-cache", "rust-deps")
}
