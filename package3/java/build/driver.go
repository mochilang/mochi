package build

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// Driver is the top-level entry point for the MEP-67 bridge build pipeline.
// Phase 0 ships only the cache-key + work-dir scaffolding; later phases
// attach the Maven client, the reflection tool, the wrapper synthesiser,
// and the javac invocation.
//
// Lifecycle:
//
//	d := build.NewDriver(build.Options{...})
//	w, err := d.PrepareWorkspace()
//	// phase 1-12: fetch, reflect, synthesise, build, publish
//	d.Cleanup()  // remove scratch work-dir; cache-dir is preserved.
type Driver struct {
	opts Options
}

// Options configure a Driver. All fields are optional; sensible defaults are
// applied by NewDriver.
type Options struct {
	// CacheDir is the persistent content-addressed cache root. Default:
	// $XDG_CACHE_HOME/mochi/java-deps/ or ~/.cache/mochi/java-deps/.
	CacheDir string
	// WorkDir is the scratch directory used for a single build. Default:
	// a fresh temp dir under $TMPDIR/mochi-java-XXXX/.
	WorkDir string
	// NoCache disables the cache entirely. Every build re-fetches from scratch.
	NoCache bool
	// Verbose turns on extra diagnostics.
	Verbose bool
}

// NewDriver constructs a Driver with the given options. The work-dir is
// allocated lazily on the first call to PrepareWorkspace.
func NewDriver(opts Options) *Driver {
	if opts.CacheDir == "" {
		opts.CacheDir = defaultCacheDir()
	}
	return &Driver{opts: opts}
}

// CacheDir returns the resolved persistent cache directory. Empty if NoCache is set.
func (d *Driver) CacheDir() string {
	if d.opts.NoCache {
		return ""
	}
	return d.opts.CacheDir
}

// WorkDir returns the resolved scratch work directory. Empty if PrepareWorkspace
// has not yet been called.
func (d *Driver) WorkDir() string { return d.opts.WorkDir }

// Verbose returns whether the driver was configured for verbose output.
func (d *Driver) Verbose() bool { return d.opts.Verbose }

// PrepareWorkspace allocates the scratch work directory (if not already set)
// and returns a default Workspace. The caller adds members before writing the
// workspace root POM.
//
// PrepareWorkspace is idempotent: calling it twice re-uses the existing work-dir.
func (d *Driver) PrepareWorkspace() (*Workspace, error) {
	if d.opts.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-java-")
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
	return DefaultWorkspace(), nil
}

// WriteWorkspaceRoot serialises the workspace root pom.xml into the work-dir.
// The caller must have invoked PrepareWorkspace first. The directory layout:
//
//	<work-dir>/
//	  java_workspace/
//	    pom.xml         -- the aggregator POM
func (d *Driver) WriteWorkspaceRoot(w *Workspace) (string, error) {
	if d.opts.WorkDir == "" {
		return "", fmt.Errorf("driver: WriteWorkspaceRoot called before PrepareWorkspace")
	}
	if err := w.Validate(); err != nil {
		return "", err
	}
	root := filepath.Join(d.opts.WorkDir, "java_workspace")
	if err := os.MkdirAll(root, 0o755); err != nil {
		return "", fmt.Errorf("driver: create workspace root %s: %w", root, err)
	}
	pomPath := filepath.Join(root, "pom.xml")
	pom := w.RenderRootPOM()
	if err := os.WriteFile(pomPath, []byte(pom), 0o644); err != nil {
		return "", fmt.Errorf("driver: write pom.xml: %w", err)
	}
	return root, nil
}

// Cleanup removes the scratch work directory. The cache directory is preserved.
// Cleanup is safe to call multiple times.
func (d *Driver) Cleanup() error {
	if d.opts.WorkDir == "" {
		return nil
	}
	if !strings.HasPrefix(filepath.Base(d.opts.WorkDir), "mochi-java-") {
		// Work-dir was set by caller, not allocated by the driver; don't remove it.
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
func defaultCacheDir() string {
	if xdg := os.Getenv("XDG_CACHE_HOME"); xdg != "" {
		return filepath.Join(xdg, "mochi", "java-deps")
	}
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		return filepath.Join(home, ".cache", "mochi", "java-deps")
	}
	return filepath.Join(os.TempDir(), "mochi-cache", "java-deps")
}
