package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNewDriverDefaults(t *testing.T) {
	d := NewDriver(Options{})
	if d.CacheDir() == "" {
		t.Error("CacheDir should have a non-empty default")
	}
	if !strings.Contains(d.CacheDir(), "php-deps") {
		t.Errorf("CacheDir %q should contain php-deps", d.CacheDir())
	}
	if d.PHPBin() != "php" {
		t.Errorf("PHPBin = %q; want %q", d.PHPBin(), "php")
	}
	if d.WorkDir() != "" {
		t.Errorf("WorkDir should be empty before PrepareWorkspace; got %q", d.WorkDir())
	}
}

func TestNewDriverNoCacheReturnsEmpty(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if d.CacheDir() != "" {
		t.Errorf("CacheDir should be empty when NoCache=true; got %q", d.CacheDir())
	}
}

func TestNewDriverVerboseDeterministic(t *testing.T) {
	d := NewDriver(Options{Verbose: true, Deterministic: true})
	if !d.Verbose() {
		t.Error("Verbose() should be true")
	}
	if !d.Deterministic() {
		t.Error("Deterministic() should be true")
	}
}

func TestPrepareWorkspaceCreatesDir(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace returned error: %v", err)
	}
	defer d.Cleanup() //nolint:errcheck
	if w.Root == "" {
		t.Error("Workspace.Root should be non-empty")
	}
	if _, err := os.Stat(w.Root); err != nil {
		t.Errorf("work dir %q does not exist: %v", w.Root, err)
	}
	if !strings.Contains(w.Root, "mochi-php-") {
		t.Errorf("WorkDir %q should contain mochi-php- prefix", w.Root)
	}
}

func TestPrepareWorkspaceIdempotent(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	w1, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("first PrepareWorkspace: %v", err)
	}
	defer d.Cleanup() //nolint:errcheck
	w2, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("second PrepareWorkspace: %v", err)
	}
	if w1.Root != w2.Root {
		t.Errorf("PrepareWorkspace not idempotent: %q != %q", w1.Root, w2.Root)
	}
}

func TestPrepareWorkspaceExplicitWorkDir(t *testing.T) {
	dir := t.TempDir()
	d := NewDriver(Options{NoCache: true, WorkDir: dir})
	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if w.Root != dir {
		t.Errorf("WorkDir = %q; want %q", w.Root, dir)
	}
}

func TestCleanupRemovesWorkDir(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	workDir := w.Root
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(workDir); !os.IsNotExist(err) {
		t.Errorf("work dir %q still exists after Cleanup", workDir)
	}
	if d.WorkDir() != "" {
		t.Errorf("WorkDir should be empty after Cleanup; got %q", d.WorkDir())
	}
}

func TestCleanupWithoutPrepare(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.Cleanup(); err != nil {
		t.Errorf("Cleanup without PrepareWorkspace should be a no-op; got: %v", err)
	}
}

func TestCacheDirXDG(t *testing.T) {
	tmp := t.TempDir()
	t.Setenv("XDG_CACHE_HOME", tmp)
	got := defaultCacheDir()
	want := filepath.Join(tmp, "mochi", "php-deps")
	if got != want {
		t.Errorf("defaultCacheDir() = %q; want %q", got, want)
	}
}

func TestWorkspaceEnsureSubDirs(t *testing.T) {
	root := t.TempDir()
	w := &Workspace{Root: root}
	if err := w.EnsureSubDirs(); err != nil {
		t.Fatalf("EnsureSubDirs: %v", err)
	}
	for _, dir := range []string{w.ShimDir, w.GlueDir, w.VendorDir} {
		if _, err := os.Stat(dir); err != nil {
			t.Errorf("sub-dir %q does not exist: %v", dir, err)
		}
	}
	if w.ShimDir != filepath.Join(root, "shims") {
		t.Errorf("ShimDir = %q; want %q", w.ShimDir, filepath.Join(root, "shims"))
	}
	if w.GlueDir != filepath.Join(root, "glue") {
		t.Errorf("GlueDir = %q; want %q", w.GlueDir, filepath.Join(root, "glue"))
	}
	if w.VendorDir != filepath.Join(root, "vendor") {
		t.Errorf("VendorDir = %q; want %q", w.VendorDir, filepath.Join(root, "vendor"))
	}
}

func TestWorkspaceEnsureSubDirsIdempotent(t *testing.T) {
	root := t.TempDir()
	w := &Workspace{Root: root}
	if err := w.EnsureSubDirs(); err != nil {
		t.Fatalf("first EnsureSubDirs: %v", err)
	}
	if err := w.EnsureSubDirs(); err != nil {
		t.Fatalf("second EnsureSubDirs: %v", err)
	}
}
