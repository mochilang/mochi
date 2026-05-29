package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNewDriverDefaultCacheDir(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", "/tmp/xdg-test-cache")
	d := NewDriver(Options{})
	if got := d.CacheDir(); got != "/tmp/xdg-test-cache/mochi/go-deps" {
		t.Errorf("CacheDir() = %q; want /tmp/xdg-test-cache/mochi/go-deps", got)
	}
}

func TestNewDriverNoCache(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if got := d.CacheDir(); got != "" {
		t.Errorf("CacheDir() with NoCache = %q; want empty", got)
	}
}

func TestNewDriverCustomCacheDir(t *testing.T) {
	d := NewDriver(Options{CacheDir: "/var/cache/mochi-test"})
	if got := d.CacheDir(); got != "/var/cache/mochi-test" {
		t.Errorf("CacheDir() = %q; want /var/cache/mochi-test", got)
	}
}

func TestPrepareWorkspaceAllocatesWorkDir(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if w == nil {
		t.Fatalf("PrepareWorkspace returned nil workspace")
	}
	if d.WorkDir() == "" {
		t.Errorf("PrepareWorkspace did not set WorkDir")
	}
	if _, err := os.Stat(d.WorkDir()); err != nil {
		t.Errorf("WorkDir %s not created: %v", d.WorkDir(), err)
	}
	if _, err := os.Stat(d.CacheDir()); err != nil {
		t.Errorf("CacheDir %s not created: %v", d.CacheDir(), err)
	}
}

func TestPrepareWorkspaceHonoursExplicitWorkDir(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{WorkDir: filepath.Join(tmp, "my-workdir"), NoCache: true})
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if d.WorkDir() != filepath.Join(tmp, "my-workdir") {
		t.Errorf("WorkDir() = %q; want %q", d.WorkDir(), filepath.Join(tmp, "my-workdir"))
	}
	if _, err := os.Stat(d.WorkDir()); err != nil {
		t.Errorf("explicit WorkDir not created: %v", err)
	}
}

func TestPrepareWorkspaceIdempotent(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("first PrepareWorkspace: %v", err)
	}
	first := d.WorkDir()
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("second PrepareWorkspace: %v", err)
	}
	if d.WorkDir() != first {
		t.Errorf("second PrepareWorkspace allocated a new WorkDir %q (was %q)", d.WorkDir(), first)
	}
}

func TestWriteWorkspaceRoot(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	w.AddModule(WorkspaceModule{ImportPath: "mochi_user", Path: "mochi_user", Kind: ModuleUser})

	root, err := d.WriteWorkspaceRoot(w)
	if err != nil {
		t.Fatalf("WriteWorkspaceRoot: %v", err)
	}
	manifest, err := os.ReadFile(filepath.Join(root, "go.work"))
	if err != nil {
		t.Fatalf("read go.work: %v", err)
	}
	if !strings.Contains(string(manifest), "./mochi_user") {
		t.Errorf("go.work missing module mochi_user:\n%s", manifest)
	}
	gitignoreBytes, err := os.ReadFile(filepath.Join(root, ".gitignore"))
	if err != nil {
		t.Fatalf("read .gitignore: %v", err)
	}
	gitignore := string(gitignoreBytes)
	for _, want := range []string{"*.a", "*.h", "go.work.sum"} {
		if !strings.Contains(gitignore, want) {
			t.Errorf(".gitignore missing %q\n%s", want, gitignore)
		}
	}
}

func TestWriteWorkspaceRootRejectsBadWorkspace(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	bad := &Workspace{GoVersion: "nightly"}
	if _, err := d.WriteWorkspaceRoot(bad); err == nil {
		t.Errorf("WriteWorkspaceRoot accepted invalid go-version; expected error")
	}
}

func TestWriteWorkspaceRootBeforePrepare(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if _, err := d.WriteWorkspaceRoot(DefaultWorkspace()); err == nil {
		t.Errorf("WriteWorkspaceRoot accepted call before PrepareWorkspace; expected error")
	}
}

func TestCleanupRemovesAllocatedWorkDir(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	work := d.WorkDir()
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(work); !os.IsNotExist(err) {
		t.Errorf("Cleanup left allocated WorkDir behind: %v", err)
	}
	if d.WorkDir() != "" {
		t.Errorf("WorkDir() = %q after Cleanup; want empty", d.WorkDir())
	}
}

func TestCleanupSkipsUserProvidedWorkDir(t *testing.T) {
	tmp := t.TempDir()
	work := filepath.Join(tmp, "user-dir")
	if err := os.MkdirAll(work, 0o755); err != nil {
		t.Fatal(err)
	}
	d := NewDriver(Options{WorkDir: work, NoCache: true})
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(work); err != nil {
		t.Errorf("Cleanup removed user-provided WorkDir %s: %v", work, err)
	}
}

func TestCleanupIdempotent(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Fatalf("first Cleanup: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Errorf("second Cleanup: %v; want nil", err)
	}
}

func TestDriverOptionsAccessors(t *testing.T) {
	d := NewDriver(Options{Verbose: true, Deterministic: true, NoCache: true})
	if !d.Verbose() {
		t.Errorf("Verbose() = false; want true")
	}
	if !d.Deterministic() {
		t.Errorf("Deterministic() = false; want true")
	}
}

func TestDefaultCacheDirFallbacks(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", "/xdg-home")
	if got := defaultCacheDir(); got != "/xdg-home/mochi/go-deps" {
		t.Errorf("defaultCacheDir() with XDG = %q; want /xdg-home/mochi/go-deps", got)
	}
	t.Setenv("XDG_CACHE_HOME", "")
	t.Setenv("HOME", "/home/test")
	if got := defaultCacheDir(); got != "/home/test/.cache/mochi/go-deps" {
		t.Errorf("defaultCacheDir() with HOME = %q; want /home/test/.cache/mochi/go-deps", got)
	}
}

func TestDefaultCacheDirTmpFallback(t *testing.T) {
	t.Setenv("XDG_CACHE_HOME", "")
	t.Setenv("HOME", "")
	// On some platforms UserHomeDir falls back to other env vars; if it
	// still returns a value the test is skipped because we cannot force
	// the TMPDIR branch deterministically.
	if home, err := os.UserHomeDir(); err == nil && home != "" {
		t.Skip("UserHomeDir() returned non-empty even with HOME unset")
	}
	got := defaultCacheDir()
	if !strings.Contains(got, "mochi-cache/go-deps") {
		t.Errorf("defaultCacheDir() = %q; want a path containing mochi-cache/go-deps", got)
	}
}
