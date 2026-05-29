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
	if got := d.CacheDir(); got != "/tmp/xdg-test-cache/mochi/python-deps" {
		t.Errorf("CacheDir() = %q; want /tmp/xdg-test-cache/mochi/python-deps", got)
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

func TestPrepareVenvAllocatesWorkDir(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	v, err := d.PrepareVenv()
	if err != nil {
		t.Fatalf("PrepareVenv: %v", err)
	}
	if v == nil {
		t.Fatalf("PrepareVenv returned nil venv")
	}
	if d.WorkDir() == "" {
		t.Errorf("PrepareVenv did not set WorkDir")
	}
	if _, err := os.Stat(d.WorkDir()); err != nil {
		t.Errorf("WorkDir %s not created: %v", d.WorkDir(), err)
	}
	if _, err := os.Stat(d.CacheDir()); err != nil {
		t.Errorf("CacheDir %s not created: %v", d.CacheDir(), err)
	}
	if !strings.HasPrefix(filepath.Base(d.WorkDir()), "mochi-python-") {
		t.Errorf("WorkDir base %q should start with mochi-python-", filepath.Base(d.WorkDir()))
	}
}

func TestPrepareVenvHonoursExplicitWorkDir(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{WorkDir: filepath.Join(tmp, "my-workdir"), NoCache: true})
	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("PrepareVenv: %v", err)
	}
	if d.WorkDir() != filepath.Join(tmp, "my-workdir") {
		t.Errorf("WorkDir() = %q; want %q", d.WorkDir(), filepath.Join(tmp, "my-workdir"))
	}
	if _, err := os.Stat(d.WorkDir()); err != nil {
		t.Errorf("explicit WorkDir not created: %v", err)
	}
}

func TestPrepareVenvIdempotent(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("first PrepareVenv: %v", err)
	}
	first := d.WorkDir()
	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("second PrepareVenv: %v", err)
	}
	if d.WorkDir() != first {
		t.Errorf("second PrepareVenv allocated a new WorkDir %q (was %q)", d.WorkDir(), first)
	}
}

func TestWriteVenvRoot(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	v, err := d.PrepareVenv()
	if err != nil {
		t.Fatalf("PrepareVenv: %v", err)
	}
	v.AddMember(VenvMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})

	root, err := d.WriteVenvRoot(v)
	if err != nil {
		t.Fatalf("WriteVenvRoot: %v", err)
	}
	manifest, err := os.ReadFile(filepath.Join(root, "pyproject.toml"))
	if err != nil {
		t.Fatalf("read pyproject.toml: %v", err)
	}
	if !strings.Contains(string(manifest), `"mochi_user"`) {
		t.Errorf("pyproject.toml missing member mochi_user:\n%s", manifest)
	}
	gitignore, err := os.ReadFile(filepath.Join(root, ".gitignore"))
	if err != nil {
		t.Fatalf("read .gitignore: %v", err)
	}
	if !strings.Contains(string(gitignore), ".venv/") {
		t.Errorf(".gitignore missing .venv/:\n%s", gitignore)
	}
	if !strings.Contains(string(gitignore), "__pycache__/") {
		t.Errorf(".gitignore missing __pycache__/:\n%s", gitignore)
	}
}

func TestWriteVenvRootRejectsBadVenv(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache")})
	defer d.Cleanup()

	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("PrepareVenv: %v", err)
	}
	bad := &Venv{Implementation: "pypy"}
	if _, err := d.WriteVenvRoot(bad); err == nil {
		t.Errorf("WriteVenvRoot accepted bad Implementation; expected error")
	}
}

func TestWriteVenvRootBeforePrepare(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if _, err := d.WriteVenvRoot(DefaultVenv()); err == nil {
		t.Errorf("WriteVenvRoot accepted call before PrepareVenv; expected error")
	}
}

func TestCleanupRemovesAllocatedWorkDir(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("PrepareVenv: %v", err)
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
	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("PrepareVenv: %v", err)
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
	if _, err := d.PrepareVenv(); err != nil {
		t.Fatalf("PrepareVenv: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Fatalf("first Cleanup: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Errorf("second Cleanup: %v; want nil", err)
	}
}

func TestCleanupNoOpBeforePrepare(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.Cleanup(); err != nil {
		t.Errorf("Cleanup before PrepareVenv: %v; want nil", err)
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
	// With XDG_CACHE_HOME set, prefer XDG.
	t.Setenv("XDG_CACHE_HOME", "/xdg-home")
	if got := defaultCacheDir(); got != "/xdg-home/mochi/python-deps" {
		t.Errorf("defaultCacheDir() with XDG = %q; want /xdg-home/mochi/python-deps", got)
	}
	// Without XDG, fall back to HOME/.cache/.
	t.Setenv("XDG_CACHE_HOME", "")
	t.Setenv("HOME", "/home/test")
	if got := defaultCacheDir(); got != "/home/test/.cache/mochi/python-deps" {
		t.Errorf("defaultCacheDir() with HOME = %q; want /home/test/.cache/mochi/python-deps", got)
	}
}
