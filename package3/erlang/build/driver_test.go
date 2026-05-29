package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestNewDriver_Defaults(t *testing.T) {
	d := NewDriver(Options{})
	if d.opts.OTPVersion != "25" {
		t.Errorf("default OTPVersion = %q, want %q", d.opts.OTPVersion, "25")
	}
	if d.opts.CacheDir == "" {
		t.Error("default CacheDir should not be empty")
	}
	if d.WorkDir() != "" {
		t.Error("WorkDir should be empty before PrepareWorkspace")
	}
}

func TestNewDriver_ExplicitOptions(t *testing.T) {
	d := NewDriver(Options{
		CacheDir:      "/tmp/test-cache",
		WorkDir:       "/tmp/test-work",
		OTPVersion:    "26",
		NoCache:       true,
		Verbose:       true,
		Deterministic: true,
	})
	if d.CacheDir() != "" {
		t.Error("CacheDir() should return empty when NoCache=true")
	}
	if d.WorkDir() != "/tmp/test-work" {
		t.Errorf("WorkDir() = %q, want %q", d.WorkDir(), "/tmp/test-work")
	}
	if !d.Verbose() {
		t.Error("Verbose() should be true")
	}
	if !d.Deterministic() {
		t.Error("Deterministic() should be true")
	}
}

func TestPrepareWorkspace_AutoAllocate(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()

	wd := d.WorkDir()
	if wd == "" {
		t.Fatal("WorkDir should be set after PrepareWorkspace")
	}
	if _, err := os.Stat(wd); os.IsNotExist(err) {
		t.Errorf("work dir %s does not exist", wd)
	}
	if !strings.Contains(filepath.Base(wd), "mochi-erlang-") {
		t.Errorf("work dir base %q should contain mochi-erlang-", filepath.Base(wd))
	}
}

func TestPrepareWorkspace_UserSupplied(t *testing.T) {
	dir := t.TempDir()
	sub := filepath.Join(dir, "mywork")
	d := NewDriver(Options{WorkDir: sub, NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if _, err := os.Stat(sub); os.IsNotExist(err) {
		t.Errorf("user-supplied work dir %s was not created", sub)
	}
}

func TestPrepareWorkspace_CreatesCacheDir(t *testing.T) {
	dir := t.TempDir()
	cache := filepath.Join(dir, "cache")
	d := NewDriver(Options{CacheDir: cache})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()
	if _, err := os.Stat(cache); os.IsNotExist(err) {
		t.Errorf("cache dir %s was not created", cache)
	}
}

func TestPrepareWorkspace_Idempotent(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("first PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()
	wd := d.WorkDir()
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("second PrepareWorkspace: %v", err)
	}
	if d.WorkDir() != wd {
		t.Error("WorkDir changed on second PrepareWorkspace call")
	}
}

func TestCleanup_AutoAllocated(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	wd := d.WorkDir()
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if d.WorkDir() != "" {
		t.Error("WorkDir should be empty after Cleanup")
	}
	if _, err := os.Stat(wd); !os.IsNotExist(err) {
		t.Errorf("work dir %s should have been removed", wd)
	}
}

func TestCleanup_UserSupplied_NotRemoved(t *testing.T) {
	dir := t.TempDir()
	d := NewDriver(Options{WorkDir: dir, NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		t.Error("user-supplied work dir should NOT have been removed by Cleanup")
	}
}

func TestCleanup_Idempotent(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup on empty driver: %v", err)
	}
}

func TestSynthRebar3Config_Basic(t *testing.T) {
	d := NewDriver(Options{})
	deps := []DepEntry{
		{Name: "cowboy", Version: "2.12.0"},
		{Name: "hackney", Version: "1.20.1"},
	}
	cfg, err := d.SynthRebar3Config(deps, "25")
	if err != nil {
		t.Fatalf("SynthRebar3Config: %v", err)
	}
	if !strings.Contains(cfg, "{erl_opts, [debug_info]}") {
		t.Error("missing erl_opts")
	}
	if !strings.Contains(cfg, `{cowboy, "2.12.0"}`) {
		t.Errorf("missing cowboy dep, got:\n%s", cfg)
	}
	if !strings.Contains(cfg, `{hackney, "1.20.1"}`) {
		t.Errorf("missing hackney dep, got:\n%s", cfg)
	}
	if !strings.Contains(cfg, `{minimum_otp_vsn, "25"}`) {
		t.Errorf("missing minimum_otp_vsn, got:\n%s", cfg)
	}
}

func TestSynthRebar3Config_DefaultOTPVersion(t *testing.T) {
	d := NewDriver(Options{OTPVersion: "26"})
	cfg, err := d.SynthRebar3Config(nil, "")
	if err != nil {
		t.Fatalf("SynthRebar3Config: %v", err)
	}
	if !strings.Contains(cfg, `{minimum_otp_vsn, "26"}`) {
		t.Errorf("expected driver OTPVersion=26, got:\n%s", cfg)
	}
}

func TestSynthRebar3Config_EmptyDeps(t *testing.T) {
	d := NewDriver(Options{})
	cfg, err := d.SynthRebar3Config(nil, "25")
	if err != nil {
		t.Fatalf("SynthRebar3Config: %v", err)
	}
	if !strings.Contains(cfg, "{deps, [") {
		t.Errorf("expected empty deps block, got:\n%s", cfg)
	}
}

func TestSynthRebar3Lock_Basic(t *testing.T) {
	d := NewDriver(Options{})
	deps := []DepEntry{
		{
			Name:        "cowboy",
			Version:     "2.12.0",
			InnerSHA256: "abc123",
			Deps:        []string{"cowlib", "ranch"},
		},
		{
			Name:    "hackney",
			Version: "1.20.1",
		},
	}
	lock, err := d.SynthRebar3Lock(deps)
	if err != nil {
		t.Fatalf("SynthRebar3Lock: %v", err)
	}
	if !strings.Contains(lock, `<<"cowboy">>`) {
		t.Errorf("missing cowboy entry, got:\n%s", lock)
	}
	if !strings.Contains(lock, `<<"abc123">>`) {
		t.Errorf("missing sha256 hash, got:\n%s", lock)
	}
	if !strings.Contains(lock, `<<"cowlib">>`) {
		t.Errorf("missing cowlib subdep, got:\n%s", lock)
	}
	// Ends with ].
	trimmed := strings.TrimSpace(lock)
	if !strings.HasSuffix(trimmed, "].") {
		t.Errorf("lock file should end with ]., got:\n%s", lock)
	}
}

func TestSynthRebar3Lock_Sorted(t *testing.T) {
	d := NewDriver(Options{})
	deps := []DepEntry{
		{Name: "zlib_dep", Version: "1.0.1"},
		{Name: "aardvark", Version: "2.0.0"},
		{Name: "mnesia_ext", Version: "0.5.0"},
	}
	lock, err := d.SynthRebar3Lock(deps)
	if err != nil {
		t.Fatalf("SynthRebar3Lock: %v", err)
	}
	aIdx := strings.Index(lock, "aardvark")
	mIdx := strings.Index(lock, "mnesia_ext")
	zIdx := strings.Index(lock, "zlib_dep")
	if !(aIdx < mIdx && mIdx < zIdx) {
		t.Errorf("lock entries not sorted alphabetically:\n%s", lock)
	}
}

func TestSynthRebar3Lock_MissingHash_UsesZeros(t *testing.T) {
	d := NewDriver(Options{})
	deps := []DepEntry{{Name: "ranch", Version: "2.1.0"}}
	lock, err := d.SynthRebar3Lock(deps)
	if err != nil {
		t.Fatalf("SynthRebar3Lock: %v", err)
	}
	zeros := strings.Repeat("0", 64)
	if !strings.Contains(lock, zeros) {
		t.Errorf("expected 64 zero-bytes placeholder for missing hash, got:\n%s", lock)
	}
}

func TestWriteRebar3Config_CreatesFiles(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()

	deps := []DepEntry{
		{Name: "cowboy", Version: "2.12.0", InnerSHA256: "deadbeef"},
	}
	cfgPath, err := d.WriteRebar3Config(deps)
	if err != nil {
		t.Fatalf("WriteRebar3Config: %v", err)
	}
	if _, err := os.Stat(cfgPath); os.IsNotExist(err) {
		t.Errorf("rebar.config not found at %s", cfgPath)
	}
	lockPath := filepath.Join(filepath.Dir(cfgPath), "rebar3.lock")
	if _, err := os.Stat(lockPath); os.IsNotExist(err) {
		t.Errorf("rebar3.lock not found at %s", lockPath)
	}
	data, _ := os.ReadFile(cfgPath)
	if !strings.Contains(string(data), "cowboy") {
		t.Errorf("rebar.config missing cowboy dep")
	}
}

func TestWriteRebar3Config_BeforePrepare(t *testing.T) {
	d := NewDriver(Options{})
	_, err := d.WriteRebar3Config(nil)
	if err == nil {
		t.Error("WriteRebar3Config should fail before PrepareWorkspace")
	}
}

func TestErlangShimsDir_CreatesDir(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()

	shimDir, err := d.ErlangShimsDir()
	if err != nil {
		t.Fatalf("ErlangShimsDir: %v", err)
	}
	if !strings.HasSuffix(shimDir, "erlang_shims") {
		t.Errorf("shims dir %q should end with erlang_shims", shimDir)
	}
	if _, err := os.Stat(shimDir); os.IsNotExist(err) {
		t.Errorf("shims dir %s does not exist", shimDir)
	}
}

func TestErlangShimsDir_BeforePrepare(t *testing.T) {
	d := NewDriver(Options{})
	_, err := d.ErlangShimsDir()
	if err == nil {
		t.Error("ErlangShimsDir should fail before PrepareWorkspace")
	}
}

func TestDefaultCacheDir(t *testing.T) {
	dir := defaultCacheDir()
	if dir == "" {
		t.Error("defaultCacheDir should not be empty")
	}
	if !strings.Contains(dir, "mochi") {
		t.Errorf("defaultCacheDir %q should contain 'mochi'", dir)
	}
	if !strings.Contains(dir, "erlang-deps") {
		t.Errorf("defaultCacheDir %q should contain 'erlang-deps'", dir)
	}
}
