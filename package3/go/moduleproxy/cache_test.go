package moduleproxy

import (
	"archive/zip"
	"bytes"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCachePathsEscapeUppercase(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	got, err := c.InfoPath("github.com/Spf13/Cobra", "v1.0.0")
	if err != nil {
		t.Fatalf("InfoPath: %v", err)
	}
	// Path should contain the escaped form "!spf13" / "!cobra".
	if !strings.Contains(got, "!spf13") || !strings.Contains(got, "!cobra") {
		t.Errorf("InfoPath did not escape upper-case path: %q", got)
	}
	if !strings.HasSuffix(got, "v1.0.0.info") {
		t.Errorf("InfoPath suffix wrong: %q", got)
	}
}

func TestCachePathsEscapeVersion(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	got, err := c.ModPath("x", "v1.0.0-Pre")
	if err != nil {
		t.Fatalf("ModPath: %v", err)
	}
	if !strings.HasSuffix(got, "v1.0.0-!pre.mod") {
		t.Errorf("ModPath version not escaped: %q", got)
	}
}

func TestStoreZipRoundTrip(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	zipBytes := mustBuildZip(t, map[string]string{
		"example.com/foo@v1.2.3/main.go": "package main\n",
		"example.com/foo@v1.2.3/go.mod":  "module example.com/foo\n\ngo 1.21\n",
	})
	digest, err := c.StoreZip("example.com/foo", "v1.2.3", bytes.NewReader(zipBytes))
	if err != nil {
		t.Fatalf("StoreZip: %v", err)
	}
	if !strings.HasPrefix(digest, "h1:") {
		t.Errorf("digest missing h1: prefix: %q", digest)
	}
	if _, err := c.StoreMod("example.com/foo", "v1.2.3", []byte("module example.com/foo\n\ngo 1.21\n")); err != nil {
		t.Fatalf("StoreMod: %v", err)
	}
	if err := c.StoreInfo("example.com/foo", "v1.2.3", []byte(`{"Version":"v1.2.3"}`)); err != nil {
		t.Fatalf("StoreInfo: %v", err)
	}
	if !c.Has("example.com/foo", "v1.2.3") {
		t.Errorf("cache.Has returned false after StoreZip + StoreMod + StoreInfo")
	}
	if err := c.VerifyZip("example.com/foo", "v1.2.3"); err != nil {
		t.Errorf("VerifyZip after StoreZip: %v", err)
	}
}

func TestHasReturnsFalseWhenAnyArtifactMissing(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	if c.Has("x", "v1") {
		t.Errorf("Has on empty cache returned true")
	}
	// Stash only the .info and confirm Has still returns false.
	if err := c.StoreInfo("x", "v1", []byte(`{"Version":"v1"}`)); err != nil {
		t.Fatalf("StoreInfo: %v", err)
	}
	if c.Has("x", "v1") {
		t.Errorf("Has returned true with only info present")
	}
}

func TestStoreZipRejectsMismatchedPrefix(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	zipBytes := mustBuildZip(t, map[string]string{
		"WRONGPATH/main.go": "package main\n",
	})
	_, err := c.StoreZip("example.com/foo", "v1.0.0", bytes.NewReader(zipBytes))
	if err == nil {
		t.Errorf("StoreZip accepted mismatched-prefix zip; want error")
	}
	// And the artifact must not exist on disk after the rejection.
	zp, _ := c.ZipPath("example.com/foo", "v1.0.0")
	if _, statErr := os.Stat(zp); statErr == nil {
		t.Errorf("StoreZip created %s despite hash failure", zp)
	}
}

func TestVerifyZipDetectsTampering(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	zipBytes := mustBuildZip(t, map[string]string{
		"example.com/foo@v1/main.go": "package main\n",
	})
	if _, err := c.StoreZip("example.com/foo", "v1", bytes.NewReader(zipBytes)); err != nil {
		t.Fatalf("StoreZip: %v", err)
	}
	// Overwrite the zip with different bytes; integrity must fail.
	tampered := mustBuildZip(t, map[string]string{
		"example.com/foo@v1/main.go": "package main\nvar x = 1\n",
	})
	zp, _ := c.ZipPath("example.com/foo", "v1")
	if err := os.WriteFile(zp, tampered, 0o644); err != nil {
		t.Fatalf("tamper write: %v", err)
	}
	err := c.VerifyZip("example.com/foo", "v1")
	if err == nil {
		t.Errorf("VerifyZip returned nil after tampering; want ErrCacheCorrupt")
	}
	if !errors.Is(err, ErrCacheCorrupt) {
		t.Errorf("VerifyZip err = %v; want ErrCacheCorrupt", err)
	}
}

func TestVerifyZipMissingFiles(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	err := c.VerifyZip("x", "v1")
	if !errors.Is(err, ErrCacheCorrupt) {
		t.Errorf("VerifyZip on missing files = %v; want ErrCacheCorrupt", err)
	}
}

func TestStoreModReturnsHashGoModDigest(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	modBytes := []byte("module example.com/foo\n\ngo 1.21\n")
	digest, err := c.StoreMod("example.com/foo", "v1.0.0", modBytes)
	if err != nil {
		t.Fatalf("StoreMod: %v", err)
	}
	if digest != HashGoMod(modBytes) {
		t.Errorf("StoreMod digest %q != HashGoMod %q", digest, HashGoMod(modBytes))
	}
	// And the file must exist.
	mp, _ := c.ModPath("example.com/foo", "v1.0.0")
	got, err := os.ReadFile(mp)
	if err != nil {
		t.Fatalf("read stored mod: %v", err)
	}
	if !bytes.Equal(got, modBytes) {
		t.Errorf("stored .mod body mismatch")
	}
}

func TestWriteFileAtomic(t *testing.T) {
	dir := t.TempDir()
	dst := filepath.Join(dir, "sub", "out.txt")
	if err := WriteFile(dst, []byte("hello"), 0o644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
	got, err := os.ReadFile(dst)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if string(got) != "hello" {
		t.Errorf("WriteFile contents = %q; want hello", got)
	}
	// Confirm no leftover temp files in the directory.
	entries, err := os.ReadDir(filepath.Dir(dst))
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), ".out.txt.tmp") {
			t.Errorf("leftover temp file %q", e.Name())
		}
	}
}

func TestNewCacheCreatesRoot(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "nested", "cache")
	c, err := NewCache(dir)
	if err != nil {
		t.Fatalf("NewCache: %v", err)
	}
	if c.Root != dir {
		// On macOS t.TempDir can resolve through /var symlink; allow that.
		abs, _ := filepath.Abs(dir)
		if c.Root != abs {
			t.Errorf("NewCache.Root = %q; want %q (or its abs form)", c.Root, dir)
		}
	}
	info, err := os.Stat(dir)
	if err != nil {
		t.Errorf("Stat created cache root: %v", err)
	} else if !info.IsDir() {
		t.Errorf("cache root is not a directory")
	}
}

func TestNewCacheRespectsEnvOverride(t *testing.T) {
	dir := t.TempDir()
	t.Setenv("MOCHI_GO_BRIDGE_CACHE", dir)
	c, err := NewCache("")
	if err != nil {
		t.Fatalf("NewCache: %v", err)
	}
	abs, _ := filepath.Abs(dir)
	if c.Root != abs {
		t.Errorf("NewCache(env) root = %q; want %q", c.Root, abs)
	}
}

func TestFingerprintBytes(t *testing.T) {
	got := FingerprintBytes([]byte("hello\n"))
	// sha256("hello\n") = 5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03
	want := "5891b5b522d5df086d0ff0b110fbd9d21bb4fc7163af34d08286a2e846f6be03"
	if got != want {
		t.Errorf("FingerprintBytes = %q; want %q", got, want)
	}
}

// Round-trip a real-looking zip through the cache and re-read it via
// the standard library zip parser to make sure StoreZip did not
// corrupt the body during atomic-write.
func TestStoreZipPreservesBytes(t *testing.T) {
	c := &CacheLayout{Root: t.TempDir()}
	zipBytes := mustBuildZip(t, map[string]string{
		"example.com/foo@v1/main.go": "package main\n",
	})
	if _, err := c.StoreZip("example.com/foo", "v1", bytes.NewReader(zipBytes)); err != nil {
		t.Fatalf("StoreZip: %v", err)
	}
	zp, _ := c.ZipPath("example.com/foo", "v1")
	got, err := os.ReadFile(zp)
	if err != nil {
		t.Fatalf("read stored zip: %v", err)
	}
	zr, err := zip.NewReader(bytes.NewReader(got), int64(len(got)))
	if err != nil {
		t.Fatalf("re-parse stored zip: %v", err)
	}
	if len(zr.File) != 1 {
		t.Errorf("stored zip has %d entries; want 1", len(zr.File))
	}
}
