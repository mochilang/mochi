package nuget_test

import (
	"errors"
	"io"
	"os"
	"strings"
	"testing"

	"mochi/package3/dotnet/nuget"
)

func TestNewCache(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	if c.Dir != dir {
		t.Errorf("Cache.Dir = %q; want %q", c.Dir, dir)
	}
}

func TestCache_Store_and_Has(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	content := "fake nupkg data for testing"
	digest, n, err := c.Store("MyPkg", "1.0.0", strings.NewReader(content))
	if err != nil {
		t.Fatalf("Store error: %v", err)
	}
	if n != int64(len(content)) {
		t.Errorf("Store returned %d bytes; want %d", n, len(content))
	}
	if len(digest) == 0 {
		t.Error("Store returned empty digest")
	}
	if !c.Has(digest) {
		t.Error("Has should return true after Store")
	}
}

func TestCache_Has_missing(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	fakeDigest := strings.Repeat("a", 128) // 512-bit hex = 128 chars
	if c.Has(fakeDigest) {
		t.Error("Has should return false for uncached digest")
	}
}

func TestCache_Has_shortDigest(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	if c.Has("ab") {
		t.Error("Has should return false for too-short digest")
	}
}

func TestCache_Open_success(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	content := "nupkg bytes"
	digest, _, err := c.Store("Pkg", "2.0.0", strings.NewReader(content))
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	rc, err := c.Open(digest)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer rc.Close()
	got, err := io.ReadAll(rc)
	if err != nil {
		t.Fatalf("ReadAll: %v", err)
	}
	if string(got) != content {
		t.Errorf("Open returned %q; want %q", got, content)
	}
}

func TestCache_Open_notFound(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	fakeDigest := strings.Repeat("b", 128)
	_, err := c.Open(fakeDigest)
	if err == nil {
		t.Fatal("Open of missing entry should return error")
	}
	if !errors.Is(err, os.ErrNotExist) {
		t.Errorf("expected os.ErrNotExist, got %v", err)
	}
}

func TestCache_Path(t *testing.T) {
	dir := t.TempDir()
	c := nuget.NewCache(dir)
	digest := strings.Repeat("c", 128)
	p := c.Path(digest)
	if !strings.HasPrefix(p, dir) {
		t.Errorf("Path %q should be under %q", p, dir)
	}
	// First two chars form the bucket prefix.
	if !strings.Contains(p, "cc") {
		t.Errorf("Path %q should contain bucket prefix 'cc'", p)
	}
}

func TestCache_Store_emptyID(t *testing.T) {
	c := nuget.NewCache(t.TempDir())
	_, _, err := c.Store("", "1.0.0", strings.NewReader("data"))
	if err == nil {
		t.Error("Store with empty id should return error")
	}
}

func TestCache_Store_emptyVersion(t *testing.T) {
	c := nuget.NewCache(t.TempDir())
	_, _, err := c.Store("Pkg", "", strings.NewReader("data"))
	if err == nil {
		t.Error("Store with empty version should return error")
	}
}

func TestCache_Store_differentContent_differentDigest(t *testing.T) {
	c := nuget.NewCache(t.TempDir())
	d1, _, _ := c.Store("Pkg", "1.0.0", strings.NewReader("content one"))
	d2, _, _ := c.Store("Pkg", "2.0.0", strings.NewReader("content two"))
	if d1 == d2 {
		t.Error("different content should produce different digests")
	}
}

func TestCache_Store_sameContent_sameDigest(t *testing.T) {
	c := nuget.NewCache(t.TempDir())
	d1, _, _ := c.Store("Pkg", "1.0.0", strings.NewReader("same content"))
	d2, _, _ := c.Store("Pkg", "1.0.1", strings.NewReader("same content"))
	if d1 != d2 {
		t.Error("identical content should produce identical digests")
	}
}

func TestCache_Open_shortDigest(t *testing.T) {
	c := nuget.NewCache(t.TempDir())
	_, err := c.Open("ab")
	if err == nil {
		t.Error("Open with too-short digest should return error")
	}
}
