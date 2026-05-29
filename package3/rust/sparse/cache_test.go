package sparse

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestCacheIndexPath(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	got, err := c.IndexPath("serde")
	if err != nil {
		t.Fatalf("IndexPath: %v", err)
	}
	want := filepath.Join(dir, "registry", "index", "index.crates.io", "se", "rd", "serde")
	if got != want {
		t.Errorf("IndexPath = %q; want %q", got, want)
	}
}

func TestCacheCratePath(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "custom-host")
	got, err := c.CratePath("serde", "1.0.0")
	if err != nil {
		t.Fatalf("CratePath: %v", err)
	}
	want := filepath.Join(dir, "registry", "cache", "custom-host", "serde-1.0.0.crate")
	if got != want {
		t.Errorf("CratePath = %q; want %q", got, want)
	}
}

func TestCacheBlake3Path(t *testing.T) {
	c := NewCache("/cache", "")
	got, err := c.Blake3Path("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
	if err != nil {
		t.Fatalf("Blake3Path: %v", err)
	}
	want := filepath.Join("/cache", "registry", "blake3", "01", "23456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef.crate")
	if got != want {
		t.Errorf("Blake3Path = %q; want %q", got, want)
	}
}

func TestCacheBlake3PathBadDigest(t *testing.T) {
	c := NewCache("/cache", "")
	if _, err := c.Blake3Path("zz"); err == nil {
		t.Errorf("Blake3Path(zz) err = nil")
	}
	if _, err := c.Blake3Path("XYZ"); err == nil {
		t.Errorf("Blake3Path(XYZ) err = nil")
	}
}

func TestCacheStoreIndex(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	body := []byte(serdeFixture)
	path, err := c.StoreIndex("serde", body)
	if err != nil {
		t.Fatalf("StoreIndex: %v", err)
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("StoreIndex round-trip mismatch")
	}
}

func TestCacheLoadIndexMissing(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	_, err := c.LoadIndex("serde")
	if !errors.Is(err, os.ErrNotExist) {
		t.Errorf("LoadIndex(missing) = %v; want os.ErrNotExist", err)
	}
}

func TestCacheStoreCrate(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	body := []byte("fake .crate bytes for testing")
	sha := sha256.Sum256(body)
	expected := hex.EncodeToString(sha[:])
	path, b3, err := c.StoreCrate("serde", "1.0.0", expected, strings.NewReader(string(body)))
	if err != nil {
		t.Fatalf("StoreCrate: %v", err)
	}
	if !c.HasCrate("serde", "1.0.0") {
		t.Errorf("HasCrate = false after StoreCrate")
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile: %v", err)
	}
	if string(got) != string(body) {
		t.Errorf("StoreCrate body mismatch")
	}
	if len(b3) != 64 {
		t.Errorf("blake3 hex = %q; want 64 chars", b3)
	}
	if _, err := hex.DecodeString(b3); err != nil {
		t.Errorf("blake3 hex not valid: %v", err)
	}
	// BLAKE3 alias should exist.
	alias, err := c.Blake3Path(b3)
	if err != nil {
		t.Fatalf("Blake3Path: %v", err)
	}
	if _, err := os.Stat(alias); err != nil {
		t.Errorf("blake3 alias missing: %v", err)
	}
}

func TestCacheStoreCrateChecksumMismatch(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	body := []byte("fake bytes")
	wrong := strings.Repeat("0", 64)
	_, _, err := c.StoreCrate("serde", "1.0.0", wrong, strings.NewReader(string(body)))
	if !errors.Is(err, ErrChecksumMismatch) {
		t.Errorf("StoreCrate err = %v; want ErrChecksumMismatch", err)
	}
	if c.HasCrate("serde", "1.0.0") {
		t.Errorf("HasCrate = true after mismatch; partial file should be removed")
	}
}

func TestCacheStoreCrateNoChecksum(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	_, _, err := c.StoreCrate("serde", "1.0.0", "", strings.NewReader("any bytes"))
	if err != nil {
		t.Errorf("StoreCrate(no checksum) err = %v", err)
	}
	if !c.HasCrate("serde", "1.0.0") {
		t.Errorf("HasCrate = false")
	}
}

func TestCacheFetchIndex(t *testing.T) {
	dir := t.TempDir()
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, serdeFixture)
	})
	client := NewClient(server.URL + "/")
	cache := NewCache(dir, "")
	entries, err := cache.FetchIndex(context.Background(), client, "serde")
	if err != nil {
		t.Fatalf("Cache.FetchIndex: %v", err)
	}
	if len(entries) != 4 {
		t.Errorf("len(entries) = %d; want 4", len(entries))
	}
	// Cached copy must be on disk.
	loaded, err := cache.LoadIndex("serde")
	if err != nil {
		t.Fatalf("LoadIndex: %v", err)
	}
	if !strings.Contains(string(loaded), `"vers":"1.0.0"`) {
		t.Errorf("cached index missing expected content")
	}
}

func TestCacheRoundTripIndex(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	if _, err := c.StoreIndex("serde", []byte(serdeFixture)); err != nil {
		t.Fatalf("StoreIndex: %v", err)
	}
	loaded, err := c.LoadIndex("serde")
	if err != nil {
		t.Fatalf("LoadIndex: %v", err)
	}
	entries, err := ParseIndex(strings.NewReader(string(loaded)))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	if len(entries) != 4 {
		t.Errorf("len(entries) = %d; want 4", len(entries))
	}
}

func TestStoreIndexOverwrites(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir, "")
	if _, err := c.StoreIndex("serde", []byte("old")); err != nil {
		t.Fatalf("StoreIndex old: %v", err)
	}
	if _, err := c.StoreIndex("serde", []byte("new")); err != nil {
		t.Fatalf("StoreIndex new: %v", err)
	}
	got, err := c.LoadIndex("serde")
	if err != nil {
		t.Fatalf("LoadIndex: %v", err)
	}
	if string(got) != "new" {
		t.Errorf("StoreIndex did not overwrite: got %q", got)
	}
}
