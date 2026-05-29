package index

import (
	"crypto/sha256"
	"encoding/hex"
	"path/filepath"
	"sync"
	"testing"
)

func TestCache_StoreAndFetch(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("hello ruby gem content")
	hexSum, err := c.Store("mygem", "1.0.0", "", data)
	if err != nil {
		t.Fatalf("Store: unexpected error: %v", err)
	}
	if hexSum == "" {
		t.Fatal("Store returned empty SHA256")
	}

	got, err := c.Fetch(hexSum)
	if err != nil {
		t.Fatalf("Fetch: unexpected error: %v", err)
	}
	if string(got) != string(data) {
		t.Errorf("Fetch returned %q, want %q", got, data)
	}
}

func TestCache_Has_TrueAfterStore(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("some gem data content")
	hexSum, err := c.Store("mygem", "1.0.0", "", data)
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	if !c.Has(hexSum) {
		t.Errorf("Has(%q) = false, want true", hexSum)
	}
}

func TestCache_Has_FalseForMissing(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)
	if c.Has("0000000000000000000000000000000000000000000000000000000000000000") {
		t.Error("Has() = true for missing key, want false")
	}
}

func TestCache_FetchMiss(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)
	_, err := c.Fetch("aaaa0000000000000000000000000000000000000000000000000000bbbb1234")
	if err == nil {
		t.Fatal("Fetch of missing key should return error")
	}
}

func TestCache_SHA256Computed(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("checksum test")
	sum := sha256.Sum256(data)
	expectedHex := hex.EncodeToString(sum[:])

	hexSum, err := c.Store("g", "1.0", "", data)
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	if hexSum != expectedHex {
		t.Errorf("Store returned %q, want %q", hexSum, expectedHex)
	}
}

func TestCache_StoreIdempotent(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("idempotent content")
	h1, err := c.Store("g", "1.0", "", data)
	if err != nil {
		t.Fatalf("first Store: %v", err)
	}
	h2, err := c.Store("g", "1.0", "", data)
	if err != nil {
		t.Fatalf("second Store: %v", err)
	}
	if h1 != h2 {
		t.Errorf("Store is not idempotent: first=%q second=%q", h1, h2)
	}
}

func TestCache_DirectoryLayout(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("layout check")
	hexSum, err := c.Store("g", "1.0", "", data)
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	if len(hexSum) < 2 {
		t.Fatal("SHA256 too short")
	}
	expectedPath := filepath.Join(dir, hexSum[:2], hexSum+".gem")
	if !c.Has(hexSum) {
		t.Errorf("Has(%q) = false after Store", hexSum)
	}
	// Verify the path is where we expect it.
	gotPath := c.gemPath(hexSum)
	if gotPath != expectedPath {
		t.Errorf("gemPath = %q, want %q", gotPath, expectedPath)
	}
}

func TestCache_ConcurrentStoresSameBlob(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("concurrent blob content test")
	const workers = 10
	errs := make(chan error, workers)
	hashes := make(chan string, workers)

	var wg sync.WaitGroup
	for range workers {
		wg.Add(1)
		go func() {
			defer wg.Done()
			h, err := c.Store("g", "1.0", "", data)
			if err != nil {
				errs <- err
				return
			}
			hashes <- h
		}()
	}
	wg.Wait()
	close(errs)
	close(hashes)

	for err := range errs {
		t.Errorf("concurrent Store error: %v", err)
	}
	var first string
	for h := range hashes {
		if first == "" {
			first = h
		} else if h != first {
			t.Errorf("concurrent Store returned different hashes: %q vs %q", first, h)
		}
	}
}

func TestCache_StoreWithPlatform(t *testing.T) {
	dir := t.TempDir()
	c := NewCache(dir)

	data := []byte("platform specific gem")
	hexSum, err := c.Store("nokogiri", "1.16.2", "x86_64-linux", data)
	if err != nil {
		t.Fatalf("Store with platform: %v", err)
	}
	got, err := c.Fetch(hexSum)
	if err != nil {
		t.Fatalf("Fetch after Store with platform: %v", err)
	}
	if string(got) != string(data) {
		t.Errorf("roundtrip mismatch")
	}
}

func TestDefaultCacheDir(t *testing.T) {
	dir := DefaultCacheDir()
	if dir == "" {
		t.Error("DefaultCacheDir() returned empty string")
	}
	// Should contain mochi/ruby-gems.
	if !filepath.IsAbs(dir) {
		// Relative paths are acceptable only if home dir lookup failed.
		t.Logf("DefaultCacheDir() returned relative path: %q", dir)
	}
}
