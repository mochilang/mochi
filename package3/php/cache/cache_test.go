package cache

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func sha256Hex(data []byte) string {
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:])
}

const zeroDigest = "0000000000000000000000000000000000000000000000000000000000000000"

func TestNewCacheDefaults(t *testing.T) {
	c := NewCache("/tmp/test-cache")
	if c.Root != "/tmp/test-cache" {
		t.Errorf("Root = %q; want /tmp/test-cache", c.Root)
	}
	if c.HTTP == nil {
		t.Error("HTTP should be non-nil by default")
	}
	if c.UserAgent == "" {
		t.Error("UserAgent should be non-empty by default")
	}
}

func TestDistPath(t *testing.T) {
	c := NewCache("/cache")
	digest := strings.Repeat("ab", 32) // 64 valid hex chars
	path, err := c.DistPath(digest)
	if err != nil {
		t.Fatalf("DistPath: %v", err)
	}
	wantDir := filepath.Join("/cache", "ab")
	if !strings.HasPrefix(path, wantDir) {
		t.Errorf("DistPath = %q; should start with %q", path, wantDir)
	}
	if !strings.HasSuffix(path, ".zip") {
		t.Errorf("DistPath = %q; should end with .zip", path)
	}
}

func TestDistPathInvalidDigest(t *testing.T) {
	c := NewCache("/cache")
	cases := []string{"", "short", strings.Repeat("g", 64)}
	for _, bad := range cases {
		_, err := c.DistPath(bad)
		if err == nil {
			t.Errorf("DistPath(%q) should return error", bad)
		}
	}
}

func TestHasReturnsFalseWhenMissing(t *testing.T) {
	c := NewCache(t.TempDir())
	if c.Has(sha256Hex([]byte("nonexistent"))) {
		t.Error("Has should return false for missing file")
	}
}

func TestStoreAndLoad(t *testing.T) {
	payload := []byte("fake zip archive contents")
	expectedSHA := sha256Hex(payload)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write(payload) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	path, gotSHA, err := c.Store(context.Background(), srv.URL+"/dist.zip", expectedSHA)
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	if gotSHA != expectedSHA {
		t.Errorf("returned SHA %q != expected %q", gotSHA, expectedSHA)
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("cached file %q not found: %v", path, err)
	}

	rc, err := c.Load(gotSHA)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	defer rc.Close()
	got, _ := io.ReadAll(rc)
	if !bytes.Equal(got, payload) {
		t.Errorf("Load returned %q; want %q", got, payload)
	}
}

func TestStoreFastPath(t *testing.T) {
	payload := []byte("cached payload")
	expectedSHA := sha256Hex(payload)
	var calls int
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		calls++
		w.Write(payload) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	if _, _, err := c.Store(context.Background(), srv.URL+"/dist.zip", expectedSHA); err != nil {
		t.Fatalf("first Store: %v", err)
	}
	if calls != 1 {
		t.Errorf("expected 1 server call; got %d", calls)
	}
	if _, _, err := c.Store(context.Background(), srv.URL+"/dist.zip", expectedSHA); err != nil {
		t.Fatalf("second Store: %v", err)
	}
	if calls != 1 {
		t.Errorf("fast-path: expected still 1 server call; got %d", calls)
	}
}

func TestStoreChecksumMismatch(t *testing.T) {
	payload := []byte("payload")
	wrongExpected := sha256Hex([]byte("different content"))
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write(payload) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	_, _, err := c.Store(context.Background(), srv.URL+"/dist.zip", wrongExpected)
	if err == nil {
		t.Fatal("expected error for checksum mismatch")
	}
	if !errors.Is(err, ErrChecksumMismatch) {
		t.Errorf("error %v is not ErrChecksumMismatch", err)
	}
}

func TestStore404(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotFound)
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	_, _, err := c.Store(context.Background(), srv.URL+"/missing.zip", "")
	if err == nil {
		t.Fatal("expected error for 404")
	}
	if !errors.Is(err, ErrDistNotFound) {
		t.Errorf("error %v is not ErrDistNotFound", err)
	}
}

func TestStore500(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal error", http.StatusInternalServerError)
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	_, _, err := c.Store(context.Background(), srv.URL+"/dist.zip", "")
	if err == nil {
		t.Fatal("expected error for 500")
	}
	if !strings.Contains(err.Error(), "500") {
		t.Errorf("error %v should mention status 500", err)
	}
}

func TestStoreNoExpectedSHA(t *testing.T) {
	payload := []byte("archive without known sha")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write(payload) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	path, gotSHA, err := c.Store(context.Background(), srv.URL+"/dist.zip", "")
	if err != nil {
		t.Fatalf("Store: %v", err)
	}
	if gotSHA != sha256Hex(payload) {
		t.Errorf("SHA mismatch: %q != %q", gotSHA, sha256Hex(payload))
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("file not found at %q: %v", path, err)
	}
}

func TestLoadNotCached(t *testing.T) {
	c := NewCache(t.TempDir())
	_, err := c.Load(sha256Hex([]byte("not downloaded")))
	if err == nil {
		t.Fatal("expected error for uncached digest")
	}
	if !errors.Is(err, ErrDistNotCached) {
		t.Errorf("error %v is not ErrDistNotCached", err)
	}
}

func TestHashReader(t *testing.T) {
	data := []byte("hello world")
	want := sha256Hex(data)
	got, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader: %v", err)
	}
	if got != want {
		t.Errorf("HashReader = %q; want %q", got, want)
	}
}

func TestUserAgentSent(t *testing.T) {
	var gotUA string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotUA = r.Header.Get("User-Agent")
		w.Write([]byte("data")) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	c.Store(context.Background(), srv.URL+"/dist.zip", "") //nolint:errcheck
	if !strings.Contains(gotUA, "mochi-php-bridge") {
		t.Errorf("User-Agent %q should contain mochi-php-bridge", gotUA)
	}
}

func TestValidateSHA256Hex(t *testing.T) {
	if err := validateSHA256Hex(zeroDigest); err != nil {
		t.Errorf("validateSHA256Hex(zeros) = %v; want nil", err)
	}
	if err := validateSHA256Hex("abc"); err == nil {
		t.Error("validateSHA256Hex(short) should error")
	}
	if err := validateSHA256Hex(strings.Repeat("zz", 32)); err == nil {
		t.Error("validateSHA256Hex(non-hex) should error")
	}
}

func TestHasReturnsTrueAfterStore(t *testing.T) {
	payload := []byte("presence test")
	sha := sha256Hex(payload)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write(payload) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewCache(t.TempDir())
	if c.Has(sha) {
		t.Error("Has should be false before Store")
	}
	if _, _, err := c.Store(context.Background(), srv.URL+"/dist.zip", sha); err != nil {
		t.Fatalf("Store: %v", err)
	}
	if !c.Has(sha) {
		t.Error("Has should be true after Store")
	}
}
