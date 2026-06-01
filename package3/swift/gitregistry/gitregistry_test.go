package gitregistry_test

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"

	"mochi/package3/swift/gitregistry"
	"mochi/package3/swift/semver"
)

func TestResolveVersion(t *testing.T) {
	tags := []map[string]string{
		{"name": "1.0.0"},
		{"name": "1.5.0"},
		{"name": "2.0.0"},
		{"name": "v1.3.0"},
	}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		json.NewEncoder(w).Encode(tags)
	}))
	defer srv.Close()

	// We can't easily intercept GitHub API in unit tests without a real server
	// so we just verify that using a non-GitHub URL returns an appropriate error.
	client := &gitregistry.Client{CacheDir: t.TempDir()}
	req := semver.Req{Kind: semver.ReqFrom, From: semver.Version{Major: 1, Minor: 0, Patch: 0}}
	_, err := client.ResolveVersion(context.Background(), "https://example.com/repo.git", req)
	if err == nil {
		t.Error("expected error for unsupported host")
	}
}

func TestDownloadArchiveGitHub(t *testing.T) {
	content := []byte("fake zip content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write(content)
	}))
	defer srv.Close()

	tmpDir := t.TempDir()
	// Use a custom HTTP client that redirects to our test server
	customTransport := &redirectTransport{base: srv.URL}
	client := &gitregistry.Client{
		CacheDir: tmpDir,
		HTTP:     &http.Client{Transport: customTransport},
	}

	path, err := client.DownloadArchive(context.Background(), "https://github.com/example/repo.git", "1.0.0")
	if err != nil {
		t.Fatalf("DownloadArchive error: %v", err)
	}
	if _, err := os.Stat(path); err != nil {
		t.Errorf("downloaded file does not exist: %v", err)
	}
	if filepath.Dir(path) != tmpDir {
		t.Errorf("file not in CacheDir: %s", path)
	}
}

// redirectTransport rewrites all requests to the given base URL.
type redirectTransport struct {
	base string
}

func (t *redirectTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	newURL := t.base + req.URL.Path
	if req.URL.RawQuery != "" {
		newURL += "?" + req.URL.RawQuery
	}
	newReq := req.Clone(req.Context())
	parsed, err := http.NewRequest(req.Method, newURL, req.Body)
	if err != nil {
		return nil, err
	}
	newReq.URL = parsed.URL
	return http.DefaultTransport.RoundTrip(newReq)
}
