package se0292_test

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"

	"mochi/package3/swift/se0292"
)

func TestListVersions(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/vnd.swift.registry.v1+json")
		w.WriteHeader(http.StatusOK)
		io.WriteString(w, `{"releases":{"1.0.0":{},"1.1.0":{},"2.0.0":{}}}`)
	}))
	defer srv.Close()

	reg := &se0292.Registry{BaseURL: srv.URL, HTTP: srv.Client()}
	versions, err := reg.ListVersions(context.Background(), "myscope.mypackage")
	if err != nil {
		t.Fatalf("ListVersions error: %v", err)
	}
	if len(versions) != 3 {
		t.Errorf("got %d versions, want 3: %v", len(versions), versions)
	}
}

func TestListVersionsInvalidID(t *testing.T) {
	reg := &se0292.Registry{BaseURL: "https://example.com"}
	_, err := reg.ListVersions(context.Background(), "invalid-no-dot")
	if err == nil {
		t.Error("expected error for invalid id")
	}
}

func TestDownloadSourceArchive(t *testing.T) {
	content := []byte("zip content")
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/zip")
		w.WriteHeader(http.StatusOK)
		w.Write(content)
	}))
	defer srv.Close()

	reg := &se0292.Registry{BaseURL: srv.URL, HTTP: srv.Client()}
	rc, err := reg.DownloadSourceArchive(context.Background(), "scope.pkg", "1.0.0")
	if err != nil {
		t.Fatalf("DownloadSourceArchive error: %v", err)
	}
	defer rc.Close()

	data, err := io.ReadAll(rc)
	if err != nil {
		t.Fatalf("read error: %v", err)
	}
	if string(data) != string(content) {
		t.Errorf("content mismatch: got %q", data)
	}
}

func TestGetManifest(t *testing.T) {
	manifest := `// swift-tools-version: 5.9
import PackageDescription
let package = Package(name: "MyPkg", products: [], targets: [])
`
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/x-swift")
		w.WriteHeader(http.StatusOK)
		io.WriteString(w, manifest)
	}))
	defer srv.Close()

	reg := &se0292.Registry{BaseURL: srv.URL, HTTP: srv.Client()}
	data, err := reg.GetManifest(context.Background(), "scope.pkg", "1.0.0")
	if err != nil {
		t.Fatalf("GetManifest error: %v", err)
	}
	if string(data) != manifest {
		t.Errorf("manifest mismatch: got %q", data)
	}
}

func TestGetManifestNotFound(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusNotFound)
	}))
	defer srv.Close()

	reg := &se0292.Registry{BaseURL: srv.URL, HTTP: srv.Client()}
	_, err := reg.GetManifest(context.Background(), "scope.pkg", "9.9.9")
	if err == nil {
		t.Error("expected error for 404 response")
	}
}
