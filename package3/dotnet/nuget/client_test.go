package nuget_test

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"mochi/package3/dotnet/nuget"
	"mochi/package3/dotnet/semver"
)

func newTestServer(t *testing.T, handler http.HandlerFunc) (*httptest.Server, *nuget.Client) {
	t.Helper()
	srv := httptest.NewServer(handler)
	t.Cleanup(srv.Close)
	c := nuget.NewClient("")
	c.FlatContainerBaseURL = srv.URL + "/"
	c.DownloadURLTemplate = srv.URL + "/{id}/{version}/{id}.{version}.nupkg"
	c.HTTP = srv.Client()
	return srv, c
}

func TestNewClient_defaults(t *testing.T) {
	c := nuget.NewClient("")
	if c.ServiceIndexURL != nuget.DefaultServiceIndexURL {
		t.Errorf("ServiceIndexURL = %q; want %q", c.ServiceIndexURL, nuget.DefaultServiceIndexURL)
	}
	if c.UserAgent != nuget.DefaultUserAgent {
		t.Errorf("UserAgent = %q; want %q", c.UserAgent, nuget.DefaultUserAgent)
	}
	if c.HTTP == nil {
		t.Error("HTTP client must not be nil")
	}
}

func TestNewClient_customURL(t *testing.T) {
	c := nuget.NewClient("https://custom.example.com/v3/index.json")
	if c.ServiceIndexURL != "https://custom.example.com/v3/index.json" {
		t.Errorf("ServiceIndexURL = %q", c.ServiceIndexURL)
	}
}

func TestFetchVersions_success(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		if !strings.HasSuffix(r.URL.Path, "/index.json") {
			http.NotFound(w, r)
			return
		}
		json.NewEncoder(w).Encode(nuget.VersionsIndex{
			Versions: []string{"1.0.0", "1.1.0", "2.0.0"},
		})
	})
	versions, err := c.FetchVersions(context.Background(), "Newtonsoft.Json")
	if err != nil {
		t.Fatalf("FetchVersions error: %v", err)
	}
	if len(versions) != 3 {
		t.Errorf("got %d versions; want 3", len(versions))
	}
}

func TestFetchVersions_notFound(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	_, err := c.FetchVersions(context.Background(), "NoSuchPackage")
	if !errors.Is(err, nuget.ErrPackageNotFound) {
		t.Errorf("expected ErrPackageNotFound, got %v", err)
	}
}

func TestFetchVersions_emptyID(t *testing.T) {
	c := nuget.NewClient("")
	_, err := c.FetchVersions(context.Background(), "")
	if err == nil {
		t.Error("empty id should return error")
	}
}

func TestFetchVersions_serverError(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal error", http.StatusInternalServerError)
	})
	_, err := c.FetchVersions(context.Background(), "SomePackage")
	if err == nil {
		t.Error("expected error on 500 response")
	}
}

func TestLatestVersion_success(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		json.NewEncoder(w).Encode(nuget.VersionsIndex{
			Versions: []string{"1.0.0", "1.5.0", "2.0.0", "3.0.0"},
		})
	})
	req := semver.MustParseReq("[1.0.0, 2.0.0)")
	got, err := c.LatestVersion(context.Background(), "Pkg", req)
	if err != nil {
		t.Fatalf("LatestVersion error: %v", err)
	}
	if got != "1.5.0" {
		t.Errorf("LatestVersion = %q; want %q", got, "1.5.0")
	}
}

func TestLatestVersion_noneMatch(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		json.NewEncoder(w).Encode(nuget.VersionsIndex{
			Versions: []string{"3.0.0"},
		})
	})
	req := semver.MustParseReq("[1.0.0, 2.0.0)")
	_, err := c.LatestVersion(context.Background(), "Pkg", req)
	if !errors.Is(err, nuget.ErrVersionNotFound) {
		t.Errorf("expected ErrVersionNotFound, got %v", err)
	}
}

func TestDownloadURLFor_substitution(t *testing.T) {
	c := nuget.NewClient("")
	url, err := c.DownloadURLFor("Newtonsoft.Json", "13.0.1")
	if err != nil {
		t.Fatalf("DownloadURLFor error: %v", err)
	}
	if !strings.Contains(url, "newtonsoft.json") {
		t.Errorf("URL %q should contain lowercased id", url)
	}
	if !strings.Contains(url, "13.0.1") {
		t.Errorf("URL %q should contain version", url)
	}
}

func TestDownloadURLFor_emptyID(t *testing.T) {
	c := nuget.NewClient("")
	_, err := c.DownloadURLFor("", "1.0.0")
	if err == nil {
		t.Error("empty id should return error")
	}
}

func TestDownloadURLFor_emptyVersion(t *testing.T) {
	c := nuget.NewClient("")
	_, err := c.DownloadURLFor("Pkg", "")
	if err == nil {
		t.Error("empty version should return error")
	}
}

func TestFetchPackage_success(t *testing.T) {
	body := []byte("fake nupkg content")
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write(body)
	})
	var buf strings.Builder
	n, err := c.FetchPackage(context.Background(), "Pkg", "1.0.0", &buf)
	if err != nil {
		t.Fatalf("FetchPackage error: %v", err)
	}
	if n != int64(len(body)) {
		t.Errorf("FetchPackage wrote %d bytes; want %d", n, len(body))
	}
}

func TestFetchPackage_notFound(t *testing.T) {
	_, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	_, err := c.FetchPackage(context.Background(), "Pkg", "1.0.0", io.Discard)
	if !errors.Is(err, nuget.ErrVersionNotFound) {
		t.Errorf("expected ErrVersionNotFound, got %v", err)
	}
}

func TestErrPackageNotFound_sentinel(t *testing.T) {
	err := nuget.ErrPackageNotFound
	if err == nil {
		t.Fatal("ErrPackageNotFound must not be nil")
	}
	if err.Error() == "" {
		t.Error("ErrPackageNotFound.Error() must not be empty")
	}
}

func TestErrVersionNotFound_sentinel(t *testing.T) {
	err := nuget.ErrVersionNotFound
	if err == nil {
		t.Fatal("ErrVersionNotFound must not be nil")
	}
}
