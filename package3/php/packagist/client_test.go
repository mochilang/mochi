package packagist

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestNewClientDefaults(t *testing.T) {
	c := NewClient("")
	if c.BaseURL != DefaultBaseURL {
		t.Errorf("BaseURL = %q; want %q", c.BaseURL, DefaultBaseURL)
	}
	if c.HTTP == nil {
		t.Error("HTTP should be non-nil by default")
	}
	if c.UserAgent != DefaultUserAgent {
		t.Errorf("UserAgent = %q; want %q", c.UserAgent, DefaultUserAgent)
	}
}

func TestNewClientCustomBaseURL(t *testing.T) {
	c := NewClient("https://packagist.example.com")
	if c.BaseURL != "https://packagist.example.com/" {
		t.Errorf("BaseURL = %q; want trailing slash", c.BaseURL)
	}
}

func TestPackageURL(t *testing.T) {
	c := NewClient("")
	url, err := c.PackageURL("guzzlehttp", "guzzle")
	if err != nil {
		t.Fatalf("PackageURL: %v", err)
	}
	want := DefaultBaseURL + "p2/guzzlehttp/guzzle.json"
	if url != want {
		t.Errorf("PackageURL = %q; want %q", url, want)
	}
}

func TestPackageURLEmptyVendor(t *testing.T) {
	c := NewClient("")
	_, err := c.PackageURL("", "guzzle")
	if err == nil {
		t.Error("expected error for empty vendor")
	}
}

func TestPackageURLEmptyName(t *testing.T) {
	c := NewClient("")
	_, err := c.PackageURL("guzzlehttp", "")
	if err == nil {
		t.Error("expected error for empty name")
	}
}

func makePackagistServer(t *testing.T, vendor, name, body string, statusCode int) *httptest.Server {
	t.Helper()
	mux := http.NewServeMux()
	mux.HandleFunc("/p2/"+vendor+"/"+name+".json", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		w.WriteHeader(statusCode)
		w.Write([]byte(body)) //nolint:errcheck
	})
	return httptest.NewServer(mux)
}

func TestFetchPackageOK(t *testing.T) {
	body := `{
		"packages": {
			"symfony/console": [
				{
					"name": "symfony/console",
					"version": "6.4.0",
					"version_normalized": "6.4.0.0",
					"dist": {
						"type": "zip",
						"url": "https://api.github.com/repos/symfony/console/zipball/abc123",
						"shasum": "aabbcc"
					},
					"require": {
						"php": ">=8.1",
						"symfony/polyfill-mbstring": "~1.0"
					},
					"autoload": {
						"psr-4": {"Symfony\\Component\\Console\\": ""}
					},
					"description": "Symfony Console Component"
				}
			]
		}
	}`
	srv := makePackagistServer(t, "symfony", "console", body, http.StatusOK)
	defer srv.Close()

	c := NewClient(srv.URL)
	pr, err := c.FetchPackage(context.Background(), "symfony", "console")
	if err != nil {
		t.Fatalf("FetchPackage: %v", err)
	}
	versions := pr.Versions("symfony", "console")
	if len(versions) != 1 {
		t.Fatalf("expected 1 version; got %d", len(versions))
	}
	v := versions[0]
	if v.Version != "6.4.0" {
		t.Errorf("Version = %q; want 6.4.0", v.Version)
	}
	if v.Dist.URL == "" {
		t.Error("Dist.URL should not be empty")
	}
	if v.Dist.Type != "zip" {
		t.Errorf("Dist.Type = %q; want zip", v.Dist.Type)
	}
}

func TestFetchPackage404(t *testing.T) {
	srv := makePackagistServer(t, "vendor", "nonexistent", `{"status":"error","message":"Package not found"}`, http.StatusNotFound)
	defer srv.Close()

	c := NewClient(srv.URL)
	_, err := c.FetchPackage(context.Background(), "vendor", "nonexistent")
	if err == nil {
		t.Fatal("expected error for 404")
	}
	if !errors.Is(err, ErrPackageNotFound) {
		t.Errorf("error %v is not ErrPackageNotFound", err)
	}
}

func TestFetchPackage500(t *testing.T) {
	srv := makePackagistServer(t, "vendor", "pkg", `internal error`, http.StatusInternalServerError)
	defer srv.Close()

	c := NewClient(srv.URL)
	_, err := c.FetchPackage(context.Background(), "vendor", "pkg")
	if err == nil {
		t.Fatal("expected error for 500")
	}
	if !strings.Contains(err.Error(), "500") {
		t.Errorf("error %v should mention status 500", err)
	}
}

func TestFetchPackageBadJSON(t *testing.T) {
	srv := makePackagistServer(t, "vendor", "pkg", `not valid json`, http.StatusOK)
	defer srv.Close()

	c := NewClient(srv.URL)
	_, err := c.FetchPackage(context.Background(), "vendor", "pkg")
	if err == nil {
		t.Fatal("expected error for bad JSON")
	}
}

func TestPackageResponseVersions(t *testing.T) {
	pr := &PackageResponse{
		Packages: map[string][]PackageVersion{
			"guzzlehttp/guzzle": {{Name: "guzzlehttp/guzzle", Version: "7.8.0"}},
		},
	}
	vv := pr.Versions("guzzlehttp", "guzzle")
	if len(vv) != 1 {
		t.Fatalf("expected 1 version; got %d", len(vv))
	}
	if pr.Versions("missing", "pkg") != nil {
		t.Error("Versions of missing key should be nil")
	}
	var nilPR *PackageResponse
	if nilPR.Versions("x", "y") != nil {
		t.Error("nil PackageResponse.Versions should be nil")
	}
}

func TestUserAgentSent(t *testing.T) {
	var gotUA string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotUA = r.Header.Get("User-Agent")
		resp := map[string]interface{}{
			"packages": map[string]interface{}{
				"a/b": []PackageVersion{{Name: "a/b", Version: "1.0.0"}},
			},
		}
		json.NewEncoder(w).Encode(resp) //nolint:errcheck
	}))
	defer srv.Close()

	c := NewClient(srv.URL)
	c.FetchPackage(context.Background(), "a", "b") //nolint:errcheck
	if gotUA != DefaultUserAgent {
		t.Errorf("User-Agent = %q; want %q", gotUA, DefaultUserAgent)
	}
}
