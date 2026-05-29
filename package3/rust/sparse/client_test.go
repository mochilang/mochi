package sparse

import (
	"context"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func newTestServer(t *testing.T, handler http.HandlerFunc) *httptest.Server {
	t.Helper()
	s := httptest.NewServer(handler)
	t.Cleanup(s.Close)
	return s
}

func TestClientIndexURL(t *testing.T) {
	c := NewClient("https://index.example.com/")
	got, err := c.IndexURL("serde")
	if err != nil {
		t.Fatalf("IndexURL: %v", err)
	}
	if got != "https://index.example.com/se/rd/serde" {
		t.Errorf("IndexURL(serde) = %q", got)
	}
}

func TestClientIndexURLTrailingSlash(t *testing.T) {
	c := NewClient("https://index.example.com") // no trailing slash
	got, err := c.IndexURL("ab")
	if err != nil {
		t.Fatalf("IndexURL: %v", err)
	}
	if got != "https://index.example.com/2/ab" {
		t.Errorf("IndexURL(ab) = %q", got)
	}
}

func TestClientFetchIndex(t *testing.T) {
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/se/rd/serde" {
			t.Errorf("unexpected path %q", r.URL.Path)
		}
		if r.Header.Get("User-Agent") == "" {
			t.Errorf("missing User-Agent")
		}
		io.WriteString(w, serdeFixture)
	})
	c := NewClient(server.URL + "/")
	entries, err := c.FetchIndex(context.Background(), "serde")
	if err != nil {
		t.Fatalf("FetchIndex: %v", err)
	}
	if len(entries) != 4 {
		t.Errorf("len(entries) = %d; want 4", len(entries))
	}
}

func TestClientFetchIndex404(t *testing.T) {
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	c := NewClient(server.URL + "/")
	_, err := c.FetchIndex(context.Background(), "no-such-crate")
	if !errors.Is(err, ErrCrateNotFound) {
		t.Errorf("FetchIndex err = %v; want ErrCrateNotFound", err)
	}
}

func TestClientFetchIndex500(t *testing.T) {
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "boom", http.StatusInternalServerError)
	})
	c := NewClient(server.URL + "/")
	if _, err := c.FetchIndex(context.Background(), "serde"); err == nil {
		t.Errorf("FetchIndex err = nil; want non-nil")
	}
}

func TestDownloadURLFor(t *testing.T) {
	c := NewClient("")
	c.DownloadURL = "https://example.com/{crate}/{crate}-{version}.crate"
	got, err := c.DownloadURLFor("serde", "1.0.0")
	if err != nil {
		t.Fatalf("DownloadURLFor: %v", err)
	}
	if got != "https://example.com/serde/serde-1.0.0.crate" {
		t.Errorf("DownloadURLFor = %q", got)
	}
}

func TestDownloadURLForBadName(t *testing.T) {
	c := NewClient("")
	if _, err := c.DownloadURLFor("bad name", "1.0.0"); err == nil {
		t.Errorf("DownloadURLFor(bad name) err = nil")
	}
	if _, err := c.DownloadURLFor("serde", ""); err == nil {
		t.Errorf("DownloadURLFor(empty version) err = nil")
	}
}

func TestClientFetchCrate(t *testing.T) {
	body := "fake crate bytes"
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		if !strings.Contains(r.URL.Path, "serde-1.0.0.crate") {
			t.Errorf("unexpected path %q", r.URL.Path)
		}
		io.WriteString(w, body)
	})
	c := NewClient("")
	c.DownloadURL = server.URL + "/{crate}-{version}.crate"
	var sink strings.Builder
	n, err := c.FetchCrate(context.Background(), "serde", "1.0.0", &sink)
	if err != nil {
		t.Fatalf("FetchCrate: %v", err)
	}
	if int(n) != len(body) || sink.String() != body {
		t.Errorf("FetchCrate body mismatch: n=%d, got=%q", n, sink.String())
	}
}

func TestClientFetchCrateNotFound(t *testing.T) {
	server := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	})
	c := NewClient("")
	c.DownloadURL = server.URL + "/{crate}-{version}.crate"
	var sink strings.Builder
	_, err := c.FetchCrate(context.Background(), "serde", "1.0.0", &sink)
	if !errors.Is(err, ErrCrateVersionNotFound) {
		t.Errorf("FetchCrate err = %v; want ErrCrateVersionNotFound", err)
	}
}
