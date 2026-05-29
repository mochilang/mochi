package sumdb

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestClientLatest(t *testing.T) {
	body := "go.sum database tree\n5\nAAAA=\n\n— sum.golang.org AAAAAA==\n"
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if !strings.HasSuffix(r.URL.Path, "/latest") {
			http.Error(w, "wrong path "+r.URL.Path, http.StatusNotFound)
			return
		}
		io.WriteString(w, body)
	}))
	t.Cleanup(srv.Close)

	c := NewClient(srv.URL)
	got, err := c.Latest(context.Background())
	if err != nil {
		t.Fatalf("Latest: %v", err)
	}
	if string(got) != body {
		t.Errorf("Latest mismatch:\n  got %q\n  want %q", got, body)
	}
}

func TestClientLookup(t *testing.T) {
	body := `42
example.com/foo v1.0.0 h1:zip==
example.com/foo v1.0.0/go.mod h1:mod==

go.sum database tree
50
AAAA=

— sum.golang.org AAAA==
`
	var gotURL string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotURL = r.URL.Path
		io.WriteString(w, body)
	}))
	t.Cleanup(srv.Close)
	c := NewClient(srv.URL)
	rec, err := c.Lookup(context.Background(), "example.com/foo", "v1.0.0")
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if rec.ID != 42 || rec.Module != "example.com/foo" || rec.Version != "v1.0.0" {
		t.Errorf("Lookup record wrong: %+v", rec)
	}
	if !strings.Contains(gotURL, "lookup/") {
		t.Errorf("client did not hit lookup endpoint: %q", gotURL)
	}
	if !strings.Contains(gotURL, "example.com") {
		t.Errorf("client URL %q missing module path", gotURL)
	}
}

func TestClientLookupEmptyArgs(t *testing.T) {
	c := NewClient("https://example.invalid/")
	if _, err := c.Lookup(context.Background(), "", "v1"); err == nil {
		t.Errorf("Lookup with empty module: want error")
	}
	if _, err := c.Lookup(context.Background(), "x", ""); err == nil {
		t.Errorf("Lookup with empty version: want error")
	}
}

func TestClientTile(t *testing.T) {
	want := []byte("tile bytes")
	var gotURL string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotURL = r.URL.Path
		w.Write(want)
	}))
	t.Cleanup(srv.Close)
	c := NewClient(srv.URL)
	got, err := c.Tile(context.Background(), 8, 0, 3)
	if err != nil {
		t.Fatalf("Tile: %v", err)
	}
	if string(got) != string(want) {
		t.Errorf("Tile body mismatch")
	}
	if !strings.HasSuffix(gotURL, "/tile/8/0/3") {
		t.Errorf("Tile URL %q does not end in /tile/8/0/3", gotURL)
	}
}

func TestClientTileBadCoords(t *testing.T) {
	c := NewClient("https://example.invalid/")
	if _, err := c.Tile(context.Background(), 0, 0, 0); err == nil {
		t.Errorf("Tile with H=0: want error")
	}
	if _, err := c.Tile(context.Background(), 8, -1, 0); err == nil {
		t.Errorf("Tile with negative level: want error")
	}
	if _, err := c.Tile(context.Background(), 8, 0, -1); err == nil {
		t.Errorf("Tile with negative index: want error")
	}
}

func TestClientServerError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal", http.StatusInternalServerError)
	}))
	t.Cleanup(srv.Close)
	c := NewClient(srv.URL)
	if _, err := c.Latest(context.Background()); err == nil {
		t.Errorf("Latest on 500 returned nil; want error")
	}
}

func TestNewClientDefaultBaseURL(t *testing.T) {
	c := NewClient("")
	if c.BaseURL != DefaultBaseURL {
		t.Errorf("BaseURL = %q; want %q", c.BaseURL, DefaultBaseURL)
	}
}

func TestNewClientTrailingSlash(t *testing.T) {
	c := NewClient("https://example.invalid")
	if !strings.HasSuffix(c.BaseURL, "/") {
		t.Errorf("BaseURL not normalised: %q", c.BaseURL)
	}
}
