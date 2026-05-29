package simple

import (
	"context"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestHTTPClientFetchProjectHTML(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/simple/httpx/" {
			t.Errorf("unexpected path %q", r.URL.Path)
		}
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(minimalHTML))
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	p, err := c.FetchProject(context.Background(), "HTTPX", FormatHTML)
	if err != nil {
		t.Fatalf("FetchProject err = %v", err)
	}
	if p.Name != "httpx" {
		t.Errorf("Name = %q", p.Name)
	}
	if len(p.Files) != 2 {
		t.Errorf("len(Files) = %d; want 2", len(p.Files))
	}
}

func TestHTTPClientFetchProjectJSON(t *testing.T) {
	var gotAccept string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotAccept = r.Header.Get("Accept")
		w.Header().Set("Content-Type", "application/vnd.pypi.simple.v1+json")
		w.Write([]byte(minimalJSON))
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	p, err := c.FetchProject(context.Background(), "httpx", FormatJSON)
	if err != nil {
		t.Fatalf("FetchProject err = %v", err)
	}
	if !strings.Contains(gotAccept, "application/vnd.pypi.simple.v1+json") {
		t.Errorf("Accept header = %q; want PEP 691 JSON content type", gotAccept)
	}
	if p.Meta.APIVersion != "1.1" {
		t.Errorf("Meta.APIVersion = %q", p.Meta.APIVersion)
	}
}

func TestHTTPClientFetchProjectNegotiatesDownToHTML(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Write([]byte(minimalHTML))
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	p, err := c.FetchProject(context.Background(), "httpx", FormatJSON)
	if err != nil {
		t.Fatalf("FetchProject err = %v", err)
	}
	if len(p.Files) != 2 {
		t.Errorf("len(Files) = %d; want 2 (negotiated to HTML)", len(p.Files))
	}
}

func TestHTTPClientFetchProjectHTTPError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "not found", http.StatusNotFound)
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	_, err := c.FetchProject(context.Background(), "httpx", FormatHTML)
	if err == nil {
		t.Fatal("FetchProject err = nil; want HTTP 404 error")
	}
	if !strings.Contains(err.Error(), "404") {
		t.Errorf("err = %v; want mention of 404", err)
	}
}

func TestHTTPClientFetchProjectEmptyBaseURL(t *testing.T) {
	c := &HTTPClient{
		HTTPClient: http.DefaultClient,
		UserAgent:  "test",
	}
	_, err := c.FetchProject(context.Background(), "httpx", FormatHTML)
	if err == nil {
		t.Fatal("FetchProject err = nil; want error on empty BaseURL")
	}
}

func TestHTTPClientFetchProjectNormalisesName(t *testing.T) {
	var gotPath string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath = r.URL.Path
		w.Header().Set("Content-Type", "text/html")
		w.Write([]byte(minimalHTML))
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	_, err := c.FetchProject(context.Background(), "Flask_SQLAlchemy", FormatHTML)
	if err != nil {
		t.Fatalf("FetchProject err = %v", err)
	}
	if gotPath != "/simple/flask-sqlalchemy/" {
		t.Errorf("path = %q; want /simple/flask-sqlalchemy/", gotPath)
	}
}

func TestHTTPClientFetchFile(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("filebody"))
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	body, err := c.FetchFile(context.Background(), srv.URL+"/files/x.whl")
	if err != nil {
		t.Fatalf("FetchFile err = %v", err)
	}
	defer body.Close()
	b, _ := io.ReadAll(body)
	if string(b) != "filebody" {
		t.Errorf("body = %q; want filebody", string(b))
	}
}

func TestHTTPClientFetchFileError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "gone", http.StatusGone)
	}))
	defer srv.Close()

	c := NewHTTPClient(srv.URL + "/simple/")
	_, err := c.FetchFile(context.Background(), srv.URL+"/files/x.whl")
	if err == nil {
		t.Fatal("FetchFile err = nil; want HTTP 410 error")
	}
}

func TestEnsureTrailingSlash(t *testing.T) {
	cases := []struct {
		in, want string
	}{
		{"", ""},
		{"https://example.com/", "https://example.com/"},
		{"https://example.com", "https://example.com/"},
		{"a", "a/"},
	}
	for _, tc := range cases {
		got := ensureTrailingSlash(tc.in)
		if got != tc.want {
			t.Errorf("ensureTrailingSlash(%q) = %q; want %q", tc.in, got, tc.want)
		}
	}
}

func TestIndexFormatString(t *testing.T) {
	if FormatHTML.String() != "html" {
		t.Errorf("FormatHTML.String() = %q", FormatHTML.String())
	}
	if FormatJSON.String() != "json" {
		t.Errorf("FormatJSON.String() = %q", FormatJSON.String())
	}
	if FormatUnknown.String() != "unknown" {
		t.Errorf("FormatUnknown.String() = %q", FormatUnknown.String())
	}
}

func TestNewHTTPClientDefaults(t *testing.T) {
	c := NewHTTPClient("https://pypi.org/simple")
	if c.BaseURL != "https://pypi.org/simple/" {
		t.Errorf("BaseURL = %q; want trailing slash", c.BaseURL)
	}
	if c.HTTPClient == nil {
		t.Error("HTTPClient = nil; want default")
	}
	if c.UserAgent == "" {
		t.Error("UserAgent = empty; want default")
	}
}
