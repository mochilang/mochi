package moduleproxy

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func newTestServer(t *testing.T, handler http.HandlerFunc) (*httptest.Server, *Client) {
	t.Helper()
	srv := httptest.NewServer(handler)
	t.Cleanup(srv.Close)
	return srv, NewClient(srv.URL)
}

func TestClientListSuccess(t *testing.T) {
	var gotURL string
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		gotURL = r.URL.Path
		w.Header().Set("Content-Type", "text/plain")
		io.WriteString(w, "v1.0.0\nv1.1.0\nv1.2.3\n")
	})
	_ = srv

	versions, err := c.List(context.Background(), "example.com/foo")
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	want := []string{"v1.0.0", "v1.1.0", "v1.2.3"}
	if len(versions) != len(want) {
		t.Fatalf("List len = %d; want %d (%v)", len(versions), len(want), versions)
	}
	for i, v := range want {
		if versions[i] != v {
			t.Errorf("List[%d] = %q; want %q", i, versions[i], v)
		}
	}
	if !strings.HasSuffix(gotURL, "/example.com/foo/@v/list") {
		t.Errorf("server saw URL %q; want suffix /example.com/foo/@v/list", gotURL)
	}
}

func TestClientListEscapesUppercase(t *testing.T) {
	var gotURL string
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		gotURL = r.URL.Path
		io.WriteString(w, "v1.0.0\n")
	})
	_ = srv

	if _, err := c.List(context.Background(), "github.com/Spf13/Cobra"); err != nil {
		t.Fatalf("List: %v", err)
	}
	if !strings.HasSuffix(gotURL, "/github.com/!spf13/!cobra/@v/list") {
		t.Errorf("server saw URL %q; want uppercase letters escaped to !-prefix", gotURL)
	}
}

func TestClientListSortsOutput(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "v1.2.3\nv1.0.0\nv1.1.0\n")
	})
	_ = srv

	got, err := c.List(context.Background(), "x")
	if err != nil {
		t.Fatalf("List: %v", err)
	}
	if !(got[0] < got[1] && got[1] < got[2]) {
		t.Errorf("List did not sort lexicographically: %v", got)
	}
}

func TestClientList404IsModuleNotFound(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "no module", http.StatusNotFound)
	})
	_ = srv

	_, err := c.List(context.Background(), "x")
	if !errors.Is(err, ErrModuleNotFound) {
		t.Errorf("List 404 = %v; want ErrModuleNotFound", err)
	}
}

func TestClientInfo(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		io.WriteString(w, `{"Version":"v1.2.3","Time":"2025-01-15T10:30:00Z"}`)
	})
	_ = srv

	info, err := c.Info(context.Background(), "x", "v1.2.3")
	if err != nil {
		t.Fatalf("Info: %v", err)
	}
	if info.Version != "v1.2.3" {
		t.Errorf("Version = %q; want v1.2.3", info.Version)
	}
	wantTime := time.Date(2025, 1, 15, 10, 30, 0, 0, time.UTC)
	if !info.Time.Equal(wantTime) {
		t.Errorf("Time = %v; want %v", info.Time, wantTime)
	}
}

func TestClientInfoEscapesVersion(t *testing.T) {
	var gotURL string
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		gotURL = r.URL.Path
		io.WriteString(w, `{"Version":"v1.2.3-Pre","Time":"2025-01-15T10:30:00Z"}`)
	})
	_ = srv

	if _, err := c.Info(context.Background(), "x", "v1.2.3-Pre"); err != nil {
		t.Fatalf("Info: %v", err)
	}
	if !strings.HasSuffix(gotURL, "/x/@v/v1.2.3-!pre.info") {
		t.Errorf("server saw URL %q; want version-escape on Pre suffix", gotURL)
	}
}

func TestClientInfo404IsVersionNotFound(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "no version", http.StatusNotFound)
	})
	_ = srv

	_, err := c.Info(context.Background(), "x", "v1.2.3")
	if !errors.Is(err, ErrVersionNotFound) {
		t.Errorf("Info 404 = %v; want ErrVersionNotFound", err)
	}
}

func TestClientMod(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain")
		io.WriteString(w, "module example.com/x\n\ngo 1.21\n")
	})
	_ = srv

	got, err := c.Mod(context.Background(), "example.com/x", "v1.0.0")
	if err != nil {
		t.Fatalf("Mod: %v", err)
	}
	if !strings.Contains(string(got), "module example.com/x") {
		t.Errorf("Mod returned %q; want to contain `module example.com/x`", got)
	}
}

func TestClientModLimitsSize(t *testing.T) {
	// Server hands a body larger than the 4 MB cap; the client must
	// stop reading at 4 MB rather than allocating unboundedly.
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		w.Write(bytes.Repeat([]byte("A"), 8*1024*1024))
	})
	_ = srv

	got, err := c.Mod(context.Background(), "x", "v1")
	if err != nil {
		t.Fatalf("Mod: %v", err)
	}
	if len(got) != 4*1024*1024 {
		t.Errorf("Mod returned %d bytes; want 4 MiB cap", len(got))
	}
}

func TestClientZip(t *testing.T) {
	zipBytes := []byte{'P', 'K', 0x03, 0x04, 0, 0, 0, 0} // not a valid zip but okay for streaming test
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/zip")
		w.Write(zipBytes)
	})
	_ = srv

	var buf bytes.Buffer
	n, err := c.Zip(context.Background(), "x", "v1.0.0", &buf)
	if err != nil {
		t.Fatalf("Zip: %v", err)
	}
	if n != int64(len(zipBytes)) {
		t.Errorf("Zip n = %d; want %d", n, len(zipBytes))
	}
	if !bytes.Equal(buf.Bytes(), zipBytes) {
		t.Errorf("Zip body mismatch")
	}
}

func TestClientZip404IsVersionNotFound(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "gone", http.StatusGone)
	})
	_ = srv

	var buf bytes.Buffer
	_, err := c.Zip(context.Background(), "x", "v1.0.0", &buf)
	if !errors.Is(err, ErrVersionNotFound) {
		t.Errorf("Zip 410 = %v; want ErrVersionNotFound", err)
	}
}

func TestClientPassesUserAgent(t *testing.T) {
	var gotUA string
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		gotUA = r.Header.Get("User-Agent")
		io.WriteString(w, "v1.0.0\n")
	})
	_ = srv

	c.UserAgent = "test-agent/1.0"
	if _, err := c.List(context.Background(), "x"); err != nil {
		t.Fatalf("List: %v", err)
	}
	if gotUA != "test-agent/1.0" {
		t.Errorf("User-Agent = %q; want test-agent/1.0", gotUA)
	}
}

func TestClientServerError(t *testing.T) {
	srv, c := newTestServer(t, func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "internal", http.StatusInternalServerError)
	})
	_ = srv

	_, err := c.List(context.Background(), "x")
	if err == nil {
		t.Errorf("List on 500 returned nil; want error")
	}
	if errors.Is(err, ErrModuleNotFound) {
		t.Errorf("500 should not classify as ErrModuleNotFound: %v", err)
	}
}

func TestNewClientNormalisesBaseURL(t *testing.T) {
	c := NewClient("https://example.com")
	if !strings.HasSuffix(c.BaseURL, "/") {
		t.Errorf("NewClient did not append trailing slash: %q", c.BaseURL)
	}
}

func TestNewClientDefaultBaseURL(t *testing.T) {
	c := NewClient("")
	if c.BaseURL != DefaultProxyURL {
		t.Errorf("default BaseURL = %q; want %q", c.BaseURL, DefaultProxyURL)
	}
}
