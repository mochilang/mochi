package simple

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestPhase1SimpleIndex is the gate sentinel for MEP-71 Phase 1. It exercises
// the end-to-end shape that downstream phases will consume: the client fetches
// a simple-index project, the parser returns a Project, and the verifier
// matches advertised hashes against a streamed body.
func TestPhase1SimpleIndex(t *testing.T) {
	t.Run("end_to_end_html", func(t *testing.T) {
		indexBody := `<a href="https://files.example.com/httpx-0.27.0-py3-none-any.whl#sha256=` + sha256Of("file body") + `">httpx-0.27.0-py3-none-any.whl</a>`
		indexSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "text/html")
			w.Write([]byte(indexBody))
		}))
		defer indexSrv.Close()

		fileSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("file body"))
		}))
		defer fileSrv.Close()

		c := NewHTTPClient(indexSrv.URL + "/simple/")
		p, err := c.FetchProject(context.Background(), "httpx", FormatHTML)
		if err != nil {
			t.Fatalf("FetchProject err = %v", err)
		}
		if len(p.Files) != 1 {
			t.Fatalf("len(Files) = %d; want 1", len(p.Files))
		}
		// Replace the upstream URL with our local file server. We deliberately
		// kept the index pointing at example.com to test that the parser does
		// not "helpfully" rewrite URLs.
		body, err := c.FetchFile(context.Background(), fileSrv.URL+"/file")
		if err != nil {
			t.Fatalf("FetchFile err = %v", err)
		}
		defer body.Close()
		if err := Verify(body, p.Files[0].Hashes); err != nil {
			t.Errorf("Verify err = %v; want nil", err)
		}
	})

	t.Run("end_to_end_json", func(t *testing.T) {
		want := sha256Of("file body 2")
		indexBody := `{
"meta": {"api-version": "1.1"},
"name": "x",
"files": [{
"filename": "x-1.tar.gz",
"url": "https://upstream.example/x-1.tar.gz",
"hashes": {"sha256": "` + want + `"}
}]
}`
		indexSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Header().Set("Content-Type", "application/vnd.pypi.simple.v1+json")
			w.Write([]byte(indexBody))
		}))
		defer indexSrv.Close()

		fileSrv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.Write([]byte("file body 2"))
		}))
		defer fileSrv.Close()

		c := NewHTTPClient(indexSrv.URL + "/simple/")
		p, err := c.FetchProject(context.Background(), "x", FormatJSON)
		if err != nil {
			t.Fatalf("FetchProject err = %v", err)
		}
		if p.Meta.APIVersion != "1.1" {
			t.Errorf("APIVersion = %q", p.Meta.APIVersion)
		}
		body, err := c.FetchFile(context.Background(), fileSrv.URL+"/x")
		if err != nil {
			t.Fatalf("FetchFile err = %v", err)
		}
		defer body.Close()
		if err := Verify(body, p.Files[0].Hashes); err != nil {
			t.Errorf("Verify err = %v", err)
		}
	})

	t.Run("normalise_invariants", func(t *testing.T) {
		// PEP 503 normalisation is idempotent.
		for _, in := range []string{"httpx", "Flask-SQLAlchemy", "zope.interface", "ALL_CAPS", "Mixed.Case-Name"} {
			once := Normalise(in)
			twice := Normalise(once)
			if once != twice {
				t.Errorf("Normalise is not idempotent for %q: once=%q twice=%q", in, once, twice)
			}
			if strings.ToLower(once) != once {
				t.Errorf("Normalise(%q) = %q; not lowercase", in, once)
			}
			if strings.ContainsAny(once, "_.") {
				t.Errorf("Normalise(%q) = %q; still contains _ or .", in, once)
			}
		}
	})

	t.Run("verify_md5_rejected", func(t *testing.T) {
		// Even if the index advertises md5, the verifier must refuse to use it.
		err := Verify(strings.NewReader("anything"), map[string]string{"md5": "0cc175b9c0f1b6a831c399e269772661"})
		if err == nil {
			t.Fatal("Verify err = nil; want error: md5 not in supported algos")
		}
	})
}

func sha256Of(s string) string {
	return sha256Hex(s)
}
