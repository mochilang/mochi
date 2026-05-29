package vanity

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

// TestPhase17VanityResolverSentinel exercises the full vanity-resolver
// path against a real httptest.Server: a simulated vanity host serves
// the `<meta name="go-import">` redirect at `/x/sync?go-get=1`, the
// HTTPFetcher fetches it, ParseGoImport extracts the resolution, and
// the test asserts the resulting Module/VCS/RepoURL match the
// expected canonical record. This is the closest reproduction of
// the live golang.org/x/* path without making a real network call.
func TestPhase17VanityResolverSentinel(t *testing.T) {
	mux := http.NewServeMux()
	mux.HandleFunc("/x/sync", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Query().Get("go-get") != "1" {
			http.Error(w, "expected go-get=1 probe", http.StatusBadRequest)
			return
		}
		fmt.Fprintln(w, `<!doctype html><html><head>
<meta name="go-import" content="vanity.test/x/sync git https://repo.test/sync">
</head></html>`)
	})
	srv := httptest.NewTLSServer(mux)
	defer srv.Close()

	// Rewrite the host so the fetcher hits the local server. The
	// HTTPFetcher itself takes the URL verbatim, so we use a
	// rewriting fetcher that maps vanity.test → the live test
	// server's URL.
	transport := &rewriteTransport{
		Base:   srv.Client().Transport,
		Target: srv.URL,
		From:   "https://vanity.test",
	}
	fetcher := &HTTPFetcher{Client: &http.Client{Transport: transport}}

	res, err := Resolve("vanity.test/x/sync", fetcher)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if res.Module != "vanity.test/x/sync" {
		t.Errorf("Module = %q", res.Module)
	}
	if res.VCS != "git" {
		t.Errorf("VCS = %q", res.VCS)
	}
	if res.RepoURL != "https://repo.test/sync" {
		t.Errorf("RepoURL = %q", res.RepoURL)
	}
}

// TestPhase17VanityResolverServer404Surfaces verifies the resolver
// wraps a non-2xx response in ErrVanity rather than masking it as
// a "no go-import meta" parse failure.
func TestPhase17VanityResolverServer404Surfaces(t *testing.T) {
	srv := httptest.NewTLSServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.Error(w, "not found", http.StatusNotFound)
	}))
	defer srv.Close()

	transport := &rewriteTransport{
		Base:   srv.Client().Transport,
		Target: srv.URL,
		From:   "https://vanity.test",
	}
	fetcher := &HTTPFetcher{Client: &http.Client{Transport: transport}}

	_, err := Resolve("vanity.test/x/sync", fetcher)
	if err == nil {
		t.Fatalf("expected error for 404")
	}
	if !strings.Contains(err.Error(), "status 404") {
		t.Errorf("error should name the status code: %v", err)
	}
}

// TestPhase17WasmPublishGateRejectsCgoSurface walks a hand-curated
// apisurface that imports runtime/cgo and asserts the wasm-wasip1
// publish gate reports the violation. Mirrors the closeout
// invariant for the publish-direction matrix.
func TestPhase17WasmPublishGateRejectsCgoSurface(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "example.com/cgo-using",
		Imports:    []string{"fmt", "runtime/cgo", "syscall/js"},
	}
	got := CheckPublish(WasmTargetWasip1, pkg)
	if len(got) != 2 {
		t.Fatalf("want 2 violations, got %v", got)
	}
	// wasip1 bans both runtime/cgo and syscall/js.
	wantWheres := []string{"runtime/cgo", "syscall/js"}
	for i, w := range wantWheres {
		if got[i].Where != w {
			t.Errorf("got[%d].Where = %q, want %q", i, got[i].Where, w)
		}
	}

	// On wasm-js only runtime/cgo trips.
	gotJS := CheckPublish(WasmTargetJS, pkg)
	if len(gotJS) != 1 || gotJS[0].Where != "runtime/cgo" {
		t.Fatalf("wasm-js: want 1 cgo violation, got %v", gotJS)
	}
}

// TestPhase17WasmPublishGatePassesPureSurface confirms a pure-go
// surface (no cgo, no syscall/js) publishes cleanly to both wasm
// targets — the green-path of the matrix.
func TestPhase17WasmPublishGatePassesPureSurface(t *testing.T) {
	pkg := apisurface.Package{
		ImportPath: "example.com/pure",
		Imports:    []string{"fmt", "errors", "strings", "math"},
		Funcs: []apisurface.Func{
			{
				Name:    "Hash",
				Params:  []apisurface.Param{{Name: "s", Type: "string"}},
				Results: []apisurface.Param{{Type: "uint64"}},
			},
		},
	}
	for _, target := range []WasmTarget{WasmTargetWasip1, WasmTargetJS} {
		if !IsPublishable(target, pkg) {
			t.Errorf("%s: pure surface should be publishable, got %v", target, CheckPublish(target, pkg))
		}
	}
}

// rewriteTransport rewrites a fixed URL prefix (`From`) to the
// httptest server's URL before delegating to the base RoundTripper.
// Used so the in-test HTTPFetcher can hit a "vanity.test" host name
// without DNS.
type rewriteTransport struct {
	Base   http.RoundTripper
	From   string
	Target string
}

func (t *rewriteTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	if strings.HasPrefix(req.URL.String(), t.From) {
		newURL := t.Target + strings.TrimPrefix(req.URL.String(), t.From)
		newReq, err := http.NewRequestWithContext(req.Context(), req.Method, newURL, req.Body)
		if err != nil {
			return nil, err
		}
		newReq.Header = req.Header
		req = newReq
	}
	return t.Base.RoundTrip(req)
}
