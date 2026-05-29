package publish

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestGitHubActionsProviderMissingEnv(t *testing.T) {
	p := GitHubActionsProvider{Env: func(string) string { return "" }}
	if _, err := p.Token("pypi"); err == nil || !strings.Contains(err.Error(), "OIDC env") {
		t.Fatalf("expected OIDC env error, got %v", err)
	}
}

func TestGitHubActionsProviderFetchesToken(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Query().Get("audience") != "pypi" {
			t.Errorf("audience = %q", r.URL.Query().Get("audience"))
		}
		if r.Header.Get("Authorization") != "bearer runner-tok" {
			t.Errorf("bad authz: %q", r.Header.Get("Authorization"))
		}
		_ = json.NewEncoder(w).Encode(map[string]string{"value": "jwt.value.sig"})
	}))
	defer srv.Close()
	p := GitHubActionsProvider{
		HTTP: srv.Client(),
		Env: func(k string) string {
			switch k {
			case "ACTIONS_ID_TOKEN_REQUEST_URL":
				return srv.URL
			case "ACTIONS_ID_TOKEN_REQUEST_TOKEN":
				return "runner-tok"
			}
			return ""
		},
	}
	tok, err := p.Token("pypi")
	if err != nil {
		t.Fatalf("Token: %v", err)
	}
	if tok != "jwt.value.sig" {
		t.Fatalf("token = %q", tok)
	}
}

func TestGitHubActionsProviderHandlesHTTPError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusForbidden)
		_, _ = w.Write([]byte("nope"))
	}))
	defer srv.Close()
	p := GitHubActionsProvider{
		HTTP: srv.Client(),
		Env: func(k string) string {
			if k == "ACTIONS_ID_TOKEN_REQUEST_URL" {
				return srv.URL
			}
			if k == "ACTIONS_ID_TOKEN_REQUEST_TOKEN" {
				return "x"
			}
			return ""
		},
	}
	if _, err := p.Token("pypi"); err == nil || !strings.Contains(err.Error(), "403") {
		t.Fatalf("expected 403 error, got %v", err)
	}
}

func TestGitHubActionsProviderRejectsEmptyToken(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewEncoder(w).Encode(map[string]string{"value": ""})
	}))
	defer srv.Close()
	p := GitHubActionsProvider{
		HTTP: srv.Client(),
		Env: func(k string) string {
			if k == "ACTIONS_ID_TOKEN_REQUEST_URL" {
				return srv.URL
			}
			if k == "ACTIONS_ID_TOKEN_REQUEST_TOKEN" {
				return "x"
			}
			return ""
		},
	}
	if _, err := p.Token("pypi"); err == nil || !strings.Contains(err.Error(), "empty OIDC") {
		t.Fatalf("expected empty token error, got %v", err)
	}
}

func TestStaticProviderReturnsValue(t *testing.T) {
	p := StaticProvider{TokenValue: "tok"}
	got, err := p.Token("pypi")
	if err != nil || got != "tok" {
		t.Fatalf("Token = (%q, %v)", got, err)
	}
}

func TestStaticProviderRejectsEmpty(t *testing.T) {
	if _, err := (StaticProvider{}).Token("pypi"); err == nil {
		t.Fatalf("expected error for empty StaticProvider")
	}
}

func TestMintAPITokenRejectsEmptyOIDC(t *testing.T) {
	if _, err := MintAPIToken(nil, RegistryPyPI, ""); err == nil {
		t.Fatalf("expected empty OIDC error")
	}
}

func TestMintAPITokenHappyPath(t *testing.T) {
	var seenBody map[string]string
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewDecoder(r.Body).Decode(&seenBody)
		_ = json.NewEncoder(w).Encode(map[string]any{
			"success": true,
			"token":   "pypi-mint-tok",
			"expires": "2026-05-30T01:00:00Z",
		})
	}))
	defer srv.Close()
	// Build a registry override using TestPyPI-shaped URL but the test
	// server host; we override URL via the http.DefaultClient + a custom
	// transport on the test server.
	client := srv.Client()
	client.Transport = redirectTransport{base: srv.Client().Transport, target: srv.URL}
	tok, err := MintAPIToken(client, RegistryPyPI, "oidc-jwt")
	if err != nil {
		t.Fatalf("MintAPIToken: %v", err)
	}
	if tok.Token != "pypi-mint-tok" {
		t.Fatalf("token = %q", tok.Token)
	}
	if tok.Expires.IsZero() {
		t.Fatalf("expires not parsed: %v", tok.Expires)
	}
	if seenBody["token"] != "oidc-jwt" {
		t.Fatalf("mint endpoint did not see oidc token, got %+v", seenBody)
	}
}

// redirectTransport rewrites the destination of an outgoing request to a
// local httptest server so MintAPIToken's hard-coded PyPI URL still hits
// the harness.
type redirectTransport struct {
	base   http.RoundTripper
	target string
}

func (rt redirectTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	u := req.URL
	u.Scheme = "http"
	// strip the original host; route to target
	u.Host = strings.TrimPrefix(rt.target, "http://")
	base := rt.base
	if base == nil {
		base = http.DefaultTransport
	}
	return base.RoundTrip(req)
}

func TestMintAPITokenRefusalSurfacesError(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusUnauthorized)
		_, _ = w.Write([]byte("bad oidc"))
	}))
	defer srv.Close()
	client := srv.Client()
	client.Transport = redirectTransport{base: srv.Client().Transport, target: srv.URL}
	if _, err := MintAPIToken(client, RegistryPyPI, "oidc-jwt"); err == nil || !strings.Contains(err.Error(), "401") {
		t.Fatalf("expected 401 error, got %v", err)
	}
}

func TestMintAPITokenRefusesUnsuccessfulResponse(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		_ = json.NewEncoder(w).Encode(map[string]any{"success": false, "token": ""})
	}))
	defer srv.Close()
	client := srv.Client()
	client.Transport = redirectTransport{base: srv.Client().Transport, target: srv.URL}
	if _, err := MintAPIToken(client, RegistryPyPI, "oidc-jwt"); err == nil || !strings.Contains(err.Error(), "refused") {
		t.Fatalf("expected refusal error, got %v", err)
	}
}
