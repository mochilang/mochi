package publish

import (
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	"time"
)

// OIDCProvider obtains an OpenID Connect identity token from the CI
// environment. Each implementation knows how to ask its specific runner
// for a token scoped to the requested audience.
type OIDCProvider interface {
	// Token fetches a fresh OIDC token for the given audience. Returns
	// the raw compact-JWT string.
	Token(audience string) (string, error)
}

// GitHubActionsProvider implements OIDCProvider against the GitHub Actions
// runner. The runner exposes ACTIONS_ID_TOKEN_REQUEST_URL and
// ACTIONS_ID_TOKEN_REQUEST_TOKEN to authenticated workflows that declare
// `permissions: id-token: write`.
type GitHubActionsProvider struct {
	// HTTP overrides the http client; nil means http.DefaultClient.
	HTTP *http.Client
	// Env overrides the environment lookup; nil means os.Getenv.
	Env func(string) string
}

// Token requests a fresh OIDC token from the GitHub runner.
func (p GitHubActionsProvider) Token(audience string) (string, error) {
	getenv := p.Env
	if getenv == nil {
		getenv = os.Getenv
	}
	endpoint := getenv("ACTIONS_ID_TOKEN_REQUEST_URL")
	runnerToken := getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN")
	if endpoint == "" || runnerToken == "" {
		return "", fmt.Errorf("publish: GitHub Actions OIDC env not set; run with permissions: id-token: write")
	}
	u, err := url.Parse(endpoint)
	if err != nil {
		return "", fmt.Errorf("publish: parse OIDC endpoint: %w", err)
	}
	q := u.Query()
	q.Set("audience", audience)
	u.RawQuery = q.Encode()
	client := p.HTTP
	if client == nil {
		client = http.DefaultClient
	}
	req, err := http.NewRequest(http.MethodGet, u.String(), nil)
	if err != nil {
		return "", fmt.Errorf("publish: build OIDC request: %w", err)
	}
	req.Header.Set("Authorization", "bearer "+runnerToken)
	req.Header.Set("Accept", "application/json")
	res, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("publish: OIDC request: %w", err)
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(res.Body)
		return "", fmt.Errorf("publish: OIDC status %d: %s", res.StatusCode, strings.TrimSpace(string(body)))
	}
	var payload struct {
		Value string `json:"value"`
	}
	if err := json.NewDecoder(res.Body).Decode(&payload); err != nil {
		return "", fmt.Errorf("publish: decode OIDC response: %w", err)
	}
	if payload.Value == "" {
		return "", fmt.Errorf("publish: empty OIDC token")
	}
	return payload.Value, nil
}

// StaticProvider returns a fixed token. Useful in tests and in
// already-minted-token CI flows.
type StaticProvider struct {
	TokenValue string
}

// Token returns the stored token unconditionally.
func (s StaticProvider) Token(string) (string, error) {
	if s.TokenValue == "" {
		return "", fmt.Errorf("publish: StaticProvider has empty TokenValue")
	}
	return s.TokenValue, nil
}

// MintedToken is the short-lived API token PyPI returns from its mint
// endpoint. Expires is the absolute UTC expiry per the response body.
type MintedToken struct {
	Token   string
	Expires time.Time
}

// MintAPIToken exchanges an OIDC token for a short-lived registry API token
// via the registry's `_/oidc/mint-token/` endpoint. Returns the minted
// token on success.
func MintAPIToken(client *http.Client, registry RegistryKind, oidcToken string) (MintedToken, error) {
	if oidcToken == "" {
		return MintedToken{}, fmt.Errorf("publish: empty OIDC token")
	}
	if client == nil {
		client = http.DefaultClient
	}
	endpoint := registry.URL() + "/_/oidc/mint-token/"
	body, err := json.Marshal(map[string]string{"token": oidcToken})
	if err != nil {
		return MintedToken{}, fmt.Errorf("publish: marshal mint request: %w", err)
	}
	req, err := http.NewRequest(http.MethodPost, endpoint, strings.NewReader(string(body)))
	if err != nil {
		return MintedToken{}, fmt.Errorf("publish: build mint request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")
	res, err := client.Do(req)
	if err != nil {
		return MintedToken{}, fmt.Errorf("publish: mint request: %w", err)
	}
	defer res.Body.Close()
	if res.StatusCode != http.StatusOK {
		raw, _ := io.ReadAll(res.Body)
		return MintedToken{}, fmt.Errorf("publish: mint status %d: %s", res.StatusCode, strings.TrimSpace(string(raw)))
	}
	var payload struct {
		Success bool   `json:"success"`
		Token   string `json:"token"`
		Expires string `json:"expires"`
	}
	if err := json.NewDecoder(res.Body).Decode(&payload); err != nil {
		return MintedToken{}, fmt.Errorf("publish: decode mint response: %w", err)
	}
	if !payload.Success || payload.Token == "" {
		return MintedToken{}, fmt.Errorf("publish: mint endpoint refused token")
	}
	exp, err := time.Parse(time.RFC3339, payload.Expires)
	if err != nil {
		exp = time.Time{}
	}
	return MintedToken{Token: payload.Token, Expires: exp}, nil
}
