package publish

import (
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// PublishRequest is the in-memory shape for `mochi pkg publish --to=hex.pm`.
type PublishRequest struct {
	// PackageName is the Hex.pm package name (atom, e.g. "cowboy").
	PackageName string
	// Version is the version to publish.
	Version string
	// TarballBytes is the content of the .tar file to upload.
	// Typically produced by `rebar3 hex build` or assembled by Phase 9.
	TarballBytes []byte
	// OIDCToken is the short-lived JWT obtained from GitHub Actions OIDC.
	OIDCToken string
	// HexURL is the Hex.pm API base URL. Empty defaults to DefaultHexURL.
	HexURL string
	// DryRun, when true, validates + exchanges the OIDC token but stops
	// before the package upload. Returns the resolved API key in the result.
	DryRun bool
}

// PublishResult records the outcome of a Publish call.
type PublishResult struct {
	// APIKey is the short-lived Hex.pm API key obtained via OIDC exchange.
	APIKey string
	// UploadedURL is the endpoint the tarball was posted to.
	// Empty on DryRun.
	UploadedURL string
	// StatusCode is the HTTP status of the upload. 0 on DryRun.
	StatusCode int
}

// PublishError is a structural validation or protocol error.
type PublishError struct{ Reason string }

func (e PublishError) Error() string { return "publish: " + e.Reason }

// Transport is the HTTP abstraction for the Hex.pm API calls. Both the
// OIDC token exchange and the package upload go through this interface
// so the publish flow is unit-testable without a real Hex.pm endpoint.
type Transport interface {
	// Do issues an HTTP POST to url with the given headers and body.
	// It returns the response status code, body bytes, and any
	// transport-level error.
	Do(url string, headers map[string]string, body []byte) (status int, response []byte, err error)
}

// ExchangeOIDCToken presents the JWT to Hex.pm's trusted-publishing
// endpoint and returns the short-lived API key.
//
//	POST https://hex.pm/api/auth
//	Content-Type: application/json
//	{"token": "<jwt>"}
//
// On success Hex.pm responds with {"key": "<api-key>"}.
func ExchangeOIDCToken(oidcJWT string, hexURL string, transport Transport) (string, error) {
	if transport == nil {
		return "", PublishError{Reason: "nil transport"}
	}
	if hexURL == "" {
		hexURL = DefaultHexURL
	}
	body, _ := json.Marshal(map[string]string{"token": oidcJWT})
	headers := map[string]string{
		"Content-Type": "application/json",
		"User-Agent":   "mochi-pkg-publish/1.0 (hex.pm trusted-publishing)",
	}
	status, resp, err := transport.Do(hexURL+"/api/auth", headers, body)
	if err != nil {
		return "", fmt.Errorf("publish: OIDC exchange: %w", err)
	}
	if status < 200 || status >= 300 {
		return "", PublishError{Reason: fmt.Sprintf("OIDC exchange returned HTTP %d: %s", status, truncate(string(resp), 200))}
	}
	var result struct {
		Key string `json:"key"`
	}
	if err := json.Unmarshal(resp, &result); err != nil {
		return "", fmt.Errorf("publish: OIDC exchange response decode: %w", err)
	}
	if result.Key == "" {
		return "", PublishError{Reason: "OIDC exchange: response missing 'key' field"}
	}
	return result.Key, nil
}

// Validate checks the structural invariants of a PublishRequest.
func (r PublishRequest) Validate() error {
	if strings.TrimSpace(r.PackageName) == "" {
		return PublishError{Reason: "empty package name"}
	}
	if strings.TrimSpace(r.Version) == "" {
		return PublishError{Reason: "empty version"}
	}
	if strings.TrimSpace(r.OIDCToken) == "" {
		return PublishError{Reason: "empty OIDC token (HEX_API_KEY is not accepted)"}
	}
	if len(r.TarballBytes) == 0 {
		return PublishError{Reason: "empty tarball bytes"}
	}
	return nil
}

// Publish runs the Hex.pm trusted-publishing flow:
//
//  1. Validates the request.
//  2. Exchanges the OIDC token for a short-lived API key.
//  3. Uploads the package tarball (unless DryRun).
//
// The transport is the only impure dependency; pass a fake in tests.
func Publish(req PublishRequest, transport Transport) (PublishResult, error) {
	if err := req.Validate(); err != nil {
		return PublishResult{}, err
	}

	hexURL := req.HexURL
	if hexURL == "" {
		hexURL = DefaultHexURL
	}

	// Step 1: parse + validate the OIDC claims before touching the network.
	// Tests use far-future exp values in the JWT so time.Now() is fine here.
	claims, err := ParseOIDCToken(req.OIDCToken)
	if err != nil {
		return PublishResult{}, err
	}
	if err := ValidateClaims(claims, time.Now()); err != nil {
		return PublishResult{}, err
	}

	// Step 2: exchange OIDC token → short-lived API key.
	apiKey, err := ExchangeOIDCToken(req.OIDCToken, hexURL, transport)
	if err != nil {
		return PublishResult{}, err
	}

	result := PublishResult{APIKey: apiKey}
	if req.DryRun {
		return result, nil
	}

	// Step 3: upload the tarball.
	url := fmt.Sprintf("%s/api/packages/%s/releases", hexURL, req.PackageName)
	headers := map[string]string{
		"Authorization": "key " + apiKey,
		"Content-Type":  "application/octet-stream",
		"User-Agent":    "mochi-pkg-publish/1.0 (hex.pm trusted-publishing)",
	}
	status, _, err := transport.Do(url, headers, req.TarballBytes)
	if err != nil {
		return result, fmt.Errorf("publish: tarball upload: %w", err)
	}
	result.UploadedURL = url
	result.StatusCode = status
	if status < 200 || status >= 300 {
		return result, PublishError{Reason: fmt.Sprintf("tarball upload returned HTTP %d", status)}
	}
	return result, nil
}

func truncate(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + "..."
}
