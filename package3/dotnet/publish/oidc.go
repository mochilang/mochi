// Package publish provides NuGet package emit and trusted-publishing flows.
package publish

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"time"
)

// OIDCConfig holds configuration for the GitHub Actions OIDC trusted publishing flow.
type OIDCConfig struct {
	// PackageID is the NuGet package id to publish.
	PackageID string
	// PackageVersion is the version to publish.
	PackageVersion string
	// NuGetRegistryURL is the registry endpoint.
	// Default: "https://api.nuget.org/v3/index.json"
	NuGetRegistryURL string
	// OIDCTokenURL is the GitHub Actions OIDC token endpoint.
	// Sourced from $ACTIONS_ID_TOKEN_REQUEST_URL.
	OIDCTokenURL string
	// OIDCRequestToken is the bearer token for the OIDC request.
	// Sourced from $ACTIONS_ID_TOKEN_REQUEST_TOKEN.
	OIDCRequestToken string
	// DryRun skips the upload step; still exercises the token exchange flow
	// against a mock server if MockFulcioURL is set.
	DryRun bool
	// MockFulcioURL overrides the Fulcio endpoint for testing.
	MockFulcioURL string
}

// OIDCTokenResponse is the response from the OIDC token endpoint.
type OIDCTokenResponse struct {
	Value string `json:"value"`
}

// NuGetPublishToken is the short-lived token from nuget.org.
type NuGetPublishToken struct {
	Token     string
	ExpiresAt time.Time
}

// nugetExchangeResponse is the JSON body from the trusted-publishing exchange endpoint.
type nugetExchangeResponse struct {
	Token     string `json:"token"`
	ExpiresAt string `json:"expiresAt"`
}

// nugetExchangeRequest is the JSON body sent to the trusted-publishing exchange endpoint.
type nugetExchangeRequest struct {
	OIDCToken string `json:"oidcToken"`
}

// nuGetRegistryDefault is the default NuGet v3 index endpoint.
const nuGetRegistryDefault = "https://api.nuget.org/v3/index.json"

// nuGetExchangeEndpoint is the trusted-publishing token exchange endpoint.
const nuGetExchangeEndpoint = "https://api.nuget.org/v3/trustedpublishing/exchange"

// nuGetUploadEndpoint is the v2 package upload endpoint.
const nuGetUploadEndpoint = "https://www.nuget.org/api/v2/package"

// FetchOIDCToken retrieves a GitHub Actions OIDC JWT from the token endpoint.
func FetchOIDCToken(ctx context.Context, tokenURL, requestToken string) (string, error) {
	if tokenURL == "" {
		return "", fmt.Errorf("publish: OIDC token URL is empty; set ACTIONS_ID_TOKEN_REQUEST_URL")
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, tokenURL, nil)
	if err != nil {
		return "", fmt.Errorf("publish: build OIDC token request: %w", err)
	}
	if requestToken != "" {
		req.Header.Set("Authorization", "Bearer "+requestToken)
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("publish: fetch OIDC token: %w", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("publish: read OIDC token response: %w", err)
	}
	if resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("publish: OIDC token endpoint returned %d: %s", resp.StatusCode, string(body))
	}
	var tok OIDCTokenResponse
	if err := json.Unmarshal(body, &tok); err != nil {
		return "", fmt.Errorf("publish: decode OIDC token response: %w", err)
	}
	if tok.Value == "" {
		return "", fmt.Errorf("publish: OIDC token response has empty value")
	}
	return tok.Value, nil
}

// ExchangeForNuGetToken presents the OIDC JWT to nuget.org's trusted-publishing
// endpoint and returns a short-lived API token.
// Endpoint: POST https://api.nuget.org/v3/trustedpublishing/exchange
// Body: { "oidcToken": "<jwt>" }
// Response: { "token": "<nuget-token>", "expiresAt": "<iso8601>" }
func ExchangeForNuGetToken(ctx context.Context, cfg OIDCConfig, oidcJWT string) (*NuGetPublishToken, error) {
	exchangeURL := nuGetExchangeEndpoint
	if cfg.MockFulcioURL != "" {
		exchangeURL = cfg.MockFulcioURL
	}

	payload, err := json.Marshal(nugetExchangeRequest{OIDCToken: oidcJWT})
	if err != nil {
		return nil, fmt.Errorf("publish: marshal exchange request: %w", err)
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, exchangeURL, bytes.NewReader(payload))
	if err != nil {
		return nil, fmt.Errorf("publish: build exchange request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("publish: exchange token: %w", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("publish: read exchange response: %w", err)
	}
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("publish: exchange endpoint returned %d: %s", resp.StatusCode, string(body))
	}
	var r nugetExchangeResponse
	if err := json.Unmarshal(body, &r); err != nil {
		return nil, fmt.Errorf("publish: decode exchange response: %w", err)
	}
	if r.Token == "" {
		return nil, fmt.Errorf("publish: exchange response has empty token")
	}
	var expiresAt time.Time
	if r.ExpiresAt != "" {
		expiresAt, err = time.Parse(time.RFC3339, r.ExpiresAt)
		if err != nil {
			// Try without nanoseconds.
			expiresAt, err = time.Parse("2006-01-02T15:04:05Z", r.ExpiresAt)
			if err != nil {
				// Non-fatal: use zero time.
				expiresAt = time.Time{}
			}
		}
	}
	return &NuGetPublishToken{Token: r.Token, ExpiresAt: expiresAt}, nil
}

// PublishNupkg uploads a .nupkg file to nuget.org using the given API token.
// Endpoint: PUT https://www.nuget.org/api/v2/package
// Header: X-NuGet-ApiKey: <token>
func PublishNupkg(ctx context.Context, registryURL, token, nupkgPath string) error {
	uploadURL := registryURL
	if uploadURL == "" {
		uploadURL = nuGetUploadEndpoint
	}

	f, err := os.Open(nupkgPath)
	if err != nil {
		return fmt.Errorf("publish: open nupkg %s: %w", nupkgPath, err)
	}
	defer f.Close()

	req, err := http.NewRequestWithContext(ctx, http.MethodPut, uploadURL, f)
	if err != nil {
		return fmt.Errorf("publish: build upload request: %w", err)
	}
	req.Header.Set("X-NuGet-ApiKey", token)
	req.Header.Set("Content-Type", "application/octet-stream")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("publish: upload nupkg: %w", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return fmt.Errorf("publish: upload returned %d: %s", resp.StatusCode, string(body))
	}
	return nil
}

// OIDCPublishResult records the outcome of a trusted publishing run.
type OIDCPublishResult struct {
	PackageID           string
	PackageVersion      string
	DryRun              bool
	OIDCTokenFetched    bool
	NuGetTokenExchanged bool
	Uploaded            bool
	RegistryURL         string
	Error               error
}

// Run executes the full trusted publishing flow:
// 1. Fetch OIDC token from GitHub Actions
// 2. Exchange for nuget.org token
// 3. Upload .nupkg
// Returns an OIDCPublishResult with status and any error.
func Run(ctx context.Context, cfg OIDCConfig, nupkgPath string) (*OIDCPublishResult, error) {
	result := &OIDCPublishResult{
		PackageID:      cfg.PackageID,
		PackageVersion: cfg.PackageVersion,
		DryRun:         cfg.DryRun,
		RegistryURL:    cfg.NuGetRegistryURL,
	}
	if result.RegistryURL == "" {
		result.RegistryURL = nuGetRegistryDefault
	}

	// Step 1: Fetch OIDC token.
	oidcJWT, err := FetchOIDCToken(ctx, cfg.OIDCTokenURL, cfg.OIDCRequestToken)
	if err != nil {
		result.Error = err
		return result, err
	}
	result.OIDCTokenFetched = true

	// Step 2: Exchange for NuGet token.
	nugetTok, err := ExchangeForNuGetToken(ctx, cfg, oidcJWT)
	if err != nil {
		result.Error = err
		return result, err
	}
	result.NuGetTokenExchanged = true

	// Step 3: Upload .nupkg (unless dry run).
	if cfg.DryRun {
		return result, nil
	}

	uploadURL := cfg.NuGetRegistryURL
	if uploadURL == "" || uploadURL == nuGetRegistryDefault {
		uploadURL = nuGetUploadEndpoint
	}
	if err := PublishNupkg(ctx, uploadURL, nugetTok.Token, nupkgPath); err != nil {
		result.Error = err
		return result, err
	}
	result.Uploaded = true
	return result, nil
}
