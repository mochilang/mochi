// Package publish implements OIDC trusted publishing to rubygems.org.
// It performs the full token-exchange flow described in MEP-76 phase 11:
// get an OIDC ID token from the CI environment, exchange it for a short-lived
// rubygems.org API key, push the .gem file, and optionally record provenance
// in the Sigstore transparency log.
//
// Phase 11 of MEP-76.
// See website/docs/research/0076/07-oidc-trusted-publishing.md for context.
package publish

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"path/filepath"
	"time"
)

// ErrNoOIDCToken is returned by GetOIDCToken when no OIDC ID token is
// available in the environment (not running inside a supported CI system).
var ErrNoOIDCToken = errors.New("no OIDC ID token available in environment")

// rubyGemsBase is the default rubygems.org API base URL.
const rubyGemsBase = "https://rubygems.org"

// sigstoreBase is the Sigstore Rekor transparency log endpoint.
const sigstoreBase = "https://rekor.sigstore.dev"

// PublishConfig holds all the publish-time parameters.
type PublishConfig struct {
	// GemPath is the path to the .gem file built by gemspec.Emit + gem build.
	GemPath string
	// GemName is the name of the gem (e.g. "mochi-example").
	GemName string
	// Version is the gem version string (e.g. "0.1.0").
	Version string
	// DryRun skips the actual push but still computes the SHA-256.
	DryRun bool
	// OIDCToken is the OIDC ID token. If empty, GetOIDCToken is called to
	// obtain one from the CI environment.
	OIDCToken string

	// RegistryURL overrides the rubygems.org API base for testing.
	// Leave empty to use the default https://rubygems.org.
	RegistryURL string

	// OIDCTokenURL overrides the OIDC token request URL for testing.
	// Leave empty to read from $ACTIONS_ID_TOKEN_REQUEST_URL.
	OIDCTokenURL string
}

// Result is returned by Publish on success.
type Result struct {
	GemName    string
	Version    string
	SHA256     string // hex-encoded SHA-256 of the pushed .gem
	RekorLogID string // transparency log entry ID (empty on dry run or if sigstore unavailable)
	DryRun     bool
}

// Publish performs the full trusted-publishing flow:
//  1. Obtain OIDC token (from cfg.OIDCToken or CI environment).
//  2. Exchange the OIDC token for a short-lived rubygems.org API key.
//  3. Read the .gem file and compute its SHA-256.
//  4. Push the gem (skipped on dry run).
//  5. Record provenance in Sigstore (best-effort, only if SIGSTORE_ENABLED=true).
//
// Returns a Result on success or a wrapped error on failure.
func Publish(cfg PublishConfig) (*Result, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()

	idToken := cfg.OIDCToken
	if idToken == "" && !cfg.DryRun {
		var err error
		idToken, err = getOIDCTokenFromEnv(ctx, cfg.OIDCTokenURL)
		if err != nil {
			return nil, fmt.Errorf("publish: get OIDC token: %w", err)
		}
	}

	var apiKey string
	if !cfg.DryRun {
		var err error
		apiKey, err = exchangeOIDCTokenWithBase(ctx, idToken, cfg.RegistryURL)
		if err != nil {
			return nil, fmt.Errorf("publish: exchange OIDC token: %w", err)
		}
	}

	data, err := os.ReadFile(cfg.GemPath)
	if err != nil {
		return nil, fmt.Errorf("publish: read gem file: %w", err)
	}
	sum := sha256.Sum256(data)
	sha256hex := hex.EncodeToString(sum[:])

	if !cfg.DryRun {
		if err := pushGemWithBase(ctx, cfg.GemPath, apiKey, cfg.RegistryURL); err != nil {
			return nil, fmt.Errorf("publish: push gem: %w", err)
		}
	}

	rekorID := ""
	if !cfg.DryRun && os.Getenv("SIGSTORE_ENABLED") == "true" {
		rekorID, _ = recordSigstore(ctx, cfg.GemName, cfg.Version, sha256hex)
		// Ignore sigstore errors; it is best-effort only.
	}

	return &Result{
		GemName:    cfg.GemName,
		Version:    cfg.Version,
		SHA256:     sha256hex,
		RekorLogID: rekorID,
		DryRun:     cfg.DryRun,
	}, nil
}

// GetOIDCToken returns the OIDC ID token from the CI environment.
// Supports GitHub Actions via ACTIONS_ID_TOKEN_REQUEST_URL and
// ACTIONS_ID_TOKEN_REQUEST_TOKEN.
// Returns ErrNoOIDCToken if neither is available.
func GetOIDCToken(ctx context.Context) (string, error) {
	return getOIDCTokenFromEnv(ctx, "")
}

// getOIDCTokenFromEnv is the internal implementation that also accepts an
// optional override URL for testing.
func getOIDCTokenFromEnv(ctx context.Context, urlOverride string) (string, error) {
	requestURL := urlOverride
	if requestURL == "" {
		requestURL = os.Getenv("ACTIONS_ID_TOKEN_REQUEST_URL")
	}
	bearerToken := os.Getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN")

	if requestURL == "" || bearerToken == "" {
		return "", ErrNoOIDCToken
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, requestURL, nil)
	if err != nil {
		return "", fmt.Errorf("create OIDC token request: %w", err)
	}
	req.Header.Set("Authorization", "Bearer "+bearerToken)
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("OIDC token request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("OIDC token endpoint returned HTTP %d: %s", resp.StatusCode, bytes.TrimSpace(body))
	}

	var payload struct {
		Value string `json:"value"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&payload); err != nil {
		return "", fmt.Errorf("decode OIDC token response: %w", err)
	}
	if payload.Value == "" {
		return "", fmt.Errorf("OIDC token response contained empty value field")
	}
	return payload.Value, nil
}

// ExchangeOIDCToken exchanges an OIDC ID token for a short-lived rubygems.org
// API key. It POSTs to the trusted publishing endpoint and returns the key.
func ExchangeOIDCToken(ctx context.Context, idToken string) (string, error) {
	return exchangeOIDCTokenWithBase(ctx, idToken, "")
}

// exchangeOIDCTokenWithBase is the internal implementation that accepts an
// optional base URL override for testing.
func exchangeOIDCTokenWithBase(ctx context.Context, idToken, baseURL string) (string, error) {
	if baseURL == "" {
		baseURL = rubyGemsBase
	}
	url := baseURL + "/api/v1/trusted_publishing/tokens"

	body, err := json.Marshal(map[string]string{
		"rubygems_oidc_token": idToken,
	})
	if err != nil {
		return "", fmt.Errorf("marshal OIDC exchange body: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return "", fmt.Errorf("create OIDC exchange request: %w", err)
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", fmt.Errorf("OIDC exchange request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		respBody, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("OIDC token exchange returned HTTP %d: %s", resp.StatusCode, bytes.TrimSpace(respBody))
	}

	var payload struct {
		APIKey string `json:"api_key"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&payload); err != nil {
		return "", fmt.Errorf("decode OIDC exchange response: %w", err)
	}
	if payload.APIKey == "" {
		return "", fmt.Errorf("OIDC exchange response contained empty api_key field")
	}
	return payload.APIKey, nil
}

// PushGem pushes the .gem file at gemPath to rubygems.org using apiKey.
// It uses a multipart/form-data POST with the field name "gem".
func PushGem(ctx context.Context, gemPath, apiKey string) error {
	return pushGemWithBase(ctx, gemPath, apiKey, "")
}

// pushGemWithBase is the internal implementation that accepts an optional
// base URL override for testing.
func pushGemWithBase(ctx context.Context, gemPath, apiKey, baseURL string) error {
	if baseURL == "" {
		baseURL = rubyGemsBase
	}
	url := baseURL + "/api/v1/gems"

	f, err := os.Open(gemPath)
	if err != nil {
		return fmt.Errorf("open gem file: %w", err)
	}
	defer f.Close()

	var buf bytes.Buffer
	mw := multipart.NewWriter(&buf)
	fw, err := mw.CreateFormFile("gem", filepath.Base(gemPath))
	if err != nil {
		return fmt.Errorf("create multipart field: %w", err)
	}
	if _, err := io.Copy(fw, f); err != nil {
		return fmt.Errorf("write gem to multipart: %w", err)
	}
	if err := mw.Close(); err != nil {
		return fmt.Errorf("close multipart writer: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, &buf)
	if err != nil {
		return fmt.Errorf("create gem push request: %w", err)
	}
	req.Header.Set("Content-Type", mw.FormDataContentType())
	req.Header.Set("Authorization", apiKey)

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("gem push request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusCreated {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("gem push returned HTTP %d: %s", resp.StatusCode, bytes.TrimSpace(body))
	}
	return nil
}

// oidcEntryPayload is the JSON body sent to the Sigstore Rekor log.
type oidcEntryPayload struct {
	Kind       string            `json:"kind"`
	APIVersion string            `json:"apiVersion"`
	Spec       map[string]string `json:"spec"`
}

// rekorResponse contains the fields we care about from a Rekor log entry.
type rekorResponse struct {
	EntryID string `json:"entryID"`
}

// recordSigstore records provenance in the Sigstore Rekor transparency log.
// It is best-effort: errors are silently discarded by the caller.
func recordSigstore(ctx context.Context, gemName, version, sha256hex string) (string, error) {
	url := sigstoreBase + "/api/v1/log/entries"

	payload := oidcEntryPayload{
		Kind:       "gem",
		APIVersion: "0.0.1",
		Spec: map[string]string{
			"gem_name": gemName,
			"version":  version,
			"sha256":   sha256hex,
		},
	}
	body, err := json.Marshal(payload)
	if err != nil {
		return "", err
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, bytes.NewReader(body))
	if err != nil {
		return "", err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Accept", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusCreated && resp.StatusCode != http.StatusOK {
		return "", fmt.Errorf("rekor returned HTTP %d", resp.StatusCode)
	}

	var rr rekorResponse
	if err := json.NewDecoder(resp.Body).Decode(&rr); err != nil {
		return "", err
	}
	return rr.EntryID, nil
}
