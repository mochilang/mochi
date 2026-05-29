// Package publish implements the Hex.pm trusted-publishing flow for MEP-66
// Direction 2 (Mochi as Erlang producer).
//
// The only supported token path is GitHub Actions OIDC (id-token: write).
// Long-lived HEX_API_KEY tokens are not accepted per the MEP-66 security
// design decision and the spec rationale in §3/rationale.
//
// Flow:
//
//  1. ObtainOIDCToken: read ACTIONS_ID_TOKEN_REQUEST_URL +
//     ACTIONS_ID_TOKEN_REQUEST_TOKEN from the environment and fetch a
//     short-lived JWT from the GitHub Actions OIDC endpoint with
//     audience "hex.pm".
//  2. ExchangeOIDCToken: POST the JWT to https://hex.pm/api/auth and
//     receive a short-lived API key.
//  3. Publish: use the API key to upload the package tarball to
//     https://hex.pm/api/packages/<name>/releases.
package publish

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// HexAudience is the OIDC audience value Hex.pm expects.
const HexAudience = "hex.pm"

// DefaultHexURL is the production Hex.pm API base.
const DefaultHexURL = "https://hex.pm"

// OIDCClaims is the subset of JWT claims the publish flow validates
// before presenting the token to Hex.pm. The signature is not verified
// here; Hex.pm verifies it upstream.
type OIDCClaims struct {
	Issuer    string `json:"iss"`
	Subject   string `json:"sub"`
	Audience  string `json:"aud"`
	Expiry    int64  `json:"exp"`
	IssuedAt  int64  `json:"iat"`
	// GitHub Actions specific claims
	Repository      string `json:"repository,omitempty"`
	RepositoryOwner string `json:"repository_owner,omitempty"`
	JobWorkflowRef  string `json:"job_workflow_ref,omitempty"`
}

// ParseOIDCToken parses the payload claims from a JWT without verifying
// its signature. Malformed tokens are rejected fast so the publish flow
// fails before any network call.
func ParseOIDCToken(jwt string) (OIDCClaims, error) {
	parts := strings.Split(jwt, ".")
	if len(parts) != 3 {
		return OIDCClaims{}, PublishError{Reason: fmt.Sprintf("OIDC token: expected 3 JWT parts, got %d", len(parts))}
	}
	payload, err := base64.RawURLEncoding.DecodeString(parts[1])
	if err != nil {
		return OIDCClaims{}, fmt.Errorf("OIDC token: payload base64: %w", err)
	}
	var claims OIDCClaims
	if err := json.Unmarshal(payload, &claims); err != nil {
		return OIDCClaims{}, fmt.Errorf("OIDC token: payload JSON: %w", err)
	}
	return claims, nil
}

// ValidateClaims checks that the parsed claims satisfy the Hex.pm
// trusted-publishing constraints:
//   - Audience must be "hex.pm".
//   - Expiry must be in the future relative to now.
//   - Issuer and Subject must be non-empty.
func ValidateClaims(c OIDCClaims, now time.Time) error {
	if c.Audience != HexAudience {
		return PublishError{Reason: fmt.Sprintf("OIDC audience must be %q, got %q", HexAudience, c.Audience)}
	}
	if c.Expiry == 0 {
		return PublishError{Reason: "OIDC token missing exp claim"}
	}
	if time.Unix(c.Expiry, 0).Before(now) {
		return PublishError{Reason: "OIDC token expired"}
	}
	if strings.TrimSpace(c.Issuer) == "" {
		return PublishError{Reason: "OIDC token missing iss claim"}
	}
	if strings.TrimSpace(c.Subject) == "" {
		return PublishError{Reason: "OIDC token missing sub claim"}
	}
	return nil
}

// EncodeUnverifiedJWT produces a JWT-shaped string with alg=none.
// Used exclusively in tests to generate fixture OIDC tokens without
// requiring a real CI environment.
func EncodeUnverifiedJWT(claims OIDCClaims) string {
	hdr := base64.RawURLEncoding.EncodeToString([]byte(`{"alg":"none","typ":"JWT"}`))
	body, _ := json.Marshal(claims)
	payload := base64.RawURLEncoding.EncodeToString(body)
	return hdr + "." + payload + "."
}

// GHAOIDCEnv holds the environment variable names for the GitHub Actions
// OIDC token endpoint. These are set by GitHub when `id-token: write`
// is declared in the workflow permissions block.
type GHAOIDCEnv struct {
	RequestURL   string // ACTIONS_ID_TOKEN_REQUEST_URL
	RequestToken string // ACTIONS_ID_TOKEN_REQUEST_TOKEN
}

// FromEnv reads the OIDC environment from a map (typically os.Environ
// converted to a map). Returns ErrOIDCEnvMissing if either variable is absent.
func FromEnv(env map[string]string) (GHAOIDCEnv, error) {
	url, okURL := env["ACTIONS_ID_TOKEN_REQUEST_URL"]
	tok, okTok := env["ACTIONS_ID_TOKEN_REQUEST_TOKEN"]
	if !okURL || !okTok || url == "" || tok == "" {
		return GHAOIDCEnv{}, ErrOIDCEnvMissing
	}
	return GHAOIDCEnv{RequestURL: url, RequestToken: tok}, nil
}

// ErrOIDCEnvMissing is returned when the GitHub Actions OIDC environment
// variables are absent. This typically means the job is running outside
// GitHub Actions or is missing `id-token: write` in its permissions block.
var ErrOIDCEnvMissing = PublishError{
	Reason: "GitHub Actions OIDC environment not found; set `id-token: write` in " +
		"your workflow permissions and ensure ACTIONS_ID_TOKEN_REQUEST_URL / " +
		"ACTIONS_ID_TOKEN_REQUEST_TOKEN are set. Long-lived HEX_API_KEY tokens " +
		"are not accepted.",
}
