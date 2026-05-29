package publish

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"strings"
	"time"
)

// OIDCClaims is the subset of JWT claims trusted-publishing cares
// about. The publish flow validates these structurally (presence,
// expiry, audience) before sending the token to crates.io; it does
// NOT verify the signature here. The OIDC issuer's signature is
// verified by crates.io itself per Cargo RFC #3724.
type OIDCClaims struct {
	// Issuer is the `iss` claim, e.g.
	// "https://token.actions.githubusercontent.com" for GitHub
	// Actions or "https://gitlab.com" for GitLab CI.
	Issuer string `json:"iss"`
	// Subject is the `sub` claim. For GitHub Actions this looks
	// like `repo:mochilang/mochi:ref:refs/heads/main`.
	Subject string `json:"sub"`
	// Audience is the `aud` claim. Must equal crates.io for the
	// trusted-publishing exchange.
	Audience string `json:"aud"`
	// Expiry is the `exp` claim as a Unix timestamp.
	Expiry int64 `json:"exp"`
	// IssuedAt is the `iat` claim as a Unix timestamp.
	IssuedAt int64 `json:"iat"`
	// Repository is the GitHub Actions `repository` claim, e.g.
	// `mochilang/mochi`. Empty when the token is not from GHA.
	Repository string `json:"repository,omitempty"`
	// RepositoryOwner is the GHA `repository_owner` claim.
	RepositoryOwner string `json:"repository_owner,omitempty"`
	// JobWorkflowRef is the GHA `job_workflow_ref` claim, the
	// canonical pointer to the workflow that issued this token.
	JobWorkflowRef string `json:"job_workflow_ref,omitempty"`
}

// ParseOIDCToken parses a JWT-encoded OIDC token and returns its
// claims. The signature is NOT verified; the caller is expected to
// pass the raw JWT through to crates.io which performs the
// signature check upstream. Parsing failures (malformed JWT, invalid
// base64, malformed JSON) bubble up so the publish flow can fail
// fast before issuing the network call.
func ParseOIDCToken(jwt string) (OIDCClaims, error) {
	parts := strings.Split(jwt, ".")
	if len(parts) != 3 {
		return OIDCClaims{}, PublishError{Reason: "OIDC token: expected 3 JWT parts, got " + fmt.Sprint(len(parts))}
	}
	payload, err := base64.RawURLEncoding.DecodeString(parts[1])
	if err != nil {
		return OIDCClaims{}, fmt.Errorf("OIDC token: payload base64 decode: %w", err)
	}
	var claims OIDCClaims
	if err := json.Unmarshal(payload, &claims); err != nil {
		return OIDCClaims{}, fmt.Errorf("OIDC token: payload JSON decode: %w", err)
	}
	return claims, nil
}

// ValidateClaims checks that the parsed OIDC claims are acceptable
// for a crates.io trusted-publishing exchange:
//
//   - Audience must equal Audience ("crates.io").
//   - Expiry must be in the future relative to `now`.
//   - Issuer must be non-empty.
//   - Subject must be non-empty.
//
// Returns nil on success or a structured PublishError on failure.
func ValidateClaims(c OIDCClaims, now time.Time) error {
	if c.Audience != Audience {
		return PublishError{Reason: fmt.Sprintf("OIDC audience must be %q, got %q", Audience, c.Audience)}
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

// EncodeUnverifiedJWT produces a JWT-shaped string with the supplied
// claims, signed with a placeholder "none" alg header. Used only by
// tests; the publish flow never produces tokens, it only consumes
// them. Exported so test harnesses across the repo can compose
// realistic OIDC fixtures without pulling in a JWT library.
func EncodeUnverifiedJWT(claims OIDCClaims) string {
	header := base64.RawURLEncoding.EncodeToString([]byte(`{"alg":"none","typ":"JWT"}`))
	body, _ := json.Marshal(claims)
	payload := base64.RawURLEncoding.EncodeToString(body)
	return header + "." + payload + "."
}
