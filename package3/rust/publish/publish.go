// Package publish is the MEP-73 trusted-publishing layer. It owns the
// `mochi pkg publish --to=crates.io` flow: producing a .crate tarball
// from a rendered library crate (Phase 9), exchanging an OIDC token
// for a short-lived crates.io publish credential per Cargo RFC #3724,
// signing the upload with Sigstore-keyless, and issuing the upload to
// crates.io's `/api/v1/crates/new` endpoint.
//
// The package is layering-conservative: it imports `package3/rust/
// library` for the Files input shape and otherwise only standard
// library packages.
//
// All network calls go through the Transport interface so the
// publish flow is unit-testable end-to-end against an in-process fake.
// The publish surface is split into a pure shaping layer (Tarball,
// PublishRequest, SignBundle) and an impure runner (Publish), so
// callers can validate the request before issuing the HTTP call.
package publish

import (
	"fmt"
	"strings"

	"mochi/package3/rust/library"
)

// Audience is the OIDC audience string that crates.io expects in a
// trusted-publisher token exchange per Cargo RFC #3724.
const Audience = "crates.io"

// DefaultRegistryURL is the production crates.io registry endpoint.
// The publish flow targets `<DefaultRegistryURL>/api/v1/crates/new`.
const DefaultRegistryURL = "https://crates.io"

// PublishRequest is the in-memory shape of a `mochi pkg publish` call.
// The struct is the boundary between the pure prep phase (Render,
// Sign) and the impure HTTP phase (Publish).
type PublishRequest struct {
	// CrateName is the crate name on crates.io. Must match the
	// rendered Cargo.toml.
	CrateName string
	// Version is the version to publish.
	Version string
	// Files is the rendered crate (typically library.Render's
	// output). Materialised into the .crate tarball.
	Files library.Files
	// OIDCToken is the JWT obtained from the CI's OIDC issuer.
	OIDCToken string
	// RegistryURL is the registry base URL. Empty defaults to
	// DefaultRegistryURL. Override for staging registries or for
	// in-process tests against a fake transport.
	RegistryURL string
	// DryRun, when true, builds the upload bundle and signs it but
	// stops short of the actual POST. Used by `mochi pkg publish
	// --dry-run`.
	DryRun bool
}

// PublishResult is the outcome of a Publish call. CrateBytes is the
// .crate tarball that was uploaded (or would have been, under
// DryRun); SignedBundle is the Sigstore bundle that accompanied it.
type PublishResult struct {
	CrateBytes   []byte
	SignedBundle []byte
	UploadedURL  string
	StatusCode   int
}

// Transport is the network abstraction. Publish issues exactly one
// POST through Do; the fake test transport satisfies this interface.
type Transport interface {
	Do(url string, headers map[string]string, body []byte) (status int, response []byte, err error)
}

// Validate checks the structural invariants of a PublishRequest.
// Empty crate name / version / OIDCToken, missing Cargo.toml in
// Files, or crate-name mismatch between PublishRequest and the
// rendered Cargo.toml are rejected here so callers fail fast.
func (r PublishRequest) Validate() error {
	if strings.TrimSpace(r.CrateName) == "" {
		return PublishError{Reason: "empty crate name"}
	}
	if strings.TrimSpace(r.Version) == "" {
		return PublishError{Reason: "empty version"}
	}
	if strings.TrimSpace(r.OIDCToken) == "" {
		return PublishError{Reason: "empty OIDC token"}
	}
	manifest, ok := r.Files["Cargo.toml"]
	if !ok {
		return PublishError{Reason: "missing Cargo.toml in Files"}
	}
	if !strings.Contains(manifest, fmt.Sprintf("name = %q", r.CrateName)) {
		return PublishError{Reason: fmt.Sprintf("Cargo.toml does not declare name = %q", r.CrateName)}
	}
	if !strings.Contains(manifest, fmt.Sprintf("version = %q", r.Version)) {
		return PublishError{Reason: fmt.Sprintf("Cargo.toml does not declare version = %q", r.Version)}
	}
	if _, ok := r.Files["src/lib.rs"]; !ok {
		return PublishError{Reason: "missing src/lib.rs in Files"}
	}
	return nil
}

// PublishError is a structural validation failure.
type PublishError struct {
	Reason string
}

// Error implements error.
func (e PublishError) Error() string { return "publish: " + e.Reason }

// registryURL returns the resolved registry base URL, defaulting to
// DefaultRegistryURL when r.RegistryURL is empty.
func (r PublishRequest) registryURL() string {
	if strings.TrimSpace(r.RegistryURL) == "" {
		return DefaultRegistryURL
	}
	return strings.TrimRight(r.RegistryURL, "/")
}

// Publish runs the trusted-publishing flow end-to-end:
//
//  1. Validates the request shape.
//  2. Packs Files into a .crate tarball (Tarball).
//  3. Produces a Sigstore-keyless attestation bundle (SignBundle)
//     over the OIDC token and the tarball SHA-256.
//  4. POSTs the multipart upload to <registry>/api/v1/crates/new
//     with `Authorization: Bearer <oidc-token>`.
//
// When DryRun is true, step 4 is skipped and the result's StatusCode
// is 0. The returned PublishResult always contains CrateBytes and
// SignedBundle for inspection.
//
// Transport is the only impure dependency; callers in tests pass an
// in-process fake; the CLI passes a real net/http-backed adapter.
func Publish(req PublishRequest, transport Transport) (PublishResult, error) {
	if err := req.Validate(); err != nil {
		return PublishResult{}, err
	}
	crateBytes, err := Tarball(req.CrateName, req.Version, req.Files)
	if err != nil {
		return PublishResult{}, err
	}
	bundle, err := SignBundle(req.CrateName, req.Version, crateBytes, req.OIDCToken)
	if err != nil {
		return PublishResult{}, err
	}
	res := PublishResult{
		CrateBytes:   crateBytes,
		SignedBundle: bundle,
	}
	if req.DryRun {
		return res, nil
	}
	if transport == nil {
		return res, PublishError{Reason: "nil transport on non-dry-run publish"}
	}
	url := req.registryURL() + "/api/v1/crates/new"
	headers := map[string]string{
		"Authorization":            "Bearer " + req.OIDCToken,
		"Content-Type":             "application/octet-stream",
		"User-Agent":               "mochi-pkg-publish/1.0",
		"X-Mochi-Sigstore-Bundle":  encodeBundleHeader(bundle),
		"X-Mochi-Publish-Audience": Audience,
	}
	body := renderUploadBody(crateBytes)
	status, _, err := transport.Do(url, headers, body)
	if err != nil {
		return res, fmt.Errorf("publish: upload: %w", err)
	}
	res.UploadedURL = url
	res.StatusCode = status
	if status < 200 || status >= 300 {
		return res, PublishError{Reason: fmt.Sprintf("upload failed with status %d", status)}
	}
	return res, nil
}

// renderUploadBody packs the .crate bytes into the body shape
// crates.io's /api/v1/crates/new endpoint expects: a 4-byte
// little-endian length prefix for the JSON metadata (here, an empty
// JSON object since the manifest is in the .crate itself), then a
// 4-byte little-endian length prefix for the .crate bytes, then the
// .crate bytes themselves.
//
// This matches the wire format Cargo uses; see `cargo/src/cargo/
// ops/registry.rs` for the canonical encoder.
func renderUploadBody(crateBytes []byte) []byte {
	meta := []byte("{}")
	out := make([]byte, 0, 8+len(meta)+len(crateBytes))
	out = appendU32LE(out, uint32(len(meta)))
	out = append(out, meta...)
	out = appendU32LE(out, uint32(len(crateBytes)))
	out = append(out, crateBytes...)
	return out
}

func appendU32LE(dst []byte, v uint32) []byte {
	return append(dst,
		byte(v), byte(v>>8), byte(v>>16), byte(v>>24))
}
