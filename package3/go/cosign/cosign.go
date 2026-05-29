// Package cosign is the MEP-74 phase 13 Sigstore-cosign sibling-tag
// signer. It takes a publish commit (typically the output of
// `package3/go/publish.Publish`), exchanges an OIDC token for a
// keyless Sigstore bundle covering the publish commit SHA, and
// attaches the bundle as the annotated message body of a sibling
// tag `<tag>.sig` per the experimental gosum-cosign workflow draft
// of 2026-Q1. The sibling tag is pushed to the same remote the
// publish commit went to (unless DryRun is set).
//
// The package is layering-conservative: it imports `package3/go/
// publish` only for the Runner interface and otherwise depends only
// on the Go stdlib. The signing surface is split into a pure
// shaping layer (SignRequest.Validate) and an impure runner
// (Sign) so callers can validate the request before any side
// effect, and the network-facing signing primitive flows through a
// small Signer interface so the unit tests can exercise the full
// flow against an in-process fake.
//
// The bundle format mirrors the cosign signature envelope: a
// JSON document containing the signature bytes (base64-encoded),
// the public key or x509 certificate chain, and the Rekor
// transparency-log inclusion proof. The bundle is stored verbatim
// in the sibling tag's annotated message body so a `git fetch
// --tags` followed by a `git cat-file -p <tag>.sig` round-trips
// the signature without an out-of-band registry.
package cosign

import (
	"encoding/base64"
	"errors"
	"fmt"
	"strings"

	"mochi/package3/go/publish"
)

// ErrCosign is the package-wide error sentinel for the signer.
var ErrCosign = errors.New("cosign")

// DefaultAudience is the OIDC audience that Sigstore Fulcio
// expects in a keyless-sign token exchange. Mirrors the value
// hardcoded in the upstream cosign CLI.
const DefaultAudience = "sigstore"

// DefaultFulcioURL is the production Sigstore Fulcio certificate-
// authority endpoint. Override via SignRequest.FulcioURL for
// staging or air-gapped Sigstore deployments.
const DefaultFulcioURL = "https://fulcio.sigstore.dev"

// DefaultRekorURL is the production Sigstore Rekor transparency-
// log endpoint. Override via SignRequest.RekorURL for staging or
// air-gapped Sigstore deployments.
const DefaultRekorURL = "https://rekor.sigstore.dev"

// SignRequest is the in-memory shape of a `mochi pkg publish
// --cosign-sign` follow-up call. Callers populate it from the
// PublishResult of the upstream `publish.Publish` invocation plus
// the OIDC token harvested from the CI environment.
type SignRequest struct {
	// CommitSHA is the publish commit to sign. Required. Must be
	// the full 40-character git SHA so the bundle binds to a
	// specific revision; short SHAs are rejected because two
	// distinct commits in the same repo can share a short prefix.
	CommitSHA string
	// Tag is the publish tag the cosign sibling attaches to.
	// Required. The sibling tag will be `<Tag>.sig`. The tag is
	// also embedded in the bundle's `payload.tag` field so a
	// verifier can cross-check the sibling without reading the
	// surrounding repo.
	Tag string
	// WorkspaceRoot is the local git working tree to operate on.
	// Required. Typically the same path the upstream `publish.
	// Publish` returned in `PublishResult.WorkspaceRoot`.
	WorkspaceRoot string
	// Author is the git author / committer recorded on the
	// sibling tag. Required. Conventionally the same Author the
	// upstream publish used.
	Author publish.Author
	// OIDCToken is the JWT obtained from the CI's OIDC issuer
	// (GitHub Actions, GitLab CI, ...). Required. The Signer
	// trades this token for a short-lived Fulcio code-signing
	// certificate per Sigstore's keyless flow.
	OIDCToken string
	// FulcioURL is the Sigstore Fulcio CA endpoint. Optional;
	// defaults to DefaultFulcioURL.
	FulcioURL string
	// RekorURL is the Sigstore Rekor transparency-log endpoint.
	// Optional; defaults to DefaultRekorURL.
	RekorURL string
	// Audience is the OIDC audience the Signer requests. Optional;
	// defaults to DefaultAudience.
	Audience string
	// DryRun runs the signer end-to-end (so the bundle is
	// produced and the sibling tag is created locally) but skips
	// the `git push` step. Mirrors the upstream publish.PublishRequest
	// DryRun semantics so a single `mochi pkg publish --dry-run
	// --cosign-sign` invocation does the full local rehearsal.
	DryRun bool
}

// SignResult is the outcome of a Sign call. The Bundle is the
// cosign signature envelope (verbatim bytes that ended up in the
// sibling tag's annotated message body); SiblingTag is the tag
// name (typically `<Tag>.sig`); Pushed reflects whether the
// sibling tag was pushed to the remote.
type SignResult struct {
	SiblingTag string
	Bundle     []byte
	Pushed     bool
}

// Signer is the network-facing signing primitive. Implementations
// exchange `oidcToken` (with `audience`) for a Fulcio code-signing
// certificate, sign the SHA256 of `commitSHA` plus the metadata
// payload, log the signature to Rekor, and return the resulting
// cosign-style bundle bytes.
//
// The interface is split out so the unit tests can substitute a
// recording fake without touching the live Sigstore endpoints.
type Signer interface {
	Sign(commitSHA, tag, audience, oidcToken, fulcioURL, rekorURL string) (bundle []byte, err error)
}

// Validate checks the structural invariants of a SignRequest.
// Missing required fields, a malformed CommitSHA (must be 40
// lowercase hex chars), or an empty Tag surface here so callers
// fail fast.
func (r SignRequest) Validate() error {
	if !isFullSHA(r.CommitSHA) {
		return fmt.Errorf("%w: CommitSHA must be a 40-character lowercase hex string, got %q", ErrCosign, r.CommitSHA)
	}
	if strings.TrimSpace(r.Tag) == "" {
		return fmt.Errorf("%w: Tag is required", ErrCosign)
	}
	if strings.HasSuffix(r.Tag, ".sig") {
		return fmt.Errorf("%w: Tag %q already ends in .sig; pass the publish tag, not the sibling", ErrCosign, r.Tag)
	}
	if strings.TrimSpace(r.WorkspaceRoot) == "" {
		return fmt.Errorf("%w: WorkspaceRoot is required", ErrCosign)
	}
	if strings.TrimSpace(r.OIDCToken) == "" {
		return fmt.Errorf("%w: OIDCToken is required", ErrCosign)
	}
	if strings.TrimSpace(r.Author.Name) == "" {
		return fmt.Errorf("%w: Author.Name is required", ErrCosign)
	}
	if strings.TrimSpace(r.Author.Email) == "" {
		return fmt.Errorf("%w: Author.Email is required", ErrCosign)
	}
	return nil
}

// Sign runs the cosign sign-and-attach flow: it asks the Signer for
// a bundle, encodes the bundle into the annotated message body of
// the sibling tag, and (unless DryRun) pushes the sibling tag to
// the same remote the publish commit lives on. Callers stitch this
// together with the upstream publish.Publish call.
func Sign(req SignRequest, signer Signer, runner publish.Runner) (SignResult, error) {
	if err := req.Validate(); err != nil {
		return SignResult{}, err
	}
	audience := req.Audience
	if audience == "" {
		audience = DefaultAudience
	}
	fulcio := req.FulcioURL
	if fulcio == "" {
		fulcio = DefaultFulcioURL
	}
	rekor := req.RekorURL
	if rekor == "" {
		rekor = DefaultRekorURL
	}

	bundle, err := signer.Sign(req.CommitSHA, req.Tag, audience, req.OIDCToken, fulcio, rekor)
	if err != nil {
		return SignResult{}, fmt.Errorf("%w: signer: %v", ErrCosign, err)
	}
	if len(bundle) == 0 {
		return SignResult{}, fmt.Errorf("%w: signer returned empty bundle", ErrCosign)
	}

	siblingTag := req.Tag + ".sig"
	if err := runner.Run(req.WorkspaceRoot, "git", "config", "user.name", req.Author.Name); err != nil {
		return SignResult{}, fmt.Errorf("%w: git config user.name: %v", ErrCosign, err)
	}
	if err := runner.Run(req.WorkspaceRoot, "git", "config", "user.email", req.Author.Email); err != nil {
		return SignResult{}, fmt.Errorf("%w: git config user.email: %v", ErrCosign, err)
	}
	message := buildSiblingMessage(req.Tag, req.CommitSHA, bundle)
	if err := runner.Run(req.WorkspaceRoot, "git", "tag", "-a", siblingTag, req.CommitSHA, "-m", message); err != nil {
		return SignResult{}, fmt.Errorf("%w: git tag sibling: %v", ErrCosign, err)
	}

	pushed := false
	if !req.DryRun {
		if err := runner.Run(req.WorkspaceRoot, "git", "push", "origin", siblingTag); err != nil {
			return SignResult{}, fmt.Errorf("%w: git push sibling: %v", ErrCosign, err)
		}
		pushed = true
	}
	return SignResult{SiblingTag: siblingTag, Bundle: bundle, Pushed: pushed}, nil
}

// buildSiblingMessage renders the annotated tag message that
// carries the cosign bundle. The message has a header naming the
// signature scheme (`mochi-mep74-cosign-v1`), the publish tag, the
// signed commit SHA, and the bundle bytes base64-encoded so the tag
// message stays text-safe (git restricts what byte sequences can
// appear in tag messages).
func buildSiblingMessage(tag, commitSHA string, bundle []byte) string {
	var sb strings.Builder
	sb.WriteString("cosign-sig: mochi-mep74-cosign-v1\n")
	sb.WriteString("tag: ")
	sb.WriteString(tag)
	sb.WriteString("\n")
	sb.WriteString("commit: ")
	sb.WriteString(commitSHA)
	sb.WriteString("\n")
	sb.WriteString("bundle-base64: ")
	sb.WriteString(base64.StdEncoding.EncodeToString(bundle))
	sb.WriteString("\n")
	return sb.String()
}

// ParseSiblingMessage parses a sibling-tag annotated message back
// into its components: the publish tag, the signed commit SHA, and
// the verbatim bundle bytes (after base64 decode). This is the
// verifier-side helper a downstream consumer would call after `git
// cat-file -p <tag>.sig` to recover the bundle.
func ParseSiblingMessage(message string) (tag, commitSHA string, bundle []byte, err error) {
	var b64 string
	for _, raw := range strings.Split(message, "\n") {
		line := strings.TrimSpace(raw)
		if line == "" {
			continue
		}
		key, val, ok := cutColon(line)
		if !ok {
			continue
		}
		switch key {
		case "cosign-sig":
			if val != "mochi-mep74-cosign-v1" {
				return "", "", nil, fmt.Errorf("%w: unknown cosign-sig scheme %q", ErrCosign, val)
			}
		case "tag":
			tag = val
		case "commit":
			commitSHA = val
		case "bundle-base64":
			b64 = val
		}
	}
	if tag == "" || commitSHA == "" || b64 == "" {
		return "", "", nil, fmt.Errorf("%w: sibling message missing required fields", ErrCosign)
	}
	bundle, err = base64.StdEncoding.DecodeString(b64)
	if err != nil {
		return "", "", nil, fmt.Errorf("%w: bundle base64 decode: %v", ErrCosign, err)
	}
	return tag, commitSHA, bundle, nil
}

func cutColon(s string) (string, string, bool) {
	idx := strings.Index(s, ": ")
	if idx <= 0 {
		return "", "", false
	}
	return s[:idx], s[idx+2:], true
}

func isFullSHA(s string) bool {
	if len(s) != 40 {
		return false
	}
	for _, r := range s {
		if !((r >= '0' && r <= '9') || (r >= 'a' && r <= 'f')) {
			return false
		}
	}
	return true
}
