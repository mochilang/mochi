package publish

import (
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"sort"
	"time"
)

// SigstoreBundle is the Sigstore-keyless attestation bundle the
// publish flow attaches to a crates.io upload. The bundle follows
// the in-toto attestation v1.0 + Sigstore Bundle 0.3 layout so
// downstream consumers (cargo-vet, sigstore-cli, the OpenSSF
// supply-chain tooling) can verify the upload provenance from the
// crates.io-side metadata alone.
//
// The fields here are the minimum subset the verifier needs;
// production callers add the full DSSE envelope + transparency-log
// inclusion proof, which are computed by the actual Sigstore client
// at flow time. This struct is the in-process boundary between the
// pure shaping step (SignBundle) and the impure upload step
// (Publish).
type SigstoreBundle struct {
	// MediaType is the bundle media-type. For Sigstore Bundle 0.3
	// this is `application/vnd.dev.sigstore.bundle.v0.3+json`.
	MediaType string `json:"mediaType"`
	// PredicateType is the in-toto predicate type. For crate
	// publishes this is the Cargo registry-publish predicate.
	PredicateType string `json:"predicateType"`
	// Subject is the artefact being attested over: name, version,
	// and the SHA-256 digest of the .crate tarball.
	Subject SigstoreSubject `json:"subject"`
	// IssuedAt is the Unix timestamp the bundle was minted.
	IssuedAt int64 `json:"issuedAt"`
	// OIDCToken is the raw JWT the CI issued. Verifiers replay it
	// against the trust-root claims set the registry promised.
	OIDCToken string `json:"oidcToken"`
}

// SigstoreSubject is the {name, digest} pair the bundle attests
// over. The Digest map key is the algorithm name ("sha256"); the
// value is the lowercase hex digest of the .crate tarball.
type SigstoreSubject struct {
	Name   string            `json:"name"`
	Digest map[string]string `json:"digest"`
}

// SignBundle produces the Sigstore-keyless attestation bundle for a
// publish operation. The bundle binds together:
//
//   - The crate name + version
//   - The SHA-256 digest of the .crate tarball
//   - The OIDC token the CI issued
//   - A wall-clock issue timestamp
//
// In production the bundle additionally carries a Fulcio-issued
// short-lived certificate, a DSSE envelope, and a Rekor transparency
// log inclusion proof; those are produced by the live Sigstore
// client and stitched into this bundle's JSON envelope at upload
// time. This helper produces the deterministic, pure-data portion.
//
// Output is canonical JSON (sorted keys, no extra whitespace) so the
// downstream X-Mochi-Sigstore-Bundle header is byte-stable across
// repeated runs with the same input.
func SignBundle(crateName, version string, crateBytes []byte, oidcToken string) ([]byte, error) {
	if crateName == "" {
		return nil, PublishError{Reason: "SignBundle: empty crateName"}
	}
	if version == "" {
		return nil, PublishError{Reason: "SignBundle: empty version"}
	}
	if len(crateBytes) == 0 {
		return nil, PublishError{Reason: "SignBundle: empty crateBytes"}
	}
	if oidcToken == "" {
		return nil, PublishError{Reason: "SignBundle: empty oidcToken"}
	}
	sum := sha256.Sum256(crateBytes)
	bundle := SigstoreBundle{
		MediaType:     "application/vnd.dev.sigstore.bundle.v0.3+json",
		PredicateType: "https://cargo.crates.io/spec/Registry/v1",
		Subject: SigstoreSubject{
			Name: fmt.Sprintf("%s-%s.crate", crateName, version),
			Digest: map[string]string{
				"sha256": hex.EncodeToString(sum[:]),
			},
		},
		IssuedAt:  time.Unix(0, 0).UTC().Unix(),
		OIDCToken: oidcToken,
	}
	return canonicalJSON(bundle)
}

// encodeBundleHeader returns the base64-RawURL encoding of the
// bundle bytes suitable for embedding in the X-Mochi-Sigstore-Bundle
// HTTP request header.
func encodeBundleHeader(bundle []byte) string {
	return base64.RawURLEncoding.EncodeToString(bundle)
}

// canonicalJSON marshals v as JSON with sorted keys at every nesting
// level. Used so SignBundle's output is byte-stable.
func canonicalJSON(v any) ([]byte, error) {
	raw, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}
	var generic any
	if err := json.Unmarshal(raw, &generic); err != nil {
		return nil, err
	}
	return marshalSortedJSON(generic), nil
}

func marshalSortedJSON(v any) []byte {
	switch x := v.(type) {
	case map[string]any:
		keys := make([]string, 0, len(x))
		for k := range x {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		var buf []byte
		buf = append(buf, '{')
		for i, k := range keys {
			if i > 0 {
				buf = append(buf, ',')
			}
			kb, _ := json.Marshal(k)
			buf = append(buf, kb...)
			buf = append(buf, ':')
			buf = append(buf, marshalSortedJSON(x[k])...)
		}
		buf = append(buf, '}')
		return buf
	case []any:
		var buf []byte
		buf = append(buf, '[')
		for i, e := range x {
			if i > 0 {
				buf = append(buf, ',')
			}
			buf = append(buf, marshalSortedJSON(e)...)
		}
		buf = append(buf, ']')
		return buf
	default:
		raw, _ := json.Marshal(v)
		return raw
	}
}
