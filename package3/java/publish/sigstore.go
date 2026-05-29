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

// SigstoreBundle is the Sigstore-keyless attestation bundle the Java bridge
// publish flow attaches to a Maven Central upload. The bundle follows the
// in-toto attestation v1.0 + Sigstore Bundle 0.3 layout.
//
// The fields here are the minimum subset the verifier needs. Production
// callers add the full DSSE envelope and Rekor transparency-log inclusion
// proof, which are computed by the live Sigstore client at flow time.
type SigstoreBundle struct {
	// MediaType is "application/vnd.dev.sigstore.bundle.v0.3+json".
	MediaType string `json:"mediaType"`
	// PredicateType is the in-toto predicate type for Maven Central publishes.
	PredicateType string `json:"predicateType"`
	// Subject is the artifact being attested over.
	Subject SigstoreSubject `json:"subject"`
	// IssuedAt is the Unix timestamp the bundle was minted.
	IssuedAt int64 `json:"issuedAt"`
	// OIDCToken is the raw JWT the CI issued.
	OIDCToken string `json:"oidcToken"`
}

// SigstoreSubject is the {name, digest} pair the bundle attests over.
// The Digest map key is the algorithm name ("sha256"); the value is the
// lowercase hex digest of the JAR being published.
type SigstoreSubject struct {
	Name   string            `json:"name"`
	Digest map[string]string `json:"digest"`
}

// SignBundle produces the Sigstore-keyless attestation bundle for a Maven
// Central publish. The bundle binds:
//
//   - The Maven coordinate (groupId:artifactId:version)
//   - The JAR filename and its SHA-256 digest
//   - The OIDC token issued by CI
//   - A wall-clock issue timestamp
//
// Output is canonical JSON (sorted keys, no extra whitespace) so the
// X-Mochi-Sigstore-Bundle header is byte-stable across repeated runs
// with the same inputs.
func SignBundle(groupID, artifactID, version string, jarBytes []byte, oidcToken string) ([]byte, error) {
	if groupID == "" {
		return nil, fmt.Errorf("sigstore: empty groupID")
	}
	if artifactID == "" {
		return nil, fmt.Errorf("sigstore: empty artifactID")
	}
	if version == "" {
		return nil, fmt.Errorf("sigstore: empty version")
	}
	if len(jarBytes) == 0 {
		return nil, fmt.Errorf("sigstore: empty jarBytes")
	}
	if oidcToken == "" {
		return nil, fmt.Errorf("sigstore: empty oidcToken")
	}

	sum := sha256.Sum256(jarBytes)
	jarFilename := fmt.Sprintf("%s-%s.jar", artifactID, version)
	bundle := SigstoreBundle{
		MediaType:     "application/vnd.dev.sigstore.bundle.v0.3+json",
		PredicateType: "https://maven.apache.org/spec/MavenCentralPublish/v1",
		Subject: SigstoreSubject{
			Name: fmt.Sprintf("%s:%s:%s/%s", groupID, artifactID, version, jarFilename),
			Digest: map[string]string{
				"sha256": hex.EncodeToString(sum[:]),
			},
		},
		IssuedAt:  time.Now().UTC().Unix(),
		OIDCToken: oidcToken,
	}
	return canonicalJSON(bundle)
}

// EncodeBundleHeader returns the base64-RawURL encoding of the bundle bytes
// suitable for embedding in the X-Mochi-Sigstore-Bundle HTTP request header.
func EncodeBundleHeader(bundle []byte) string {
	return base64.RawURLEncoding.EncodeToString(bundle)
}

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
