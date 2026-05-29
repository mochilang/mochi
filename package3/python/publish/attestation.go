package publish

import (
	"encoding/json"
	"fmt"
	"sort"
	"time"
)

// AttestationPredicateType is the PEP 740 / SLSA Provenance v1 predicate
// type stamped into every Mochi-emitted attestation. The value is the
// canonical SLSA v1 identifier.
const AttestationPredicateType = "https://slsa.dev/provenance/v1"

// AttestationStatementType is the in-toto Statement v1 type uri.
const AttestationStatementType = "https://in-toto.io/Statement/v1"

// AttestationBuilderID identifies the Mochi build CLI as the builder. PyPI's
// `verify-attestations` policy keys policy decisions off this string.
const AttestationBuilderID = "https://mochi-lang.org/build/v1"

// Subject is one in-toto subject: a name + a digest map. PEP 740 mandates a
// sha256 digest for every published artifact.
type Subject struct {
	Name   string            `json:"name"`
	Digest map[string]string `json:"digest"`
}

// Statement is the PEP 740 in-toto v1 statement. The JSON form is what the
// Sigstore signer (Phase 11.1) wraps in the in-toto envelope.
type Statement struct {
	Type          string         `json:"_type"`
	PredicateType string         `json:"predicateType"`
	Subject       []Subject      `json:"subject"`
	Predicate     map[string]any `json:"predicate"`
}

// AttestationOptions controls fields that vary across calls.
type AttestationOptions struct {
	// BuilderID overrides AttestationBuilderID. Used by tests + by
	// downstream distros that re-host the Mochi build under their own
	// builder identity.
	BuilderID string
	// InvocationID is a stable per-publish identifier the verifier can
	// cross-reference against the CI log. When empty an opaque "auto"
	// marker is emitted; production callers should pass the CI run id.
	InvocationID string
	// BuildTime is stamped into the predicate. Defaults to UTC now when
	// zero.
	BuildTime time.Time
}

// BuildStatement constructs the in-toto statement for a set of targets. The
// statement is deterministic for a given (targets, opts) input so downstream
// signing is reproducible.
func BuildStatement(targets []PublishTarget, opts AttestationOptions) Statement {
	subjects := make([]Subject, 0, 2*len(targets))
	for _, t := range targets {
		subjects = append(subjects,
			Subject{
				Name:   distFilename(t.Distribution, t.Version, "sdist"),
				Digest: map[string]string{"sha256": t.SdistSHA256},
			},
			Subject{
				Name:   distFilename(t.Distribution, t.Version, "wheel"),
				Digest: map[string]string{"sha256": t.WheelSHA256},
			},
		)
	}
	sort.Slice(subjects, func(i, j int) bool { return subjects[i].Name < subjects[j].Name })
	builder := opts.BuilderID
	if builder == "" {
		builder = AttestationBuilderID
	}
	invocation := opts.InvocationID
	if invocation == "" {
		invocation = "auto"
	}
	bt := opts.BuildTime
	if bt.IsZero() {
		bt = time.Now().UTC()
	}
	return Statement{
		Type:          AttestationStatementType,
		PredicateType: AttestationPredicateType,
		Subject:       subjects,
		Predicate: map[string]any{
			"buildDefinition": map[string]any{
				"buildType":          "https://mochi-lang.org/build/pkg-publish/v1",
				"externalParameters": map[string]any{"invocationId": invocation},
			},
			"runDetails": map[string]any{
				"builder":  map[string]any{"id": builder},
				"metadata": map[string]any{"finishedOn": bt.UTC().Format(time.RFC3339)},
			},
		},
	}
}

// EncodeStatement returns the canonical JSON form of the statement. The output
// is sorted-key minified JSON; the Sigstore signer (Phase 11.1) signs this
// byte stream verbatim so the in-toto envelope payload matches what the
// verifier rehashes.
func EncodeStatement(s Statement) ([]byte, error) {
	return json.Marshal(s)
}

// distFilename returns the canonical filename PyPI lists the artifact under.
// PEP 491 + PEP 625 mandate `<dist>-<ver>.tar.gz` for sdists and
// `<dist>-<ver>-py3-none-any.whl` for pure-Python wheels.
func distFilename(dist, ver, kind string) string {
	switch kind {
	case "sdist":
		return fmt.Sprintf("%s-%s.tar.gz", dist, ver)
	case "wheel":
		return fmt.Sprintf("%s-%s-py3-none-any.whl", dist, ver)
	}
	return fmt.Sprintf("%s-%s.%s", dist, ver, kind)
}
