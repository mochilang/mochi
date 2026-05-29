package attest

import (
	"fmt"
	"sort"
)

// WheelTarget identifies the wheel the verifier is checking. URL is
// the resolver-decided download URL; Filename is the canonical
// PEP 491 filename (`<dist>-<ver>-...whl`); SHA256 is the lower-case
// hex digest the resolver committed to.
type WheelTarget struct {
	URL          string
	Filename     string
	Distribution string
	Version      string
	SHA256       string
}

// IdentityExtractor pulls the OIDC identity off a Bundle so the
// policy can match it against TrustedPublishers. Production wires the
// X.509 SAN parser; tests wire StaticIdentityExtractor.
type IdentityExtractor interface {
	Extract(b *Bundle) (string, error)
}

// StaticIdentityExtractor returns Identity verbatim; the test seam.
type StaticIdentityExtractor struct{ Identity string }

// Extract returns the static identity.
func (s StaticIdentityExtractor) Extract(*Bundle) (string, error) { return s.Identity, nil }

// Policy bundles the install-time verification knobs.
type Policy struct {
	Required             bool
	AcceptedBundleTypes  []string
	AcceptedPredicate    string
	AllowedBuilders      []string
	TrustedPublishers    []string
	IdentityExtractor    IdentityExtractor
}

// DefaultPolicy is the policy the install orchestrator uses when the
// user does not override (`--require-attestations` flips Required;
// `--allowed-builder` / `--trusted-publisher` append).
func DefaultPolicy() Policy {
	return Policy{
		AcceptedBundleTypes: []string{BundleMediaTypeV2},
		AcceptedPredicate:   PredicateTypeSLSAV1,
	}
}

// Verify walks the bundle against the policy, mutating report.
// Returns the report and a sentinel error when Policy.Required is set
// and the report ends up not-OK.
func (p Policy) Verify(bundle *Bundle, target WheelTarget) (*Report, error) {
	r := &Report{
		WheelURL:     target.URL,
		Distribution: target.Distribution,
		Version:      target.Version,
	}

	if bundle == nil {
		r.addViolation(ReasonMissingAttestation, "no attestation bundle supplied")
		return p.gate(r)
	}

	if len(p.AcceptedBundleTypes) > 0 && !contains(p.AcceptedBundleTypes, bundle.MediaType) {
		r.addViolation(ReasonUnsupportedBundleType,
			fmt.Sprintf("bundle mediaType %q is not in the accepted set %v", bundle.MediaType, p.AcceptedBundleTypes))
	}

	if bundle.DsseEnvelope.PayloadType != DSSEPayloadTypeInTotoJSON {
		r.addViolation(ReasonPayloadTypeMismatch,
			fmt.Sprintf("payloadType %q != %q", bundle.DsseEnvelope.PayloadType, DSSEPayloadTypeInTotoJSON))
		return p.gate(r)
	}

	stmt, err := bundle.Statement()
	if err != nil {
		r.addViolation(ReasonParseFailure, err.Error())
		return p.gate(r)
	}

	if stmt.Type != StatementTypeV1 {
		r.addViolation(ReasonStatementTypeMismatch,
			fmt.Sprintf("statement _type %q != %q", stmt.Type, StatementTypeV1))
	}

	wantPredicate := p.AcceptedPredicate
	if wantPredicate == "" {
		wantPredicate = PredicateTypeSLSAV1
	}
	if stmt.PredicateType != wantPredicate {
		r.addViolation(ReasonPredicateTypeMismatch,
			fmt.Sprintf("predicateType %q != %q", stmt.PredicateType, wantPredicate))
	}

	subject := stmt.SubjectByName(target.Filename)
	if subject == nil {
		r.addViolation(ReasonSubjectMissing,
			fmt.Sprintf("no subject named %q in attestation", target.Filename))
	} else {
		r.Subject = subject
		if got := subject.Digest["sha256"]; got != target.SHA256 {
			r.addViolation(ReasonDigestMismatch,
				fmt.Sprintf("subject sha256 %q != target sha256 %q", got, target.SHA256))
		}
	}

	builderID := stmt.BuilderID()
	r.BuilderID = builderID
	if len(p.AllowedBuilders) > 0 && !contains(p.AllowedBuilders, builderID) {
		r.addViolation(ReasonBuilderRejected,
			fmt.Sprintf("builder.id %q is not in the allow-list %v", builderID, sorted(p.AllowedBuilders)))
	}

	if len(p.TrustedPublishers) > 0 {
		if p.IdentityExtractor == nil {
			r.addViolation(ReasonSignatureNotVerified,
				"TrustedPublishers configured but no IdentityExtractor wired")
		} else {
			identity, err := p.IdentityExtractor.Extract(bundle)
			if err != nil {
				r.addViolation(ReasonSignatureNotVerified, err.Error())
			} else {
				r.Identity = identity
				if !contains(p.TrustedPublishers, identity) {
					r.addViolation(ReasonPublisherRejected,
						fmt.Sprintf("identity %q is not in the trusted set %v", identity, sorted(p.TrustedPublishers)))
				}
			}
		}
	}

	return p.gate(r)
}

func (p Policy) gate(r *Report) (*Report, error) {
	if p.Required && !r.OK() {
		return r, fmt.Errorf("attest: install rejected: %d violation(s)", len(r.Violations))
	}
	return r, nil
}

func contains(set []string, s string) bool {
	for _, x := range set {
		if x == s {
			return true
		}
	}
	return false
}

func sorted(in []string) []string {
	out := append([]string(nil), in...)
	sort.Strings(out)
	return out
}
