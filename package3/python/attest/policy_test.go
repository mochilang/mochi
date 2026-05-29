package attest

import (
	"encoding/base64"
	"errors"
	"fmt"
	"testing"
)

const targetSHA = "deadbeef"
const targetFilename = "demo-1.0-py3-none-any.whl"

func okTarget() WheelTarget {
	return WheelTarget{
		URL:          "https://pypi.example/demo/" + targetFilename,
		Filename:     targetFilename,
		Distribution: "demo",
		Version:      "1.0",
		SHA256:       targetSHA,
	}
}

func okStatement(t *testing.T, name, sha string) string {
	t.Helper()
	return fmt.Sprintf(`{
  "_type": "https://in-toto.io/Statement/v1",
  "predicateType": "https://slsa.dev/provenance/v1",
  "subject": [{"name":"%s","digest":{"sha256":"%s"}}],
  "predicate": {"runDetails":{"builder":{"id":"https://github.com/pypi/publish@v1"}}}
}`, name, sha)
}

func okBundle(t *testing.T, statement string) *Bundle {
	t.Helper()
	return &Bundle{
		MediaType: BundleMediaTypeV2,
		DsseEnvelope: DSSEEnvelope{
			Payload:     base64.StdEncoding.EncodeToString([]byte(statement)),
			PayloadType: DSSEPayloadTypeInTotoJSON,
			Signatures:  []Signature{{Sig: "AAAA"}},
		},
	}
}

func TestDefaultPolicy(t *testing.T) {
	p := DefaultPolicy()
	if p.Required {
		t.Error("Required should default to false")
	}
	if len(p.AcceptedBundleTypes) != 1 || p.AcceptedBundleTypes[0] != BundleMediaTypeV2 {
		t.Errorf("AcceptedBundleTypes = %v", p.AcceptedBundleTypes)
	}
	if p.AcceptedPredicate != PredicateTypeSLSAV1 {
		t.Errorf("AcceptedPredicate = %q", p.AcceptedPredicate)
	}
}

func TestPolicyVerifyHappy(t *testing.T) {
	p := DefaultPolicy()
	bundle := okBundle(t, okStatement(t, targetFilename, targetSHA))
	r, err := p.Verify(bundle, okTarget())
	if err != nil {
		t.Fatalf("Verify: %v", err)
	}
	if !r.OK() {
		t.Errorf("expected OK, got violations: %+v", r.Violations)
	}
	if r.BuilderID != "https://github.com/pypi/publish@v1" {
		t.Errorf("BuilderID = %q", r.BuilderID)
	}
	if r.Subject == nil || r.Subject.Name != targetFilename {
		t.Errorf("Subject = %+v", r.Subject)
	}
}

func TestPolicyVerifyMissingBundleNotRequired(t *testing.T) {
	p := DefaultPolicy()
	r, err := p.Verify(nil, okTarget())
	if err != nil {
		t.Fatalf("Verify: %v", err)
	}
	if r.OK() {
		t.Error("expected violation")
	}
	if r.Violations[0].Reason != ReasonMissingAttestation {
		t.Errorf("reason = %q", r.Violations[0].Reason)
	}
}

func TestPolicyVerifyMissingBundleRequired(t *testing.T) {
	p := DefaultPolicy()
	p.Required = true
	r, err := p.Verify(nil, okTarget())
	if err == nil {
		t.Fatal("expected error when Required + missing")
	}
	if r.OK() {
		t.Error("expected violation")
	}
}

func TestPolicyVerifyUnsupportedMediaType(t *testing.T) {
	p := DefaultPolicy()
	bundle := okBundle(t, okStatement(t, targetFilename, targetSHA))
	bundle.MediaType = "application/unknown"
	r, _ := p.Verify(bundle, okTarget())
	if !hasReason(r, ReasonUnsupportedBundleType) {
		t.Errorf("expected ReasonUnsupportedBundleType, got %+v", r.Violations)
	}
}

func TestPolicyVerifyBadPayloadType(t *testing.T) {
	p := DefaultPolicy()
	bundle := okBundle(t, okStatement(t, targetFilename, targetSHA))
	bundle.DsseEnvelope.PayloadType = "application/json"
	r, _ := p.Verify(bundle, okTarget())
	if !hasReason(r, ReasonPayloadTypeMismatch) {
		t.Errorf("expected ReasonPayloadTypeMismatch, got %+v", r.Violations)
	}
}

func TestPolicyVerifyStatementParseFail(t *testing.T) {
	p := DefaultPolicy()
	bundle := okBundle(t, "not-json")
	r, _ := p.Verify(bundle, okTarget())
	if !hasReason(r, ReasonParseFailure) {
		t.Errorf("expected ReasonParseFailure, got %+v", r.Violations)
	}
}

func TestPolicyVerifyWrongStatementType(t *testing.T) {
	stmt := `{
"_type":"https://wrong.example/Statement/v2",
"predicateType":"https://slsa.dev/provenance/v1",
"subject":[{"name":"` + targetFilename + `","digest":{"sha256":"` + targetSHA + `"}}],
"predicate":{}}`
	p := DefaultPolicy()
	r, _ := p.Verify(okBundle(t, stmt), okTarget())
	if !hasReason(r, ReasonStatementTypeMismatch) {
		t.Errorf("expected ReasonStatementTypeMismatch, got %+v", r.Violations)
	}
}

func TestPolicyVerifyWrongPredicateType(t *testing.T) {
	stmt := `{
"_type":"https://in-toto.io/Statement/v1",
"predicateType":"https://slsa.dev/provenance/v0.1",
"subject":[{"name":"` + targetFilename + `","digest":{"sha256":"` + targetSHA + `"}}],
"predicate":{}}`
	p := DefaultPolicy()
	r, _ := p.Verify(okBundle(t, stmt), okTarget())
	if !hasReason(r, ReasonPredicateTypeMismatch) {
		t.Errorf("expected ReasonPredicateTypeMismatch, got %+v", r.Violations)
	}
}

func TestPolicyVerifySubjectMissing(t *testing.T) {
	stmt := okStatement(t, "other-1.0-py3-none-any.whl", targetSHA)
	p := DefaultPolicy()
	r, _ := p.Verify(okBundle(t, stmt), okTarget())
	if !hasReason(r, ReasonSubjectMissing) {
		t.Errorf("expected ReasonSubjectMissing, got %+v", r.Violations)
	}
}

func TestPolicyVerifyDigestMismatch(t *testing.T) {
	stmt := okStatement(t, targetFilename, "cafef00d")
	p := DefaultPolicy()
	r, _ := p.Verify(okBundle(t, stmt), okTarget())
	if !hasReason(r, ReasonDigestMismatch) {
		t.Errorf("expected ReasonDigestMismatch, got %+v", r.Violations)
	}
}

func TestPolicyVerifyBuilderRejected(t *testing.T) {
	p := DefaultPolicy()
	p.AllowedBuilders = []string{"https://github.com/another/builder@v1"}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !hasReason(r, ReasonBuilderRejected) {
		t.Errorf("expected ReasonBuilderRejected, got %+v", r.Violations)
	}
}

func TestPolicyVerifyBuilderAccepted(t *testing.T) {
	p := DefaultPolicy()
	p.AllowedBuilders = []string{"https://github.com/pypi/publish@v1"}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !r.OK() {
		t.Errorf("expected OK, got %+v", r.Violations)
	}
}

func TestPolicyVerifyTrustedPublisherMissingExtractor(t *testing.T) {
	p := DefaultPolicy()
	p.TrustedPublishers = []string{"https://github.com/example/repo"}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !hasReason(r, ReasonSignatureNotVerified) {
		t.Errorf("expected ReasonSignatureNotVerified, got %+v", r.Violations)
	}
}

func TestPolicyVerifyPublisherRejected(t *testing.T) {
	p := DefaultPolicy()
	p.TrustedPublishers = []string{"https://github.com/example/repo"}
	p.IdentityExtractor = StaticIdentityExtractor{Identity: "https://github.com/intruder/repo"}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !hasReason(r, ReasonPublisherRejected) {
		t.Errorf("expected ReasonPublisherRejected, got %+v", r.Violations)
	}
}

func TestPolicyVerifyPublisherAccepted(t *testing.T) {
	p := DefaultPolicy()
	p.TrustedPublishers = []string{"https://github.com/example/repo"}
	p.IdentityExtractor = StaticIdentityExtractor{Identity: "https://github.com/example/repo"}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !r.OK() {
		t.Errorf("expected OK, got %+v", r.Violations)
	}
	if r.Identity != "https://github.com/example/repo" {
		t.Errorf("Identity = %q", r.Identity)
	}
}

type errExtractor struct{}

func (errExtractor) Extract(*Bundle) (string, error) { return "", errors.New("kaboom") }

func TestPolicyVerifyIdentityExtractError(t *testing.T) {
	p := DefaultPolicy()
	p.TrustedPublishers = []string{"https://github.com/example/repo"}
	p.IdentityExtractor = errExtractor{}
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), okTarget())
	if !hasReason(r, ReasonSignatureNotVerified) {
		t.Errorf("expected ReasonSignatureNotVerified, got %+v", r.Violations)
	}
}

func TestPolicyVerifyRequiredGateError(t *testing.T) {
	p := DefaultPolicy()
	p.Required = true
	stmt := okStatement(t, targetFilename, "cafef00d")
	_, err := p.Verify(okBundle(t, stmt), okTarget())
	if err == nil {
		t.Fatal("expected gate error")
	}
}

func TestPolicyVerifyNotRequiredNoError(t *testing.T) {
	p := DefaultPolicy()
	stmt := okStatement(t, targetFilename, "cafef00d")
	_, err := p.Verify(okBundle(t, stmt), okTarget())
	if err != nil {
		t.Errorf("expected nil error when not Required, got %v", err)
	}
}

func TestPolicyVerifyPopulatesReportFromTarget(t *testing.T) {
	p := DefaultPolicy()
	tgt := okTarget()
	r, _ := p.Verify(okBundle(t, okStatement(t, targetFilename, targetSHA)), tgt)
	if r.WheelURL != tgt.URL {
		t.Errorf("WheelURL = %q", r.WheelURL)
	}
	if r.Distribution != tgt.Distribution {
		t.Errorf("Distribution = %q", r.Distribution)
	}
	if r.Version != tgt.Version {
		t.Errorf("Version = %q", r.Version)
	}
}

func hasReason(r *Report, want Reason) bool {
	if r == nil {
		return false
	}
	for _, v := range r.Violations {
		if v.Reason == want {
			return true
		}
	}
	return false
}

func TestSortedHelper(t *testing.T) {
	in := []string{"b", "a", "c"}
	out := sorted(in)
	if out[0] != "a" || out[1] != "b" || out[2] != "c" {
		t.Errorf("sorted = %v", out)
	}
	// must not mutate input
	if in[0] != "b" {
		t.Errorf("input mutated: %v", in)
	}
}

func TestContainsHelper(t *testing.T) {
	if !contains([]string{"a", "b"}, "a") {
		t.Error("contains miss")
	}
	if contains([]string{"a", "b"}, "c") {
		t.Error("contains false-hit")
	}
	if contains(nil, "a") {
		t.Error("contains nil set should miss")
	}
}
