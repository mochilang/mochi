package attest

import "testing"

func TestReportOK(t *testing.T) {
	var nilReport *Report
	if nilReport.OK() {
		t.Error("nil report should not be OK")
	}

	empty := &Report{}
	if !empty.OK() {
		t.Error("empty report should be OK")
	}

	r := &Report{}
	r.addViolation(ReasonDigestMismatch, "boom")
	if r.OK() {
		t.Error("report with violation should not be OK")
	}
	if len(r.Violations) != 1 {
		t.Errorf("violations = %d", len(r.Violations))
	}
	if r.Violations[0].Reason != ReasonDigestMismatch {
		t.Errorf("reason = %q", r.Violations[0].Reason)
	}
	if r.Violations[0].Message != "boom" {
		t.Errorf("message = %q", r.Violations[0].Message)
	}
}

func TestReasonStringStability(t *testing.T) {
	// The CLI + telemetry switch on these exact codes; protect against
	// accidental renames.
	want := map[Reason]string{
		ReasonMissingAttestation:    "missing-attestation",
		ReasonParseFailure:          "parse-failure",
		ReasonUnsupportedBundleType: "unsupported-bundle-type",
		ReasonPayloadTypeMismatch:   "payload-type-mismatch",
		ReasonStatementTypeMismatch: "statement-type-mismatch",
		ReasonPredicateTypeMismatch: "predicate-type-mismatch",
		ReasonSubjectMissing:        "subject-missing",
		ReasonDigestMismatch:        "digest-mismatch",
		ReasonBuilderRejected:       "builder-rejected",
		ReasonPublisherRejected:     "publisher-rejected",
		ReasonSignatureNotVerified:  "signature-not-verified",
	}
	for r, s := range want {
		if string(r) != s {
			t.Errorf("Reason value %q != %q", string(r), s)
		}
	}
}
