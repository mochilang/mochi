package attest

// Reason is the machine-readable code the verifier attaches to every
// Violation so CLI output + telemetry can switch on cause without
// pattern-matching free-form messages.
type Reason string

const (
	ReasonMissingAttestation     Reason = "missing-attestation"
	ReasonParseFailure           Reason = "parse-failure"
	ReasonUnsupportedBundleType  Reason = "unsupported-bundle-type"
	ReasonPayloadTypeMismatch    Reason = "payload-type-mismatch"
	ReasonStatementTypeMismatch  Reason = "statement-type-mismatch"
	ReasonPredicateTypeMismatch  Reason = "predicate-type-mismatch"
	ReasonSubjectMissing         Reason = "subject-missing"
	ReasonDigestMismatch         Reason = "digest-mismatch"
	ReasonBuilderRejected        Reason = "builder-rejected"
	ReasonPublisherRejected      Reason = "publisher-rejected"
	ReasonSignatureNotVerified   Reason = "signature-not-verified"
)

// Violation is one structured rejection emitted by the verifier.
type Violation struct {
	Reason  Reason
	Message string
}

// Report is the per-wheel verification outcome the install
// orchestrator consumes.
type Report struct {
	WheelURL    string
	Distribution string
	Version     string
	BuilderID   string
	Identity    string
	Subject     *Subject
	Violations  []Violation
}

// OK is true iff Violations is empty. The orchestrator gates the
// install on this field when Policy.Required is set.
func (r *Report) OK() bool { return r != nil && len(r.Violations) == 0 }

func (r *Report) addViolation(reason Reason, message string) {
	r.Violations = append(r.Violations, Violation{Reason: reason, Message: message})
}
