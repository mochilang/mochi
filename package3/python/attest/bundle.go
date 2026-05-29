package attest

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
)

// PEP 740 / Sigstore Bundle payload + envelope shapes. The bridge
// only reads the subset PyPI emits: mediaType identifies the bundle
// version; dsseEnvelope carries the DSSE-wrapped statement;
// verificationMaterial carries the X.509 chain + Rekor inclusion
// proof that sub-phase 15.1's crypto verifier consumes.
const (
	BundleMediaTypeV2          = "application/vnd.dev.sigstore.bundle.v0.3+json"
	DSSEPayloadTypeInTotoJSON  = "application/vnd.in-toto+json"
)

// Bundle is the parsed PEP 740 attestation envelope.
type Bundle struct {
	MediaType            string                `json:"mediaType"`
	DsseEnvelope         DSSEEnvelope          `json:"dsseEnvelope"`
	VerificationMaterial VerificationMaterial  `json:"verificationMaterial"`
}

// DSSEEnvelope wraps the in-toto statement payload + the signatures
// over it. Payload is base64-std encoded (RFC 4648 §4 with padding).
type DSSEEnvelope struct {
	Payload     string      `json:"payload"`
	PayloadType string      `json:"payloadType"`
	Signatures  []Signature `json:"signatures"`
}

// Signature is one DSSE signature line.
type Signature struct {
	Sig   string `json:"sig"`
	Keyid string `json:"keyid,omitempty"`
}

// VerificationMaterial collects the Sigstore-specific material the
// crypto verifier needs: the certificate chain (rooted at Fulcio) +
// the Rekor transparency-log inclusion proof.
type VerificationMaterial struct {
	Certificate Certificate  `json:"x509CertificateChain,omitempty"`
	TLogEntries []TLogEntry  `json:"tlogEntries,omitempty"`
}

// Certificate is the X.509 chain. RawBytes per cert is base64-DER.
type Certificate struct {
	Certificates []EncodedCert `json:"certificates"`
}

// EncodedCert is one DER-encoded X.509 certificate, base64-std.
type EncodedCert struct {
	RawBytes string `json:"rawBytes"`
}

// TLogEntry is one Rekor inclusion-proof entry. The verifier checks
// LogID + KindVersion structurally; the actual inclusion proof
// (logIndex, inclusionPromise, signed entry timestamp) is sub-phase
// 15.1.
type TLogEntry struct {
	LogIndex    string      `json:"logIndex"`
	LogID       LogID       `json:"logId"`
	KindVersion KindVersion `json:"kindVersion"`
}

// LogID is the Rekor instance public-key identifier.
type LogID struct {
	KeyID string `json:"keyId"`
}

// KindVersion names the entry-format the proof was emitted under
// (e.g. kind=intoto, version=0.0.2).
type KindVersion struct {
	Kind    string `json:"kind"`
	Version string `json:"version"`
}

// ParseBundle decodes a JSON-encoded PEP 740 attestation envelope.
// The decoder is intentionally lenient about MediaType (we accept
// any string and validate it in policy.Verify so future bundle
// versions can be enforced via Policy.AcceptedBundleTypes without
// reaching back into the parser).
func ParseBundle(raw []byte) (*Bundle, error) {
	var b Bundle
	if err := json.Unmarshal(raw, &b); err != nil {
		return nil, fmt.Errorf("attest: decode bundle: %w", err)
	}
	if b.DsseEnvelope.Payload == "" {
		return nil, fmt.Errorf("attest: bundle missing dsseEnvelope.payload")
	}
	if b.DsseEnvelope.PayloadType == "" {
		return nil, fmt.Errorf("attest: bundle missing dsseEnvelope.payloadType")
	}
	if len(b.DsseEnvelope.Signatures) == 0 {
		return nil, fmt.Errorf("attest: bundle has no signatures")
	}
	return &b, nil
}

// Statement decodes the base64-std payload and parses it as an
// in-toto Statement. ParseBundle's structural checks already
// guaranteed Payload is non-empty.
func (b *Bundle) Statement() (*Statement, error) {
	if b == nil {
		return nil, fmt.Errorf("attest: nil bundle")
	}
	if b.DsseEnvelope.PayloadType != DSSEPayloadTypeInTotoJSON {
		return nil, fmt.Errorf("attest: payloadType %q != %q", b.DsseEnvelope.PayloadType, DSSEPayloadTypeInTotoJSON)
	}
	raw, err := base64.StdEncoding.DecodeString(b.DsseEnvelope.Payload)
	if err != nil {
		return nil, fmt.Errorf("attest: decode payload base64: %w", err)
	}
	return ParseStatement(raw)
}
