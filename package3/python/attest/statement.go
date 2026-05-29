package attest

import (
	"encoding/json"
	"fmt"
)

// The in-toto Statement v1 and SLSA Provenance v1 URIs the publisher
// emits and the verifier must accept. Mirrored from publish/attestation.go
// so the install-side import graph stays self-contained.
const (
	StatementTypeV1      = "https://in-toto.io/Statement/v1"
	PredicateTypeSLSAV1  = "https://slsa.dev/provenance/v1"
)

// Subject is one (name, digest) pair the statement covers.
type Subject struct {
	Name   string            `json:"name"`
	Digest map[string]string `json:"digest"`
}

// Statement is the parsed in-toto Statement v1 envelope. Predicate is
// kept as a raw map so the verifier can pull SLSA-specific fields
// without typing every other predicate the wild PEP 740 ecosystem may
// eventually use.
type Statement struct {
	Type          string         `json:"_type"`
	PredicateType string         `json:"predicateType"`
	Subject       []Subject      `json:"subject"`
	Predicate     map[string]any `json:"predicate"`
}

// ParseStatement decodes the in-toto JSON envelope. It is strict
// about top-level shape: missing _type or predicateType, or zero
// subjects, are rejected before any per-field checks.
func ParseStatement(raw []byte) (*Statement, error) {
	var s Statement
	if err := json.Unmarshal(raw, &s); err != nil {
		return nil, fmt.Errorf("attest: decode statement: %w", err)
	}
	if s.Type == "" {
		return nil, fmt.Errorf("attest: statement missing _type")
	}
	if s.PredicateType == "" {
		return nil, fmt.Errorf("attest: statement missing predicateType")
	}
	if len(s.Subject) == 0 {
		return nil, fmt.Errorf("attest: statement has no subjects")
	}
	for i, sub := range s.Subject {
		if sub.Name == "" {
			return nil, fmt.Errorf("attest: statement subject %d missing name", i)
		}
		if sub.Digest["sha256"] == "" {
			return nil, fmt.Errorf("attest: statement subject %q missing sha256 digest", sub.Name)
		}
	}
	return &s, nil
}

// BuilderID returns the builder.id field from a SLSA Provenance v1
// predicate, or "" when the predicate does not carry one. The
// path is `runDetails.builder.id` per SLSA v1 §3.
func (s *Statement) BuilderID() string {
	if s == nil {
		return ""
	}
	rd, ok := s.Predicate["runDetails"].(map[string]any)
	if !ok {
		return ""
	}
	b, ok := rd["builder"].(map[string]any)
	if !ok {
		return ""
	}
	id, _ := b["id"].(string)
	return id
}

// SubjectByName returns the subject whose name matches, or nil when
// the statement does not cover the wheel.
func (s *Statement) SubjectByName(name string) *Subject {
	if s == nil {
		return nil
	}
	for i := range s.Subject {
		if s.Subject[i].Name == name {
			return &s.Subject[i]
		}
	}
	return nil
}
