package attest

import (
	"strings"
	"testing"
)

func validStatementJSON() string {
	return `{
  "_type": "https://in-toto.io/Statement/v1",
  "predicateType": "https://slsa.dev/provenance/v1",
  "subject": [
    {"name": "demo-1.0-py3-none-any.whl", "digest": {"sha256": "deadbeef"}}
  ],
  "predicate": {
    "buildDefinition": {"buildType": "https://example/build"},
    "runDetails": {"builder": {"id": "https://example/builder@v1"}}
  }
}`
}

func TestParseStatementHappy(t *testing.T) {
	s, err := ParseStatement([]byte(validStatementJSON()))
	if err != nil {
		t.Fatalf("ParseStatement: %v", err)
	}
	if s.Type != StatementTypeV1 {
		t.Errorf("Type = %q, want %q", s.Type, StatementTypeV1)
	}
	if s.PredicateType != PredicateTypeSLSAV1 {
		t.Errorf("PredicateType = %q, want %q", s.PredicateType, PredicateTypeSLSAV1)
	}
	if len(s.Subject) != 1 {
		t.Fatalf("subjects = %d, want 1", len(s.Subject))
	}
	if got := s.Subject[0].Digest["sha256"]; got != "deadbeef" {
		t.Errorf("digest sha256 = %q", got)
	}
}

func TestParseStatementErrors(t *testing.T) {
	cases := []struct {
		name string
		body string
		want string
	}{
		{"missing-type", `{"predicateType": "x", "subject": [{"name":"a","digest":{"sha256":"d"}}]}`, "missing _type"},
		{"missing-predicate-type", `{"_type": "x", "subject": [{"name":"a","digest":{"sha256":"d"}}]}`, "missing predicateType"},
		{"no-subjects", `{"_type": "x", "predicateType": "y", "subject": []}`, "no subjects"},
		{"subject-no-name", `{"_type": "x", "predicateType": "y", "subject": [{"digest":{"sha256":"d"}}]}`, "missing name"},
		{"subject-no-sha256", `{"_type": "x", "predicateType": "y", "subject": [{"name":"a","digest":{}}]}`, "missing sha256"},
		{"invalid-json", `not-json`, "decode statement"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			_, err := ParseStatement([]byte(tc.body))
			if err == nil {
				t.Fatal("expected error")
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Errorf("error %q does not contain %q", err.Error(), tc.want)
			}
		})
	}
}

func TestStatementBuilderID(t *testing.T) {
	s, err := ParseStatement([]byte(validStatementJSON()))
	if err != nil {
		t.Fatal(err)
	}
	if got := s.BuilderID(); got != "https://example/builder@v1" {
		t.Errorf("BuilderID = %q", got)
	}

	// nil safety
	var nilStmt *Statement
	if got := nilStmt.BuilderID(); got != "" {
		t.Errorf("nil BuilderID = %q, want empty", got)
	}

	// missing runDetails
	bare := &Statement{Predicate: map[string]any{}}
	if got := bare.BuilderID(); got != "" {
		t.Errorf("bare BuilderID = %q", got)
	}

	// runDetails without builder
	noBuilder := &Statement{Predicate: map[string]any{"runDetails": map[string]any{}}}
	if got := noBuilder.BuilderID(); got != "" {
		t.Errorf("no-builder BuilderID = %q", got)
	}
}

func TestStatementSubjectByName(t *testing.T) {
	s, err := ParseStatement([]byte(validStatementJSON()))
	if err != nil {
		t.Fatal(err)
	}
	if got := s.SubjectByName("demo-1.0-py3-none-any.whl"); got == nil {
		t.Fatal("expected subject")
	} else if got.Digest["sha256"] != "deadbeef" {
		t.Errorf("digest = %q", got.Digest["sha256"])
	}
	if got := s.SubjectByName("missing.whl"); got != nil {
		t.Errorf("expected nil for missing name, got %+v", got)
	}

	var nilStmt *Statement
	if got := nilStmt.SubjectByName("x"); got != nil {
		t.Errorf("nil SubjectByName = %+v", got)
	}
}
