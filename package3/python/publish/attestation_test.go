package publish

import (
	"encoding/json"
	"strings"
	"testing"
	"time"
)

func sampleTargets() []PublishTarget {
	return []PublishTarget{validTarget(), {
		Distribution: "mochi-second",
		Version:      "0.0.1",
		SdistPath:    "/x.tar.gz",
		SdistSHA256:  "ffff",
		SdistSize:    10,
		WheelPath:    "/x.whl",
		WheelSHA256:  "eeee",
		WheelSize:    20,
	}}
}

func TestBuildStatementSubjectsSorted(t *testing.T) {
	stmt := BuildStatement(sampleTargets(), AttestationOptions{
		BuilderID:    "https://builder.test",
		InvocationID: "run-42",
		BuildTime:    time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	})
	if stmt.Type != AttestationStatementType {
		t.Fatalf("Type = %q", stmt.Type)
	}
	if stmt.PredicateType != AttestationPredicateType {
		t.Fatalf("PredicateType = %q", stmt.PredicateType)
	}
	if len(stmt.Subject) != 4 {
		t.Fatalf("expected 4 subjects, got %d", len(stmt.Subject))
	}
	for i := 1; i < len(stmt.Subject); i++ {
		if stmt.Subject[i-1].Name > stmt.Subject[i].Name {
			t.Fatalf("subjects not sorted: %v", stmt.Subject)
		}
	}
	for _, s := range stmt.Subject {
		if s.Digest["sha256"] == "" {
			t.Fatalf("subject %q missing sha256", s.Name)
		}
	}
}

func TestBuildStatementSubjectNames(t *testing.T) {
	stmt := BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{
		BuildTime: time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	})
	names := map[string]bool{}
	for _, s := range stmt.Subject {
		names[s.Name] = true
	}
	if !names["mochi-sample-0.1.0.tar.gz"] {
		t.Fatalf("missing sdist filename: %v", names)
	}
	if !names["mochi-sample-0.1.0-py3-none-any.whl"] {
		t.Fatalf("missing wheel filename: %v", names)
	}
}

func TestBuildStatementBuilderIDDefault(t *testing.T) {
	stmt := BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{
		BuildTime: time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	})
	runDetails, ok := stmt.Predicate["runDetails"].(map[string]any)
	if !ok {
		t.Fatalf("runDetails missing: %+v", stmt.Predicate)
	}
	builder, _ := runDetails["builder"].(map[string]any)
	if got := builder["id"]; got != AttestationBuilderID {
		t.Fatalf("builder id default = %v, want %s", got, AttestationBuilderID)
	}
}

func TestBuildStatementBuilderIDOverride(t *testing.T) {
	stmt := BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{
		BuilderID: "https://other.test",
		BuildTime: time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	})
	builder := stmt.Predicate["runDetails"].(map[string]any)["builder"].(map[string]any)
	if got := builder["id"]; got != "https://other.test" {
		t.Fatalf("builder id override = %v", got)
	}
}

func TestBuildStatementInvocationDefault(t *testing.T) {
	stmt := BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{
		BuildTime: time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	})
	bd := stmt.Predicate["buildDefinition"].(map[string]any)
	ep := bd["externalParameters"].(map[string]any)
	if got := ep["invocationId"]; got != "auto" {
		t.Fatalf("invocationId default = %v", got)
	}
}

func TestEncodeStatementDeterministic(t *testing.T) {
	opts := AttestationOptions{
		BuilderID:    "https://builder.test",
		InvocationID: "run-1",
		BuildTime:    time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	}
	a, _ := EncodeStatement(BuildStatement(sampleTargets(), opts))
	b, _ := EncodeStatement(BuildStatement(sampleTargets(), opts))
	if string(a) != string(b) {
		t.Fatalf("encoded statement non-deterministic")
	}
}

func TestEncodeStatementIsJSON(t *testing.T) {
	body, err := EncodeStatement(BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{
		BuildTime: time.Date(2026, 5, 30, 0, 0, 0, 0, time.UTC),
	}))
	if err != nil {
		t.Fatalf("EncodeStatement: %v", err)
	}
	var got map[string]any
	if err := json.Unmarshal(body, &got); err != nil {
		t.Fatalf("not JSON: %v", err)
	}
	if got["_type"] != AttestationStatementType {
		t.Fatalf("type missing: %+v", got)
	}
	if !strings.Contains(string(body), "buildDefinition") {
		t.Fatalf("predicate missing buildDefinition")
	}
}

func TestBuildStatementTimestampDefault(t *testing.T) {
	before := time.Now().UTC().Add(-time.Second)
	stmt := BuildStatement([]PublishTarget{validTarget()}, AttestationOptions{})
	after := time.Now().UTC().Add(time.Second)
	finishedOn := stmt.Predicate["runDetails"].(map[string]any)["metadata"].(map[string]any)["finishedOn"].(string)
	got, err := time.Parse(time.RFC3339, finishedOn)
	if err != nil {
		t.Fatalf("parse finishedOn: %v", err)
	}
	if got.Before(before) || got.After(after) {
		t.Fatalf("finishedOn %v outside window [%v, %v]", got, before, after)
	}
}
