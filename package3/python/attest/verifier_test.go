package attest

import (
	"context"
	"encoding/json"
	"errors"
	"strings"
	"testing"
)

func bundleBytes(t *testing.T, b *Bundle) []byte {
	t.Helper()
	out, err := json.Marshal(b)
	if err != nil {
		t.Fatalf("marshal bundle: %v", err)
	}
	return out
}

func TestVerifierHappy(t *testing.T) {
	raw := bundleBytes(t, okBundle(t, okStatement(t, targetFilename, targetSHA)))
	v := Verifier{Policy: DefaultPolicy(), Fetcher: StaticFetcher{Bundle: raw}}
	r, err := v.Verify(context.Background(), okTarget())
	if err != nil {
		t.Fatalf("Verify: %v", err)
	}
	if !r.OK() {
		t.Errorf("expected OK, got %+v", r.Violations)
	}
}

func TestVerifierMissingBundleNotRequired(t *testing.T) {
	v := Verifier{Policy: DefaultPolicy(), Fetcher: StaticFetcher{}}
	r, err := v.Verify(context.Background(), okTarget())
	if err != nil {
		t.Errorf("expected nil error when not Required, got %v", err)
	}
	if !hasReason(r, ReasonMissingAttestation) {
		t.Errorf("expected ReasonMissingAttestation, got %+v", r.Violations)
	}
}

func TestVerifierMissingBundleRequired(t *testing.T) {
	p := DefaultPolicy()
	p.Required = true
	v := Verifier{Policy: p, Fetcher: StaticFetcher{}}
	_, err := v.Verify(context.Background(), okTarget())
	if err == nil {
		t.Fatal("expected error")
	}
}

func TestVerifierBadDigestRequired(t *testing.T) {
	raw := bundleBytes(t, okBundle(t, okStatement(t, targetFilename, "cafef00d")))
	p := DefaultPolicy()
	p.Required = true
	v := Verifier{Policy: p, Fetcher: StaticFetcher{Bundle: raw}}
	r, err := v.Verify(context.Background(), okTarget())
	if err == nil {
		t.Fatal("expected error")
	}
	if !hasReason(r, ReasonDigestMismatch) {
		t.Errorf("expected ReasonDigestMismatch, got %+v", r.Violations)
	}
}

func TestVerifierBundleParseError(t *testing.T) {
	v := Verifier{Policy: DefaultPolicy(), Fetcher: StaticFetcher{Bundle: []byte(`{"mediaType":"x"}`)}}
	r, err := v.Verify(context.Background(), okTarget())
	if err != nil {
		t.Errorf("expected nil error (not Required), got %v", err)
	}
	if !hasReason(r, ReasonParseFailure) {
		t.Errorf("expected ReasonParseFailure, got %+v", r.Violations)
	}
}

func TestVerifierBundleParseErrorRequired(t *testing.T) {
	p := DefaultPolicy()
	p.Required = true
	v := Verifier{Policy: p, Fetcher: StaticFetcher{Bundle: []byte(`{"mediaType":"x"}`)}}
	_, err := v.Verify(context.Background(), okTarget())
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "attest:") {
		t.Errorf("error = %q", err.Error())
	}
}

type errFetcher struct{ err error }

func (e errFetcher) Fetch(context.Context, WheelTarget) ([]byte, error) { return nil, e.err }

func TestVerifierFetchErrorNotRequired(t *testing.T) {
	v := Verifier{Policy: DefaultPolicy(), Fetcher: errFetcher{err: errors.New("network down")}}
	r, err := v.Verify(context.Background(), okTarget())
	if err != nil {
		t.Errorf("expected nil error when not Required, got %v", err)
	}
	if !hasReason(r, ReasonParseFailure) {
		t.Errorf("expected ReasonParseFailure for fetch error, got %+v", r.Violations)
	}
}

func TestVerifierFetchErrorRequired(t *testing.T) {
	p := DefaultPolicy()
	p.Required = true
	v := Verifier{Policy: p, Fetcher: errFetcher{err: errors.New("network down")}}
	_, err := v.Verify(context.Background(), okTarget())
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "network down") {
		t.Errorf("error = %q", err.Error())
	}
}

func TestVerifierNoFetcher(t *testing.T) {
	var v Verifier
	_, err := v.Verify(context.Background(), okTarget())
	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "missing Fetcher") {
		t.Errorf("error = %q", err.Error())
	}
}
