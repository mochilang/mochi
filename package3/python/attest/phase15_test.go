package attest

import (
	"context"
	"encoding/json"
	"testing"
)

// TestPhase15Sentinel is the umbrella gate for Phase 15. The install
// orchestrator goal is "pull the PEP 740 bundle for a resolved wheel,
// validate Sigstore + SLSA fields, and gate when Required". The gate
// exercises the goal end-to-end through StaticFetcher so the test stays
// offline; sub-phases 15.1-15.3 wire live HTTP + sigstore-go crypto +
// CLI verbs.
func TestPhase15Sentinel(t *testing.T) {
	t.Run("ok-path", func(t *testing.T) {
		raw := mustMarshalBundle(t, okBundle(t, okStatement(t, targetFilename, targetSHA)))
		v := Verifier{Policy: DefaultPolicy(), Fetcher: StaticFetcher{Bundle: raw}}
		r, err := v.Verify(context.Background(), okTarget())
		if err != nil {
			t.Fatalf("Verify: %v", err)
		}
		if !r.OK() {
			t.Errorf("expected OK, violations %+v", r.Violations)
		}
		if r.BuilderID == "" {
			t.Error("expected BuilderID populated")
		}
	})

	t.Run("digest-mismatch-flagged", func(t *testing.T) {
		raw := mustMarshalBundle(t, okBundle(t, okStatement(t, targetFilename, "feedface")))
		v := Verifier{Policy: DefaultPolicy(), Fetcher: StaticFetcher{Bundle: raw}}
		r, err := v.Verify(context.Background(), okTarget())
		if err != nil {
			t.Errorf("expected nil err when not Required, got %v", err)
		}
		if !hasReason(r, ReasonDigestMismatch) {
			t.Errorf("expected ReasonDigestMismatch, got %+v", r.Violations)
		}
	})

	t.Run("required-gate-fails-install", func(t *testing.T) {
		p := DefaultPolicy()
		p.Required = true
		v := Verifier{Policy: p, Fetcher: StaticFetcher{}}
		_, err := v.Verify(context.Background(), okTarget())
		if err == nil {
			t.Fatal("expected gate error")
		}
	})

	t.Run("trusted-publisher-end-to-end", func(t *testing.T) {
		raw := mustMarshalBundle(t, okBundle(t, okStatement(t, targetFilename, targetSHA)))
		p := DefaultPolicy()
		p.TrustedPublishers = []string{"https://github.com/example/repo"}
		p.IdentityExtractor = StaticIdentityExtractor{Identity: "https://github.com/example/repo"}
		p.AllowedBuilders = []string{"https://github.com/pypi/publish@v1"}
		v := Verifier{Policy: p, Fetcher: StaticFetcher{Bundle: raw}}
		r, err := v.Verify(context.Background(), okTarget())
		if err != nil {
			t.Fatalf("Verify: %v", err)
		}
		if !r.OK() {
			t.Errorf("expected OK, violations %+v", r.Violations)
		}
		if r.Identity != "https://github.com/example/repo" {
			t.Errorf("Identity = %q", r.Identity)
		}
	})
}

func mustMarshalBundle(t *testing.T, b *Bundle) []byte {
	t.Helper()
	out, err := json.Marshal(b)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	return out
}
