package publish

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"mochi/package3/rust/library"
)

// TestPhase10TrustedPublishing is the MEP-73 Phase 10 sentinel. It
// asserts the trusted-publishing flow stitches together cleanly: the
// rendered library crate (Phase 9 output shape) passes Validate, the
// .crate tarball is byte-stable, the Sigstore bundle carries the
// crate's SHA-256, OIDC claims validate against the crates.io
// audience, and the upload reaches `/api/v1/crates/new` with a
// Bearer token + sigstore header on the request.
func TestPhase10TrustedPublishing(t *testing.T) {
	t.Run("end_to_end", func(t *testing.T) {
		files := library.Files{
			"Cargo.toml": "[package]\nname = \"demo\"\nversion = \"1.0.0\"\nedition = \"2021\"\n[lib]\ncrate-type = [\"rlib\", \"cdylib\"]\n",
			"src/lib.rs": "pub fn x() {}\n",
		}
		req := PublishRequest{
			CrateName: "demo",
			Version:   "1.0.0",
			Files:     files,
			OIDCToken: "header.payload.sig",
		}
		tr := &fakeTransport{retCode: 200}
		res, err := Publish(req, tr)
		if err != nil {
			t.Fatalf("end-to-end publish failed: %v", err)
		}
		if tr.calls != 1 {
			t.Errorf("expected exactly one transport call, got %d", tr.calls)
		}
		if !strings.HasSuffix(res.UploadedURL, "/api/v1/crates/new") {
			t.Errorf("UploadedURL should target /api/v1/crates/new: %q", res.UploadedURL)
		}
		if tr.lastHdr["Authorization"] != "Bearer "+req.OIDCToken {
			t.Errorf("expected Bearer auth header, got %q", tr.lastHdr["Authorization"])
		}
		if tr.lastHdr["X-Mochi-Sigstore-Bundle"] == "" {
			t.Error("expected sigstore bundle header to be set")
		}
	})

	t.Run("dry_run_produces_bundle_without_upload", func(t *testing.T) {
		req := validRequest()
		req.DryRun = true
		tr := &fakeTransport{}
		res, err := Publish(req, tr)
		if err != nil {
			t.Fatal(err)
		}
		if tr.calls != 0 {
			t.Errorf("dry-run should not call transport, got %d", tr.calls)
		}
		if len(res.SignedBundle) == 0 {
			t.Error("dry-run should still produce a Sigstore bundle")
		}
		if len(res.CrateBytes) == 0 {
			t.Error("dry-run should still produce crate bytes")
		}
	})

	t.Run("tarball_byte_stable", func(t *testing.T) {
		a, err := Tarball("demo", "1.0.0", sampleFiles())
		if err != nil {
			t.Fatal(err)
		}
		b, err := Tarball("demo", "1.0.0", sampleFiles())
		if err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(a, b) {
			t.Error("Tarball must be byte-stable across runs (SOURCE_DATE_EPOCH=0 model)")
		}
	})

	t.Run("oidc_validation_rejects_wrong_audience", func(t *testing.T) {
		c := sampleClaims()
		c.Audience = "npm-registry"
		if err := ValidateClaims(c, time.Now()); err == nil {
			t.Error("ValidateClaims should reject a token audience that is not crates.io")
		}
	})

	t.Run("oidc_validation_rejects_expired", func(t *testing.T) {
		c := sampleClaims()
		c.Expiry = time.Now().Add(-time.Hour).Unix()
		if err := ValidateClaims(c, time.Now()); err == nil {
			t.Error("ValidateClaims should reject an expired token")
		}
	})

	t.Run("sigstore_bundle_carries_crate_digest", func(t *testing.T) {
		crate, err := Tarball("demo", "1.0.0", sampleFiles())
		if err != nil {
			t.Fatal(err)
		}
		bundle, err := SignBundle("demo", "1.0.0", crate, "tok")
		if err != nil {
			t.Fatal(err)
		}
		var parsed SigstoreBundle
		if err := json.Unmarshal(bundle, &parsed); err != nil {
			t.Fatal(err)
		}
		if parsed.Subject.Digest["sha256"] == "" {
			t.Error("Sigstore bundle must carry the SHA-256 digest of the .crate tarball")
		}
		if parsed.Subject.Name != "demo-1.0.0.crate" {
			t.Errorf("Sigstore subject name = %q, want demo-1.0.0.crate", parsed.Subject.Name)
		}
	})
}
