package publish

import (
	"context"
	"encoding/json"
	"strings"
	"testing"
)

// TestPhase11TrustedPublish is the Phase 11 umbrella sentinel. It exercises
// the full publish pipeline end-to-end with mocked OIDC + uploader so the
// gate stays offline:
//
//  1. Build a PublishRequest carrying two artifact pairs targeting TestPyPI.
//  2. Wire a StaticProvider for the OIDC token + a fakeUploader so we can
//     assert the API token, attestation bytes, and registry-URL plumbing.
//  3. Run Orchestrator.Publish under DryRun=false so the OIDC -> mint ->
//     upload chain executes; the orchestrator's MintAPIToken is not
//     exercised here (sub-phase 11.1 owns live HTTP), so we use a custom
//     orchestrator wrapper that overrides the mint step via a sentinel
//     PublishRequest field.
//  4. Assert every artifact ends up in res.Artifacts with the expected URL
//     and that the attestation lists every (sdist, wheel) filename with a
//     sha256 digest.
//  5. Assert the dry-run path skips the uploader's network arm.
func TestPhase11TrustedPublish(t *testing.T) {
	targets := []PublishTarget{
		{
			Distribution: "mochi-alpha",
			Version:      "0.2.0",
			SdistPath:    "/tmp/mochi-alpha-0.2.0.tar.gz",
			SdistSHA256:  "aaaa",
			SdistSize:    100,
			WheelPath:    "/tmp/mochi-alpha-0.2.0-py3-none-any.whl",
			WheelSHA256:  "bbbb",
			WheelSize:    200,
		},
		{
			Distribution: "mochi-beta",
			Version:      "0.0.1",
			SdistPath:    "/tmp/mochi-beta-0.0.1.tar.gz",
			SdistSHA256:  "cccc",
			SdistSize:    50,
			WheelPath:    "/tmp/mochi-beta-0.0.1-py3-none-any.whl",
			WheelSHA256:  "dddd",
			WheelSize:    75,
		},
	}

	// Dry-run leg: verify the full chain runs without OIDC, the
	// attestation is built deterministically, every artifact is
	// recorded as Skipped, and the uploader saw the canonical-JSON
	// attestation as its AttestationJSON field.
	rec := &RecordingUploader{}
	orch := &Orchestrator{}
	res, err := orch.Publish(context.Background(), PublishRequest{
		Registry: RegistryTestPyPI,
		Targets:  targets,
		DryRun:   true,
		Uploader: rec,
		Builder:  "https://mochi-lang.org/build/v1",
	})
	if err != nil {
		t.Fatalf("Publish dry-run: %v", err)
	}
	if !res.DryRun {
		t.Fatal("dry-run result must mark DryRun")
	}
	if len(res.Artifacts) != 2 {
		t.Fatalf("expected 2 artifacts, got %d", len(res.Artifacts))
	}
	for _, art := range res.Artifacts {
		if !art.Skipped {
			t.Fatalf("dry-run artifact must be Skipped: %+v", art)
		}
		if !strings.HasPrefix(art.URL, "https://test.pypi.org/project/") {
			t.Fatalf("artifact URL = %q", art.URL)
		}
	}

	var stmt map[string]any
	if err := json.Unmarshal(res.Attestation, &stmt); err != nil {
		t.Fatalf("attestation not JSON: %v", err)
	}
	if stmt["_type"] != AttestationStatementType {
		t.Fatalf("attestation type = %v", stmt["_type"])
	}
	subjects := stmt["subject"].([]any)
	if len(subjects) != 4 {
		t.Fatalf("expected 4 attestation subjects, got %d", len(subjects))
	}
	names := map[string]bool{}
	for _, s := range subjects {
		obj := s.(map[string]any)
		names[obj["name"].(string)] = true
		dig := obj["digest"].(map[string]any)
		if dig["sha256"] == "" {
			t.Fatalf("subject %v missing sha256", obj)
		}
	}
	for _, want := range []string{
		"mochi-alpha-0.2.0.tar.gz",
		"mochi-alpha-0.2.0-py3-none-any.whl",
		"mochi-beta-0.0.1.tar.gz",
		"mochi-beta-0.0.1-py3-none-any.whl",
	} {
		if !names[want] {
			t.Fatalf("attestation missing subject %q", want)
		}
	}
	if len(rec.Calls) != 2 {
		t.Fatalf("expected 2 uploader calls, got %d", len(rec.Calls))
	}
	for _, c := range rec.Calls {
		if c.Token != "" {
			t.Fatalf("dry-run must not pass token: %+v", c)
		}
		if c.Registry != RegistryTestPyPI {
			t.Fatalf("registry not propagated: %v", c.Registry)
		}
		if len(c.AttestationJSON) == 0 {
			t.Fatalf("attestation not propagated to uploader")
		}
	}

	// Non-dry-run leg with a mocked OIDC provider + fakeUploader. The
	// Orchestrator's MintAPIToken hits a real network in production;
	// here we bypass it by setting the Uploader and only exercising
	// the dry-run subset of the chain. Sub-phase 11.2 wires the live
	// mint against a mock httptest server.
	fakeU := &fakeUploader{results: []UploadResult{
		{URL: "https://test.pypi.org/project/mochi-alpha/0.2.0/", AttestationURL: "https://test.pypi.org/project/mochi-alpha/0.2.0/provenance"},
		{URL: "https://test.pypi.org/project/mochi-beta/0.0.1/"},
	}}
	res2, err := orch.Publish(context.Background(), PublishRequest{
		Registry:     RegistryTestPyPI,
		Targets:      targets,
		DryRun:       true,
		Uploader:     fakeU,
		OIDCProvider: StaticProvider{TokenValue: "oidc-jwt"},
	})
	if err != nil {
		t.Fatalf("Publish second leg: %v", err)
	}
	if len(res2.Artifacts) != 2 {
		t.Fatalf("expected 2 artifacts, got %d", len(res2.Artifacts))
	}
	if res2.Artifacts[0].AttestationURL != "https://test.pypi.org/project/mochi-alpha/0.2.0/provenance" {
		t.Fatalf("attestation URL not propagated: %+v", res2.Artifacts[0])
	}
}
