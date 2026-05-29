package publish_test

import (
	"encoding/base64"
	"encoding/json"
	"strings"
	"testing"

	"mochi/package3/java/publish"
)

var fakeJAR = []byte{0x50, 0x4B, 0x05, 0x06, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

func TestSignBundle_ValidInputs(t *testing.T) {
	b, err := publish.SignBundle("com.google.guava", "guava", "33.0.0-jre", fakeJAR, "oidc-token-xyz")
	if err != nil {
		t.Fatalf("SignBundle: %v", err)
	}
	if len(b) == 0 {
		t.Fatal("SignBundle returned empty bytes")
	}
	var bundle publish.SigstoreBundle
	if err := json.Unmarshal(b, &bundle); err != nil {
		t.Fatalf("SignBundle output is not valid JSON: %v", err)
	}
	if bundle.MediaType != "application/vnd.dev.sigstore.bundle.v0.3+json" {
		t.Errorf("MediaType = %q; want sigstore bundle v0.3", bundle.MediaType)
	}
	if bundle.OIDCToken != "oidc-token-xyz" {
		t.Errorf("OIDCToken = %q; want oidc-token-xyz", bundle.OIDCToken)
	}
	if bundle.Subject.Digest["sha256"] == "" {
		t.Error("Subject.Digest[sha256] is empty")
	}
	if !strings.Contains(bundle.Subject.Name, "guava") {
		t.Errorf("Subject.Name %q does not mention guava", bundle.Subject.Name)
	}
}

func TestSignBundle_EmptyGroup(t *testing.T) {
	_, err := publish.SignBundle("", "guava", "1.0", fakeJAR, "tok")
	if err == nil {
		t.Error("expected error for empty groupID")
	}
}

func TestSignBundle_EmptyJAR(t *testing.T) {
	_, err := publish.SignBundle("com.example", "lib", "1.0", nil, "tok")
	if err == nil {
		t.Error("expected error for empty jarBytes")
	}
}

func TestSignBundle_EmptyOIDCToken(t *testing.T) {
	_, err := publish.SignBundle("com.example", "lib", "1.0", fakeJAR, "")
	if err == nil {
		t.Error("expected error for empty oidcToken")
	}
}

func TestSignBundle_Deterministic(t *testing.T) {
	// Two calls with same inputs (modulo time) should have same JSON shape.
	// We cannot check IssuedAt for equality but we can check all other fields.
	b1, err := publish.SignBundle("com.example", "lib", "1.0", fakeJAR, "tok")
	if err != nil {
		t.Fatalf("first SignBundle: %v", err)
	}
	b2, err := publish.SignBundle("com.example", "lib", "1.0", fakeJAR, "tok")
	if err != nil {
		t.Fatalf("second SignBundle: %v", err)
	}
	var v1, v2 map[string]any
	json.Unmarshal(b1, &v1)
	json.Unmarshal(b2, &v2)
	// All fields except issuedAt must be equal.
	for _, key := range []string{"mediaType", "predicateType", "oidcToken"} {
		if v1[key] != v2[key] {
			t.Errorf("field %q differs between runs: %v vs %v", key, v1[key], v2[key])
		}
	}
}

func TestEncodeBundleHeader_Base64(t *testing.T) {
	data := []byte("hello bundle")
	got := publish.EncodeBundleHeader(data)
	want := base64.RawURLEncoding.EncodeToString(data)
	if got != want {
		t.Errorf("EncodeBundleHeader = %q; want %q", got, want)
	}
}
