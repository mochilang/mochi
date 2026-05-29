package publish

import (
	"bytes"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"strings"
	"testing"
)

func TestSignBundleStableOutput(t *testing.T) {
	a, err := SignBundle("demo", "1.0.0", []byte("crate-bytes"), "jwt.payload.sig")
	if err != nil {
		t.Fatal(err)
	}
	b, err := SignBundle("demo", "1.0.0", []byte("crate-bytes"), "jwt.payload.sig")
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(a, b) {
		t.Error("SignBundle should produce byte-stable output")
	}
}

func TestSignBundleCarriesSHA256OfCrate(t *testing.T) {
	crate := []byte("imagine this is a tarball")
	bundle, err := SignBundle("demo", "1.0.0", crate, "token")
	if err != nil {
		t.Fatal(err)
	}
	var parsed SigstoreBundle
	if err := json.Unmarshal(bundle, &parsed); err != nil {
		t.Fatal(err)
	}
	want := sha256.Sum256(crate)
	got := parsed.Subject.Digest["sha256"]
	if got != hex.EncodeToString(want[:]) {
		t.Errorf("digest mismatch: got %q want %q", got, hex.EncodeToString(want[:]))
	}
}

func TestSignBundleSubjectNameFormat(t *testing.T) {
	bundle, err := SignBundle("hex", "0.4.3", []byte("c"), "tok")
	if err != nil {
		t.Fatal(err)
	}
	var parsed SigstoreBundle
	_ = json.Unmarshal(bundle, &parsed)
	if parsed.Subject.Name != "hex-0.4.3.crate" {
		t.Errorf("got name %q want %q", parsed.Subject.Name, "hex-0.4.3.crate")
	}
}

func TestSignBundleEmbedsOIDCToken(t *testing.T) {
	bundle, err := SignBundle("demo", "1.0.0", []byte("c"), "the-jwt")
	if err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(string(bundle), `"the-jwt"`) {
		t.Errorf("bundle should contain OIDC token: %s", bundle)
	}
}

func TestSignBundleRejectsEmptyInputs(t *testing.T) {
	cases := []struct {
		name        string
		crate, ver  string
		crateBytes  []byte
		token       string
		expectError bool
	}{
		{"empty crate", "", "1.0.0", []byte("x"), "t", true},
		{"empty version", "demo", "", []byte("x"), "t", true},
		{"empty bytes", "demo", "1.0.0", nil, "t", true},
		{"empty token", "demo", "1.0.0", []byte("x"), "", true},
		{"all valid", "demo", "1.0.0", []byte("x"), "t", false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			_, err := SignBundle(c.crate, c.ver, c.crateBytes, c.token)
			if (err != nil) != c.expectError {
				t.Errorf("expectError=%v but got err=%v", c.expectError, err)
			}
		})
	}
}

func TestSignBundleMediaTypeIsSigstoreV03(t *testing.T) {
	bundle, _ := SignBundle("demo", "1.0.0", []byte("c"), "t")
	var parsed SigstoreBundle
	_ = json.Unmarshal(bundle, &parsed)
	want := "application/vnd.dev.sigstore.bundle.v0.3+json"
	if parsed.MediaType != want {
		t.Errorf("got media type %q want %q", parsed.MediaType, want)
	}
}

func TestCanonicalJSONSortsKeys(t *testing.T) {
	input := map[string]any{
		"z": 1,
		"a": 2,
		"m": map[string]any{
			"y": 3,
			"x": 4,
		},
	}
	out, err := canonicalJSON(input)
	if err != nil {
		t.Fatal(err)
	}
	// expect "a" before "m" before "z" and "x" before "y" inside m
	idxA := strings.Index(string(out), `"a"`)
	idxM := strings.Index(string(out), `"m"`)
	idxZ := strings.Index(string(out), `"z"`)
	idxX := strings.Index(string(out), `"x"`)
	idxY := strings.Index(string(out), `"y"`)
	if !(idxA < idxM && idxM < idxZ) {
		t.Errorf("top-level keys not sorted: %s", out)
	}
	if !(idxX < idxY) {
		t.Errorf("nested keys not sorted: %s", out)
	}
}

func TestEncodeBundleHeader(t *testing.T) {
	got := encodeBundleHeader([]byte("hello"))
	want := base64.RawURLEncoding.EncodeToString([]byte("hello"))
	if got != want {
		t.Errorf("got %q want %q", got, want)
	}
}
