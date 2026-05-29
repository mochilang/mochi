package sumdb

import (
	"crypto/ed25519"
	"crypto/rand"
	"encoding/base64"
	"strings"
	"testing"
)

func TestParseSumGolangOrgKey(t *testing.T) {
	k, err := ParseVerifierKey(SumGolangOrgVerifierKey)
	if err != nil {
		t.Fatalf("ParseVerifierKey: %v", err)
	}
	if k.Name != "sum.golang.org" {
		t.Errorf("Name = %q; want sum.golang.org", k.Name)
	}
	if k.Algorithm != AlgEd25519 {
		t.Errorf("Algorithm = 0x%02x; want 0x%02x", k.Algorithm, AlgEd25519)
	}
	if len(k.PublicKey) != ed25519.PublicKeySize {
		t.Errorf("PublicKey len = %d; want %d", len(k.PublicKey), ed25519.PublicKeySize)
	}
	// String round-trip yields the same blob (up to canonicalisation).
	if got := k.String(); got != SumGolangOrgVerifierKey {
		t.Errorf("String round-trip mismatch:\n  got  %q\n  want %q", got, SumGolangOrgVerifierKey)
	}
}

func TestParseVerifierKeyMalformed(t *testing.T) {
	cases := []string{
		"",
		"name-only",
		"name+nothash+",
		"name+12345678",                   // missing key part
		"name+12345678+!!!!notbase64!!!!", // bad base64
		"name+1234567+" + base64.StdEncoding.EncodeToString(append([]byte{AlgEd25519}, make([]byte, ed25519.PublicKeySize)...)), // hash field 7 chars
		"name+12345678+" + base64.StdEncoding.EncodeToString([]byte{0x02, 0x00}),                                                // unsupported alg
		"name+12345678+" + base64.StdEncoding.EncodeToString([]byte{AlgEd25519}),                                                // empty pubkey
	}
	for _, tc := range cases {
		if _, err := ParseVerifierKey(tc); err == nil {
			t.Errorf("ParseVerifierKey(%q) returned nil; want error", tc)
		}
	}
}

func TestComputeKeyHashStable(t *testing.T) {
	// The sum.golang.org key has prefix 033de0ae per its blob; if
	// computeKeyHash drifts that round-trip will fail.
	k, err := ParseVerifierKey(SumGolangOrgVerifierKey)
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if k.KeyHash[0] != 0x03 || k.KeyHash[1] != 0x3d || k.KeyHash[2] != 0xe0 || k.KeyHash[3] != 0xae {
		t.Errorf("KeyHash = %02x%02x%02x%02x; want 033de0ae",
			k.KeyHash[0], k.KeyHash[1], k.KeyHash[2], k.KeyHash[3])
	}
}

func TestParseVerifierKeyRejectsKeyHashMismatch(t *testing.T) {
	pub, _, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatalf("GenerateKey: %v", err)
	}
	body := append([]byte{AlgEd25519}, pub...)
	// Wrong key-hash prefix on purpose.
	blob := "test.example+deadbeef+" + base64.StdEncoding.EncodeToString(body)
	_, err = ParseVerifierKey(blob)
	if err == nil {
		t.Errorf("ParseVerifierKey accepted mismatched key-hash; want error")
	}
	if !strings.Contains(err.Error(), "key-hash mismatch") {
		t.Errorf("error %q does not mention key-hash mismatch", err)
	}
}
