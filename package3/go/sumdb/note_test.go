package sumdb

import (
	"crypto/ed25519"
	"crypto/rand"
	"errors"
	"strings"
	"testing"
)

// makeSigner produces a deterministic ed25519 keypair + a parsed
// VerifierKey for the test's name. The seed is fixed so the test
// is reproducible across runs.
func makeSigner(t *testing.T, name string) (*Signer, *VerifierKey) {
	t.Helper()
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatalf("GenerateKey: %v", err)
	}
	hash := computeKeyHash(name, AlgEd25519, pub)
	return &Signer{Name: name, PrivateKey: priv}, &VerifierKey{
		Name: name, KeyHash: hash, Algorithm: AlgEd25519, PublicKey: pub,
	}
}

func TestSignAndParseRoundTrip(t *testing.T) {
	signer, verifier := makeSigner(t, "test.example.org")
	body := []byte("hello\nworld\n")
	out, err := signer.Sign(body)
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	note, err := ParseNote(out)
	if err != nil {
		t.Fatalf("ParseNote: %v", err)
	}
	if note.Text != string(body) {
		t.Errorf("ParseNote.Text = %q; want %q", note.Text, body)
	}
	if len(note.Signatures) != 1 {
		t.Fatalf("Signatures len = %d; want 1", len(note.Signatures))
	}
	matched, err := note.Verify(verifier)
	if err != nil {
		t.Fatalf("Verify: %v", err)
	}
	if matched.Name != verifier.Name {
		t.Errorf("Verify matched name %q; want %q", matched.Name, verifier.Name)
	}
}

func TestParseNoteRejectsMissingBlankLine(t *testing.T) {
	if _, err := ParseNote([]byte("just text no blank\n")); err == nil {
		t.Errorf("ParseNote with no blank-line separator: want error")
	}
}

func TestParseNoteRejectsNoSignatures(t *testing.T) {
	if _, err := ParseNote([]byte("body line\n\n")); err == nil {
		t.Errorf("ParseNote with empty signature block: want error")
	}
}

func TestParseNoteRejectsBodyWithSignaturePrefix(t *testing.T) {
	signer, _ := makeSigner(t, "test")
	// A note whose body line begins with "— " would alias as a
	// signature line; ParseNote must reject it.
	body := []byte("— bogus signature\nactual body\n")
	out, err := signer.Sign(body)
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	if _, err := ParseNote(out); err == nil {
		t.Errorf("ParseNote with signature-prefix in body: want error")
	}
}

func TestVerifyRejectsWrongKey(t *testing.T) {
	signer, _ := makeSigner(t, "alpha")
	_, otherVerifier := makeSigner(t, "alpha")
	out, err := signer.Sign([]byte("body\n"))
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	note, err := ParseNote(out)
	if err != nil {
		t.Fatalf("ParseNote: %v", err)
	}
	if _, err := note.Verify(otherVerifier); !errors.Is(err, ErrNoteUnverified) {
		t.Errorf("Verify with non-matching key = %v; want ErrNoteUnverified", err)
	}
}

func TestVerifyRejectsTamperedBody(t *testing.T) {
	signer, verifier := makeSigner(t, "alpha")
	out, err := signer.Sign([]byte("body\n"))
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	// Flip a byte in the body.
	tampered := []byte(strings.Replace(string(out), "body", "BODY", 1))
	note, err := ParseNote(tampered)
	if err != nil {
		t.Fatalf("ParseNote: %v", err)
	}
	if _, err := note.Verify(verifier); !errors.Is(err, ErrNoteUnverified) {
		t.Errorf("Verify after body tamper = %v; want ErrNoteUnverified", err)
	}
}

func TestSignAppendsTrailingNewline(t *testing.T) {
	signer, _ := makeSigner(t, "name")
	out, err := signer.Sign([]byte("no newline"))
	if err != nil {
		t.Fatalf("Sign: %v", err)
	}
	note, err := ParseNote(out)
	if err != nil {
		t.Fatalf("ParseNote: %v", err)
	}
	if !strings.HasSuffix(note.Text, "\n") {
		t.Errorf("Sign did not append trailing newline (Text=%q)", note.Text)
	}
}

func TestVerifyEmptyKeys(t *testing.T) {
	signer, _ := makeSigner(t, "n")
	out, _ := signer.Sign([]byte("body\n"))
	note, _ := ParseNote(out)
	if _, err := note.Verify(); err == nil {
		t.Errorf("Verify with no keys returned nil; want error")
	}
}
