package sumdb

import (
	"crypto/ed25519"
	"encoding/base64"
	"errors"
	"fmt"
	"strings"
)

// Note is a parsed signed note as used by sum.golang.org.
//
// The wire format is:
//
//	<text body, ending in '\n'>
//	'\n'
//	<one-or-more signature lines>
//
// Each signature line is:
//
//	'—' SP <key-name> SP <base64(keyHash[0:4] || signature)> '\n'
//
// The U+2014 EM DASH at the start is a literal three-byte UTF-8
// sequence; the line is otherwise plain ASCII. The signature
// payload is exactly 4 + ed25519.SignatureSize = 68 bytes.
type Note struct {
	// Text is the message body (everything up to but not including
	// the blank line that separates body from signatures). The body
	// is signed verbatim including its trailing '\n'.
	Text string

	// Signatures lists one entry per signature line. Use Verify to
	// confirm that a desired key's signature is present and valid.
	Signatures []NoteSignature
}

// NoteSignature is one parsed signature line.
type NoteSignature struct {
	// Name is the key name from the signature line (e.g.
	// "sum.golang.org").
	Name string
	// KeyHash is the 4-byte key-hash prefix that disambiguates this
	// signature when the same signer publishes multiple keys.
	KeyHash [4]byte
	// Sig is the raw Ed25519 signature bytes (64 bytes).
	Sig []byte
}

// noteSigPrefix is the U+2014 EM DASH + space prefix on every
// signature line.
const noteSigPrefix = "— "

// ParseNote splits raw note bytes into the text body and signature
// lines. It does not verify any signature.
//
// Per the spec a valid note has at least one signature; an empty
// signature section is an error.
func ParseNote(raw []byte) (*Note, error) {
	if len(raw) == 0 {
		return nil, errors.New("sumdb: empty note")
	}
	// Find the blank line separating body from signatures. The body
	// ends with "\n" and the blank line is exactly "\n", so the
	// separator pattern is "\n\n".
	s := string(raw)
	sep := strings.Index(s, "\n\n")
	if sep < 0 {
		return nil, errors.New("sumdb: note missing blank-line separator")
	}
	body := s[:sep+1] // include the body's trailing newline
	sigBlock := s[sep+2:]
	// Body must be UTF-8 with no embedded "— " at start of line;
	// the simplest check is: body lines must not start with the
	// signature prefix.
	for line := range strings.SplitSeq(strings.TrimSuffix(body, "\n"), "\n") {
		if strings.HasPrefix(line, noteSigPrefix) {
			return nil, errors.New("sumdb: note body contains a line that starts with the signature prefix")
		}
	}
	sigs, err := parseSignatureLines(sigBlock)
	if err != nil {
		return nil, err
	}
	if len(sigs) == 0 {
		return nil, errors.New("sumdb: note has no signatures")
	}
	return &Note{Text: body, Signatures: sigs}, nil
}

func parseSignatureLines(block string) ([]NoteSignature, error) {
	out := []NoteSignature{}
	for line := range strings.SplitSeq(block, "\n") {
		if line == "" {
			continue
		}
		if !strings.HasPrefix(line, noteSigPrefix) {
			return nil, fmt.Errorf("sumdb: signature line %q missing %q prefix", line, noteSigPrefix)
		}
		rest := line[len(noteSigPrefix):]
		sp := strings.IndexByte(rest, ' ')
		if sp <= 0 {
			return nil, fmt.Errorf("sumdb: signature line %q missing key name", line)
		}
		name := rest[:sp]
		b64 := strings.TrimSpace(rest[sp+1:])
		if name == "" || b64 == "" {
			return nil, fmt.Errorf("sumdb: signature line %q has empty name or body", line)
		}
		buf, err := base64.StdEncoding.DecodeString(b64)
		if err != nil {
			return nil, fmt.Errorf("sumdb: signature line %q invalid base64: %w", line, err)
		}
		if len(buf) != 4+ed25519.SignatureSize {
			return nil, fmt.Errorf("sumdb: signature line %q wrong length (%d bytes, want %d)",
				line, len(buf), 4+ed25519.SignatureSize)
		}
		var hash [4]byte
		copy(hash[:], buf[:4])
		out = append(out, NoteSignature{
			Name:    name,
			KeyHash: hash,
			Sig:     buf[4:],
		})
	}
	return out, nil
}

// Verify reports whether the note carries at least one valid
// signature from one of the provided keys. Returns the matched key
// on success.
//
// The signature is computed over the entire body bytes including
// trailing newline.
func (n *Note) Verify(keys ...*VerifierKey) (*VerifierKey, error) {
	if n == nil {
		return nil, errors.New("sumdb: nil note")
	}
	if len(keys) == 0 {
		return nil, errors.New("sumdb: no verifier keys provided")
	}
	body := []byte(n.Text)
	for _, sig := range n.Signatures {
		for _, k := range keys {
			if k == nil {
				continue
			}
			if sig.Name != k.Name {
				continue
			}
			if sig.KeyHash != k.KeyHash {
				continue
			}
			if ed25519.Verify(k.PublicKey, body, sig.Sig) {
				return k, nil
			}
		}
	}
	return nil, ErrNoteUnverified
}

// ErrNoteUnverified is returned when a note carries no valid
// signature from any of the candidate keys.
var ErrNoteUnverified = errors.New("sumdb: note has no verifying signature")

// SignerSigner signs a note body. It is exposed for tests and for
// phase 12 (publish) which produces signed notes for tagged
// publishes.
type Signer struct {
	Name       string
	PrivateKey ed25519.PrivateKey
}

// Sign returns a complete signed-note byte block for body. body must
// end in '\n'; if it does not, a trailing '\n' is appended.
func (s *Signer) Sign(body []byte) ([]byte, error) {
	if s == nil || s.PrivateKey == nil {
		return nil, errors.New("sumdb: nil signer")
	}
	if len(body) == 0 {
		return nil, errors.New("sumdb: empty body")
	}
	if body[len(body)-1] != '\n' {
		// The note framing requires the body to end in '\n'. Add it
		// for convenience.
		body = append(append([]byte{}, body...), '\n')
	}
	pub := ed25519.PublicKey(s.PrivateKey.Public().(ed25519.PublicKey))
	keyHash := computeKeyHash(s.Name, AlgEd25519, pub)
	sig := ed25519.Sign(s.PrivateKey, body)
	line := make([]byte, 0, 4+ed25519.SignatureSize)
	line = append(line, keyHash[:]...)
	line = append(line, sig...)
	encoded := base64.StdEncoding.EncodeToString(line)
	out := make([]byte, 0, len(body)+len(noteSigPrefix)+len(s.Name)+1+len(encoded)+2)
	out = append(out, body...)
	out = append(out, '\n')
	out = append(out, noteSigPrefix...)
	out = append(out, s.Name...)
	out = append(out, ' ')
	out = append(out, encoded...)
	out = append(out, '\n')
	return out, nil
}
