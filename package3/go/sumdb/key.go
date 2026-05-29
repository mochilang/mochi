// Package sumdb is the MEP-74 client for the sum.golang.org
// transparency log (a/k/a the "Go checksum database"). It implements
// the signed-note framing, the Ed25519 verifier, the lookup-response
// parser, and the Merkle inclusion-proof verifier needed to confirm
// that a module@version's h1: digests appear in the public log.
//
// References:
//
//	https://go.dev/ref/mod#checksum-database
//	https://research.swtch.com/tlog (Russ Cox's tree-log design notes)
//	golang.org/x/mod/sumdb (reference implementation)
package sumdb

import (
	"crypto/ed25519"
	"crypto/sha256"
	"encoding/base64"
	"encoding/binary"
	"errors"
	"fmt"
	"strings"
)

// AlgEd25519 is the algorithm byte prefixed onto the Ed25519 public
// key inside a verifier-key blob. sum.golang.org and every other
// signed-note key today uses 0x01.
const AlgEd25519 byte = 0x01

// VerifierKey holds a parsed verifier key blob.
//
// The on-the-wire format of a verifier key is:
//
//	<name> '+' <hex-key-hash-prefix> '+' <base64(algByte || rawKeyBytes)>
//
// where algByte is AlgEd25519 (0x01) and rawKeyBytes is the 32-byte
// Ed25519 public key. The key-hash prefix is the first 4 bytes of
// SHA-256(name + "\n" + algByte || rawKeyBytes), rendered as 8
// lower-case hex characters.
//
// The sum.golang.org verifier key is:
//
//	sum.golang.org+033de0ae+Ac4zctda0e5eza+HJyk9SxEdh+s3Ux18htTTAD8OuAn8
type VerifierKey struct {
	Name      string
	KeyHash   [4]byte
	Algorithm byte
	PublicKey ed25519.PublicKey
}

// SumGolangOrgVerifierKey is the verifier key for the public
// sum.golang.org transparency log. The key is baked in here so the
// bridge does not have to fetch it from a network endpoint at boot
// (which would itself be a chicken-and-egg integrity problem).
//
// Source: https://sum.golang.org/supported (mirrored in
// golang.org/x/mod/sumdb/sumdb.go).
const SumGolangOrgVerifierKey = "sum.golang.org+033de0ae+Ac4zctda0e5eza+HJyk9SxEdh+s3Ux18htTTAD8OuAn8"

// ParseVerifierKey parses the textual verifier key blob.
func ParseVerifierKey(s string) (*VerifierKey, error) {
	if s == "" {
		return nil, errors.New("sumdb: empty verifier key")
	}
	// Format: name + "+" + 8 hex chars + "+" + base64.
	parts := strings.SplitN(s, "+", 3)
	if len(parts) != 3 {
		return nil, fmt.Errorf("sumdb: verifier key %q must have name+hash+key fields", s)
	}
	name, hashHex, keyB64 := parts[0], parts[1], parts[2]
	if name == "" {
		return nil, fmt.Errorf("sumdb: verifier key %q: empty name", s)
	}
	if len(hashHex) != 8 {
		return nil, fmt.Errorf("sumdb: verifier key %q: key-hash field must be 8 hex chars, got %d", s, len(hashHex))
	}
	var keyHash [4]byte
	for i := range 4 {
		hi, ok1 := unhex(hashHex[2*i])
		lo, ok2 := unhex(hashHex[2*i+1])
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("sumdb: verifier key %q: non-hex in key-hash field", s)
		}
		keyHash[i] = hi<<4 | lo
	}
	rawKey, err := base64.StdEncoding.DecodeString(keyB64)
	if err != nil {
		return nil, fmt.Errorf("sumdb: verifier key %q: invalid base64: %w", s, err)
	}
	if len(rawKey) < 1 {
		return nil, fmt.Errorf("sumdb: verifier key %q: empty key body", s)
	}
	alg := rawKey[0]
	if alg != AlgEd25519 {
		return nil, fmt.Errorf("sumdb: verifier key %q: unsupported algorithm 0x%02x", s, alg)
	}
	if len(rawKey)-1 != ed25519.PublicKeySize {
		return nil, fmt.Errorf("sumdb: verifier key %q: ed25519 key must be %d bytes, got %d", s, ed25519.PublicKeySize, len(rawKey)-1)
	}
	pub := ed25519.PublicKey(rawKey[1:])
	// Recompute the key-hash and compare against the prefix in the blob.
	wantHash := computeKeyHash(name, alg, pub)
	if wantHash != keyHash {
		return nil, fmt.Errorf("sumdb: verifier key %q: key-hash mismatch (computed %02x%02x%02x%02x)",
			s, wantHash[0], wantHash[1], wantHash[2], wantHash[3])
	}
	return &VerifierKey{
		Name:      name,
		KeyHash:   keyHash,
		Algorithm: alg,
		PublicKey: pub,
	}, nil
}

// computeKeyHash is the canonical hash used to disambiguate keys in
// signed-note signatures. It is the first 4 bytes of
// SHA-256(name + "\n" + algByte || pubKey).
func computeKeyHash(name string, alg byte, pub ed25519.PublicKey) [4]byte {
	h := sha256.New()
	h.Write([]byte(name))
	h.Write([]byte{'\n'})
	h.Write([]byte{alg})
	h.Write(pub)
	sum := h.Sum(nil)
	var out [4]byte
	copy(out[:], sum[:4])
	return out
}

// KeyHashUint32 returns the key-hash as a single big-endian uint32 for
// convenient comparison.
func (v *VerifierKey) KeyHashUint32() uint32 {
	return binary.BigEndian.Uint32(v.KeyHash[:])
}

// String renders the verifier key in its on-the-wire form.
func (v *VerifierKey) String() string {
	if v == nil {
		return ""
	}
	body := append([]byte{v.Algorithm}, v.PublicKey...)
	return fmt.Sprintf("%s+%02x%02x%02x%02x+%s",
		v.Name, v.KeyHash[0], v.KeyHash[1], v.KeyHash[2], v.KeyHash[3],
		base64.StdEncoding.EncodeToString(body))
}

func unhex(c byte) (byte, bool) {
	switch {
	case c >= '0' && c <= '9':
		return c - '0', true
	case c >= 'a' && c <= 'f':
		return c - 'a' + 10, true
	case c >= 'A' && c <= 'F':
		return c - 'A' + 10, true
	}
	return 0, false
}
