package simple

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"hash"
	"io"
	"sort"
	"strings"

	"lukechampine.com/blake3"
)

// SupportedHashAlgos is the set of digest algorithms the verifier accepts.
// The bridge prefers blake3 (PEP 658 extension) when the index advertises it,
// then sha256 (PEP 503 baseline). MD5 is rejected even when the index lists
// it: it has been considered unsafe since well before 2026 and `pip` already
// refuses to install on MD5-only entries.
var SupportedHashAlgos = []string{"blake3", "sha256"}

// Verify reads the file contents from r, computes hashes, and confirms that
// at least one of the expected hashes matches. Returns an error when no
// supported algorithm is present in expected, or when every supported
// algorithm mismatches.
//
// The function reads r exactly once (it streams through a multi-writer).
// Callers that need the verified bytes should pass an io.TeeReader to
// capture them while Verify drains the stream.
func Verify(r io.Reader, expected map[string]string) error {
	if len(expected) == 0 {
		return fmt.Errorf("simple: no expected hashes")
	}
	// Pick all supported algorithms present in `expected`.
	type slot struct {
		algo string
		hash hash.Hash
		want string
	}
	var slots []slot
	for _, algo := range SupportedHashAlgos {
		if want, ok := expected[algo]; ok {
			var h hash.Hash
			switch algo {
			case "sha256":
				h = sha256.New()
			case "blake3":
				h = blake3.New(32, nil)
			default:
				continue
			}
			slots = append(slots, slot{algo: algo, hash: h, want: strings.ToLower(want)})
		}
	}
	if len(slots) == 0 {
		seen := make([]string, 0, len(expected))
		for k := range expected {
			seen = append(seen, k)
		}
		sort.Strings(seen)
		return fmt.Errorf("simple: no supported hash algorithm in %v; need one of %v", seen, SupportedHashAlgos)
	}
	writers := make([]io.Writer, len(slots))
	for i, s := range slots {
		writers[i] = s.hash
	}
	mw := io.MultiWriter(writers...)
	if _, err := io.Copy(mw, r); err != nil {
		return fmt.Errorf("simple: read body: %w", err)
	}
	// All slots must match.
	for _, s := range slots {
		got := hex.EncodeToString(s.hash.Sum(nil))
		if got != s.want {
			return fmt.Errorf("simple: %s mismatch: got %s want %s", s.algo, got, s.want)
		}
	}
	return nil
}

// HashAll returns the hex digest of every SupportedHashAlgos algorithm on the
// input. Used by the cache to populate the local content-addressed entry.
// The function reads r exactly once.
func HashAll(r io.Reader) (map[string]string, error) {
	hashes := make(map[string]hash.Hash, len(SupportedHashAlgos))
	writers := make([]io.Writer, 0, len(SupportedHashAlgos))
	for _, algo := range SupportedHashAlgos {
		var h hash.Hash
		switch algo {
		case "sha256":
			h = sha256.New()
		case "blake3":
			h = blake3.New(32, nil)
		}
		hashes[algo] = h
		writers = append(writers, h)
	}
	if _, err := io.Copy(io.MultiWriter(writers...), r); err != nil {
		return nil, fmt.Errorf("simple: HashAll: %w", err)
	}
	out := make(map[string]string, len(hashes))
	for algo, h := range hashes {
		out[algo] = hex.EncodeToString(h.Sum(nil))
	}
	return out, nil
}
