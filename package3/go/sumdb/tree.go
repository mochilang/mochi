package sumdb

import (
	"crypto/sha256"
	"encoding/base64"
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// HashSize is the byte length of every node hash in the transparency
// log Merkle tree. The tree uses SHA-256 throughout, so all node
// hashes are 32 bytes.
const HashSize = 32

// Hash is a fixed-size SHA-256 node hash.
type Hash [HashSize]byte

// TreeHead is the parsed body of a signed-tree-head note. It binds
// a tree size to a root hash; with a verifier key it constitutes the
// proof that a record at index < Size is irrevocably committed to.
type TreeHead struct {
	// Origin is the first line of the tree note (e.g.
	// "go.sum database tree").
	Origin string
	// Size is the number of leaves committed to in this tree head.
	Size int64
	// Hash is the Merkle root of those Size leaves.
	Hash Hash
}

// ParseTreeHead decodes the text body of a signed-tree-head note.
//
// The wire format is:
//
//	<origin>\n
//	<size>\n
//	<base64(root-hash)>\n
//
// (The terminating '\n' is required; ParseNote will have already
// included it in Note.Text.)
func ParseTreeHead(body []byte) (*TreeHead, error) {
	if len(body) == 0 {
		return nil, errors.New("sumdb: empty tree-head body")
	}
	lines := strings.Split(strings.TrimSuffix(string(body), "\n"), "\n")
	if len(lines) != 3 {
		return nil, fmt.Errorf("sumdb: tree-head must have 3 lines, got %d", len(lines))
	}
	origin := lines[0]
	if origin == "" {
		return nil, errors.New("sumdb: tree-head origin line is empty")
	}
	size, err := strconv.ParseInt(lines[1], 10, 64)
	if err != nil {
		return nil, fmt.Errorf("sumdb: tree-head size %q: %w", lines[1], err)
	}
	if size < 0 {
		return nil, fmt.Errorf("sumdb: tree-head size %d is negative", size)
	}
	hashBytes, err := base64.StdEncoding.DecodeString(lines[2])
	if err != nil {
		return nil, fmt.Errorf("sumdb: tree-head hash %q: %w", lines[2], err)
	}
	if len(hashBytes) != HashSize {
		return nil, fmt.Errorf("sumdb: tree-head hash length %d != %d", len(hashBytes), HashSize)
	}
	var h Hash
	copy(h[:], hashBytes)
	return &TreeHead{Origin: origin, Size: size, Hash: h}, nil
}

// HashLeaf is the leaf-hash function for the RFC-6962 Merkle tree
// used by the transparency log. It is sha256(0x00 || record).
func HashLeaf(record []byte) Hash {
	h := sha256.New()
	h.Write([]byte{0x00})
	h.Write(record)
	var out Hash
	copy(out[:], h.Sum(nil))
	return out
}

// HashChildren is the interior-hash function for the RFC-6962 Merkle
// tree. It is sha256(0x01 || left || right).
func HashChildren(left, right Hash) Hash {
	h := sha256.New()
	h.Write([]byte{0x01})
	h.Write(left[:])
	h.Write(right[:])
	var out Hash
	copy(out[:], h.Sum(nil))
	return out
}

// MerkleRoot computes the Merkle root over the given leaves using the
// RFC-6962 algorithm. The slice may be empty (root is sha256("")).
// MerkleRoot mutates a working copy of leaves, not the input slice.
func MerkleRoot(leaves []Hash) Hash {
	if len(leaves) == 0 {
		var out Hash
		s := sha256.Sum256(nil)
		copy(out[:], s[:])
		return out
	}
	return merkleRootRange(leaves, 0, len(leaves))
}

func merkleRootRange(leaves []Hash, lo, hi int) Hash {
	n := hi - lo
	if n == 1 {
		return leaves[lo]
	}
	// Split point is the largest power of two strictly less than n
	// (per RFC-6962 §2.1). For n a power of two this is n/2; for
	// other sizes it is the dominant power-of-two below n.
	k := largestPow2LessThan(n)
	left := merkleRootRange(leaves, lo, lo+k)
	right := merkleRootRange(leaves, lo+k, hi)
	return HashChildren(left, right)
}

// largestPow2LessThan returns the largest power of two strictly less
// than n, for n >= 2. For n=2 it returns 1. The function is undefined
// for n < 2 (callers must check).
func largestPow2LessThan(n int) int {
	if n < 2 {
		return 0
	}
	k := 1
	for k<<1 < n {
		k <<= 1
	}
	return k
}

// VerifyInclusion verifies an RFC-6962 inclusion proof: starting from
// the leaf at index `leafIndex` inside a tree of size `treeSize`,
// applying the `proof` hashes (one per missing sibling on the path
// from leaf to root, ordered from leaf-side up) must reconstruct
// `root`.
//
// Returns nil on success, or a descriptive error otherwise.
func VerifyInclusion(leafIndex, treeSize int64, leafHash Hash, proof []Hash, root Hash) error {
	if leafIndex < 0 || treeSize < 0 {
		return errors.New("sumdb: negative leafIndex or treeSize")
	}
	if leafIndex >= treeSize {
		return fmt.Errorf("sumdb: leafIndex %d out of range for treeSize %d", leafIndex, treeSize)
	}
	// Reconstruct the root by walking up the tree, picking siblings
	// from the proof in the order RFC-6962 §2.1.1 specifies.
	// The (fn, sn) pair tracks (this-node-index, last-node-index) at
	// each level; when fn is even and equal to sn (the boundary of
	// an odd-sized level) the node promotes without combining,
	// consuming no proof element.
	fn := leafIndex
	sn := treeSize - 1
	cur := leafHash
	pi := 0
	for sn > 0 {
		isRightChild := fn&1 == 1
		atBoundary := fn == sn && !isRightChild
		switch {
		case atBoundary:
			// Boundary promotion: no sibling at this level.
		case isRightChild || fn == sn:
			if pi >= len(proof) {
				return errors.New("sumdb: inclusion proof too short")
			}
			cur = HashChildren(proof[pi], cur)
			pi++
		default:
			if pi >= len(proof) {
				return errors.New("sumdb: inclusion proof too short")
			}
			cur = HashChildren(cur, proof[pi])
			pi++
		}
		fn >>= 1
		sn >>= 1
	}
	if pi != len(proof) {
		return fmt.Errorf("sumdb: inclusion proof has %d unused hashes", len(proof)-pi)
	}
	if cur != root {
		return errors.New("sumdb: inclusion proof does not reconstruct the root")
	}
	return nil
}

// HashFromBase64 decodes a base64-StdEncoding string into a Hash.
// Returns an error if the decoded length is not HashSize.
func HashFromBase64(s string) (Hash, error) {
	var out Hash
	raw, err := base64.StdEncoding.DecodeString(s)
	if err != nil {
		return out, err
	}
	if len(raw) != HashSize {
		return out, fmt.Errorf("sumdb: hash length %d != %d", len(raw), HashSize)
	}
	copy(out[:], raw)
	return out, nil
}

// Base64 returns the base64-StdEncoding rendering of h.
func (h Hash) Base64() string {
	return base64.StdEncoding.EncodeToString(h[:])
}
