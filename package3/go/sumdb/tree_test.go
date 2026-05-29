package sumdb

import (
	"encoding/base64"
	"strings"
	"testing"
)

func TestParseTreeHead(t *testing.T) {
	root := make([]byte, HashSize)
	for i := range root {
		root[i] = byte(i)
	}
	body := []byte("go.sum database tree\n12345\n" + base64.StdEncoding.EncodeToString(root) + "\n")
	h, err := ParseTreeHead(body)
	if err != nil {
		t.Fatalf("ParseTreeHead: %v", err)
	}
	if h.Origin != "go.sum database tree" {
		t.Errorf("Origin = %q", h.Origin)
	}
	if h.Size != 12345 {
		t.Errorf("Size = %d", h.Size)
	}
	for i := range h.Hash {
		if h.Hash[i] != byte(i) {
			t.Errorf("Hash[%d] = %x; want %x", i, h.Hash[i], i)
		}
	}
}

func TestParseTreeHeadMalformed(t *testing.T) {
	cases := []string{
		"",
		"origin only\n",
		"origin\nsize\n", // missing hash line
		"origin\nNaN\n" + base64.StdEncoding.EncodeToString(make([]byte, HashSize)) + "\n",
		"origin\n-5\n" + base64.StdEncoding.EncodeToString(make([]byte, HashSize)) + "\n",
		"\n1\n" + base64.StdEncoding.EncodeToString(make([]byte, HashSize)) + "\n", // empty origin
		"origin\n1\nNOTBASE64\n",
		"origin\n1\n" + base64.StdEncoding.EncodeToString(make([]byte, HashSize-1)) + "\n", // short hash
	}
	for _, tc := range cases {
		if _, err := ParseTreeHead([]byte(tc)); err == nil {
			t.Errorf("ParseTreeHead(%q) returned nil; want error", tc)
		}
	}
}

func TestHashLeafKnownVector(t *testing.T) {
	// sha256(0x00 || "") = 6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d
	got := HashLeaf(nil)
	gotHex := got.Base64()
	// Verify round-trip first.
	parsed, err := HashFromBase64(gotHex)
	if err != nil {
		t.Fatalf("HashFromBase64: %v", err)
	}
	if parsed != got {
		t.Errorf("Hash base64 round-trip mismatch")
	}
	// Compare leaf-bytes byte-for-byte against an inline computation.
	wantSlice := []byte{
		0x6e, 0x34, 0x0b, 0x9c, 0xff, 0xb3, 0x7a, 0x98,
		0x9c, 0xa5, 0x44, 0xe6, 0xbb, 0x78, 0x0a, 0x2c,
		0x78, 0x90, 0x1d, 0x3f, 0xb3, 0x37, 0x38, 0x76,
		0x85, 0x11, 0xa3, 0x06, 0x17, 0xaf, 0xa0, 0x1d,
	}
	for i, w := range wantSlice {
		if got[i] != w {
			t.Errorf("HashLeaf(nil)[%d] = %02x; want %02x", i, got[i], w)
		}
	}
}

func TestHashChildrenStable(t *testing.T) {
	a := HashLeaf([]byte("a"))
	b := HashLeaf([]byte("b"))
	x := HashChildren(a, b)
	y := HashChildren(a, b)
	if x != y {
		t.Errorf("HashChildren not stable")
	}
	if HashChildren(b, a) == x {
		t.Errorf("HashChildren symmetric (should not be)")
	}
}

func TestMerkleRootSizes(t *testing.T) {
	// For each tree size 1..16, compute the root via MerkleRoot and
	// re-derive via the spec algorithm by hand for sanity.
	for n := 1; n <= 16; n++ {
		leaves := make([]Hash, n)
		for i := range leaves {
			leaves[i] = HashLeaf([]byte{byte(i)})
		}
		got := MerkleRoot(leaves)
		want := manualMerkle(leaves)
		if got != want {
			t.Errorf("MerkleRoot at n=%d differs from manual: %x vs %x", n, got, want)
		}
	}
}

// manualMerkle is a reference recursion using the same RFC-6962
// algorithm; if MerkleRoot drifts, this catches it.
func manualMerkle(leaves []Hash) Hash {
	if len(leaves) == 1 {
		return leaves[0]
	}
	k := 1
	for k<<1 < len(leaves) {
		k <<= 1
	}
	return HashChildren(manualMerkle(leaves[:k]), manualMerkle(leaves[k:]))
}

func TestVerifyInclusionAllPositions(t *testing.T) {
	const n = 7
	leaves := make([]Hash, n)
	for i := range leaves {
		leaves[i] = HashLeaf([]byte{byte(i)})
	}
	root := MerkleRoot(leaves)
	for i := range leaves {
		proof := computeInclusionProof(leaves, i)
		if err := VerifyInclusion(int64(i), int64(n), leaves[i], proof, root); err != nil {
			t.Errorf("VerifyInclusion(i=%d): %v", i, err)
		}
		// Tamper with the leaf and confirm rejection.
		var bad Hash
		bad[0] = 0xff
		if err := VerifyInclusion(int64(i), int64(n), bad, proof, root); err == nil {
			t.Errorf("VerifyInclusion accepted tampered leaf at i=%d", i)
		}
	}
}

func TestVerifyInclusionShortProof(t *testing.T) {
	leaves := make([]Hash, 4)
	for i := range leaves {
		leaves[i] = HashLeaf([]byte{byte(i)})
	}
	root := MerkleRoot(leaves)
	// Real proof for leaf 0 has length 2; pass an empty proof.
	if err := VerifyInclusion(0, 4, leaves[0], nil, root); err == nil {
		t.Errorf("VerifyInclusion with empty proof: want error")
	}
}

func TestVerifyInclusionOutOfRange(t *testing.T) {
	leaves := make([]Hash, 2)
	leaves[0] = HashLeaf([]byte{0})
	leaves[1] = HashLeaf([]byte{1})
	root := MerkleRoot(leaves)
	if err := VerifyInclusion(2, 2, leaves[0], nil, root); err == nil {
		t.Errorf("VerifyInclusion with leafIndex >= treeSize: want error")
	}
	if err := VerifyInclusion(-1, 2, leaves[0], nil, root); err == nil {
		t.Errorf("VerifyInclusion with negative leafIndex: want error")
	}
}

func TestHashFromBase64Errors(t *testing.T) {
	if _, err := HashFromBase64("!!notbase64!!"); err == nil {
		t.Errorf("HashFromBase64 with bad base64: want error")
	}
	if _, err := HashFromBase64(base64.StdEncoding.EncodeToString([]byte("short"))); err == nil {
		t.Errorf("HashFromBase64 with wrong length: want error")
	}
	good := base64.StdEncoding.EncodeToString(make([]byte, HashSize))
	if !strings.HasSuffix(good, "=") {
		// padding sanity
		t.Logf("base64 encoded length: %d", len(good))
	}
	if _, err := HashFromBase64(good); err != nil {
		t.Errorf("HashFromBase64 on valid input: %v", err)
	}
}

// computeInclusionProof produces the RFC-6962 inclusion proof for
// leaves[index] in a tree of size len(leaves). It is a test helper
// used to exercise VerifyInclusion across positions.
func computeInclusionProof(leaves []Hash, index int) []Hash {
	return inclusionProofRange(leaves, 0, len(leaves), index)
}

func inclusionProofRange(leaves []Hash, lo, hi, target int) []Hash {
	n := hi - lo
	if n == 1 {
		return nil
	}
	k := 1
	for k<<1 < n {
		k <<= 1
	}
	if target-lo < k {
		// Target is on the left; sibling is the right subtree root.
		sub := inclusionProofRange(leaves, lo, lo+k, target)
		right := merkleRootRange(leaves, lo+k, hi)
		return append(sub, right)
	}
	// Target is on the right; sibling is the left subtree root.
	sub := inclusionProofRange(leaves, lo+k, hi, target)
	left := merkleRootRange(leaves, lo, lo+k)
	return append(sub, left)
}
