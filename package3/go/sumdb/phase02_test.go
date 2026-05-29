package sumdb

import (
	"context"
	"crypto/ed25519"
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestPhase2Sumdb is the umbrella sentinel for MEP-74 phase 2. It
// drives a complete sumdb consume loop against an httptest fake:
//
//  1. Generate a fresh ed25519 keypair representing a transparency log.
//  2. Sign a tree-head note over a 4-leaf Merkle tree containing a
//     known module record.
//  3. Serve /lookup/<m>@<v> with the lookup-response framing.
//  4. Client.Lookup fetches the record, ParseNote + Verify the tree
//     note, and ParseTreeHead extracts the size + root.
//  5. VerifyInclusion confirms the record is at the claimed leaf.
//
// Passing this test means a downstream caller can integrate phase 2
// without any external network call.
func TestPhase2Sumdb(t *testing.T) {
	pub, priv, err := ed25519.GenerateKey(rand.Reader)
	if err != nil {
		t.Fatalf("GenerateKey: %v", err)
	}
	name := "fake-log.example"
	keyHash := computeKeyHash(name, AlgEd25519, pub)
	verifier := &VerifierKey{Name: name, KeyHash: keyHash, Algorithm: AlgEd25519, PublicKey: pub}
	signer := &Signer{Name: name, PrivateKey: priv}

	// 4 leaves; the target record sits at index 2 with content "rec2".
	const targetIndex = 2
	leaves := []Hash{
		HashLeaf([]byte("rec0")),
		HashLeaf([]byte("rec1")),
		HashLeaf([]byte("rec2")), // our module's record
		HashLeaf([]byte("rec3")),
	}
	root := MerkleRoot(leaves)
	treeBody := fmt.Sprintf("fake-log tree\n%d\n%s\n", len(leaves), base64.StdEncoding.EncodeToString(root[:]))
	signedTree, err := signer.Sign([]byte(treeBody))
	if err != nil {
		t.Fatalf("Sign tree: %v", err)
	}

	lookupBody := fmt.Sprintf(`%d
example.com/foo v1.0.0 h1:zipDigestEqualsTwoSlashesPadded==
example.com/foo v1.0.0/go.mod h1:modDigestEqualsTwoSlashesPadded==

%s`, targetIndex, signedTree)

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case strings.HasSuffix(r.URL.Path, "/latest"):
			w.Write(signedTree)
		case strings.Contains(r.URL.Path, "/lookup/"):
			io.WriteString(w, lookupBody)
		default:
			http.NotFound(w, r)
		}
	}))
	t.Cleanup(srv.Close)

	c := NewClient(srv.URL)
	ctx := context.Background()

	// 1. /latest fetch + parse + verify + ParseTreeHead.
	latest, err := c.Latest(ctx)
	if err != nil {
		t.Fatalf("Latest: %v", err)
	}
	note, err := ParseNote(latest)
	if err != nil {
		t.Fatalf("ParseNote(latest): %v", err)
	}
	if _, err := note.Verify(verifier); err != nil {
		t.Fatalf("Verify(latest): %v", err)
	}
	head, err := ParseTreeHead([]byte(note.Text))
	if err != nil {
		t.Fatalf("ParseTreeHead: %v", err)
	}
	if head.Size != int64(len(leaves)) {
		t.Errorf("tree size = %d; want %d", head.Size, len(leaves))
	}
	if head.Hash != root {
		t.Errorf("tree root mismatch")
	}

	// 2. /lookup fetch + parse.
	rec, err := c.Lookup(ctx, "example.com/foo", "v1.0.0")
	if err != nil {
		t.Fatalf("Lookup: %v", err)
	}
	if rec.ID != targetIndex {
		t.Errorf("Lookup ID = %d; want %d", rec.ID, targetIndex)
	}
	// 3. Verify the tree note that came back inside the lookup body.
	recNote, err := ParseNote(rec.TreeNote)
	if err != nil {
		t.Fatalf("ParseNote(lookup tree): %v", err)
	}
	if _, err := recNote.Verify(verifier); err != nil {
		t.Fatalf("Verify(lookup tree): %v", err)
	}
	// 4. Inclusion proof: the leaf at targetIndex is HashLeaf("rec2").
	proof := computeInclusionProof(leaves, targetIndex)
	if err := VerifyInclusion(int64(targetIndex), int64(len(leaves)), leaves[targetIndex], proof, root); err != nil {
		t.Errorf("VerifyInclusion: %v", err)
	}
}
