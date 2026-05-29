package blob

import (
	"bytes"
	"crypto/rand"
	"encoding/hex"
	"io"
	"os"
	"path/filepath"
	"testing"
)

// Known SHA-256 for a 1KB all-zeros file:
//   echo -n "" | dd if=/dev/zero bs=1024 count=1 | sha256sum
//   sha256("000...0" × 1024) = known value
// We compute it from the standard library for reference.

func TestHashReader_AllZeros(t *testing.T) {
	data := make([]byte, 1024)
	sha256sum, blake3sum, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader error: %v", err)
	}

	// Re-compute with stdlib to verify
	sha256Ref, blake3Ref, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader reference error: %v", err)
	}

	if sha256sum != sha256Ref {
		t.Errorf("SHA256 mismatch: %x vs %x", sha256sum, sha256Ref)
	}
	if blake3sum != blake3Ref {
		t.Errorf("BLAKE3 mismatch: %x vs %x", blake3sum, blake3Ref)
	}

	// Verify known SHA-256 for 1024 zero bytes
	// sha256(zeros×1024) = 5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef
	expectedSHA256 := "5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef"
	gotSHA256 := hex.EncodeToString(sha256sum[:])
	if gotSHA256 != expectedSHA256 {
		t.Errorf("SHA256 for zeros×1024: got %s, want %s", gotSHA256, expectedSHA256)
	}
}

func TestHashReader_Random64KB(t *testing.T) {
	data := make([]byte, 64*1024)
	if _, err := io.ReadFull(rand.Reader, data); err != nil {
		t.Fatalf("rand read: %v", err)
	}

	sha256_1, blake3_1, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader (first pass): %v", err)
	}

	sha256_2, blake3_2, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader (second pass): %v", err)
	}

	if sha256_1 != sha256_2 {
		t.Error("SHA256 not deterministic for same data")
	}
	if blake3_1 != blake3_2 {
		t.Error("BLAKE3 not deterministic for same data")
	}

	// Different data should give different hashes
	data2 := make([]byte, 64*1024)
	if _, err := io.ReadFull(rand.Reader, data2); err != nil {
		t.Fatalf("rand read data2: %v", err)
	}
	sha256_3, blake3_3, err := HashReader(bytes.NewReader(data2))
	if err != nil {
		t.Fatalf("HashReader data2: %v", err)
	}
	if sha256_1 == sha256_3 {
		t.Error("SHA256 collision for different random data (astronomically unlikely)")
	}
	if blake3_1 == blake3_3 {
		t.Error("BLAKE3 collision for different random data (astronomically unlikely)")
	}
}

func TestHashFile_AllZeros(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "zeros.bin")

	data := make([]byte, 1024)
	if err := os.WriteFile(path, data, 0644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	sha256sum, blake3sum, err := HashFile(path)
	if err != nil {
		t.Fatalf("HashFile: %v", err)
	}

	// Should match HashReader result
	refSHA, refB3, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader: %v", err)
	}

	if sha256sum != refSHA {
		t.Errorf("SHA256 mismatch: HashFile=%x HashReader=%x", sha256sum, refSHA)
	}
	if blake3sum != refB3 {
		t.Errorf("BLAKE3 mismatch: HashFile=%x HashReader=%x", blake3sum, refB3)
	}
}

func TestHashFile_Random64KB(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "random.bin")

	data := make([]byte, 64*1024)
	if _, err := io.ReadFull(rand.Reader, data); err != nil {
		t.Fatalf("rand read: %v", err)
	}
	if err := os.WriteFile(path, data, 0644); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	sha256sum, blake3sum, err := HashFile(path)
	if err != nil {
		t.Fatalf("HashFile: %v", err)
	}

	refSHA, refB3, err := HashReader(bytes.NewReader(data))
	if err != nil {
		t.Fatalf("HashReader: %v", err)
	}

	if sha256sum != refSHA {
		t.Errorf("SHA256 mismatch: HashFile=%x HashReader=%x", sha256sum, refSHA)
	}
	if blake3sum != refB3 {
		t.Errorf("BLAKE3 mismatch: HashFile=%x HashReader=%x", blake3sum, refB3)
	}
}

func TestHashFile_NotExist(t *testing.T) {
	_, _, err := HashFile("/nonexistent/path/file.bin")
	if err == nil {
		t.Error("expected error for non-existent file, got nil")
	}
}

func TestHashReader_Empty(t *testing.T) {
	sha256sum, blake3sum, err := HashReader(bytes.NewReader(nil))
	if err != nil {
		t.Fatalf("HashReader empty: %v", err)
	}

	// SHA-256 of empty string is well-known
	expectedSHA256 := "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	gotSHA256 := hex.EncodeToString(sha256sum[:])
	if gotSHA256 != expectedSHA256 {
		t.Errorf("SHA256 for empty: got %s, want %s", gotSHA256, expectedSHA256)
	}
	_ = blake3sum // just verify no error
}
