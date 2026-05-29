package lockfile

import (
	"crypto/sha256"
	"encoding/hex"
	"strings"
	"testing"
)

func TestWrapperSHA256EmptyString(t *testing.T) {
	got := WrapperSHA256("")
	want := "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	if got != want {
		t.Errorf("WrapperSHA256(\"\") = %q, want %q", got, want)
	}
}

func TestWrapperSHA256Hello(t *testing.T) {
	got := WrapperSHA256("hello")
	want := "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
	if got != want {
		t.Errorf("WrapperSHA256(\"hello\") = %q, want %q", got, want)
	}
}

func TestWrapperSHA256SampleLibRS(t *testing.T) {
	body := "pub fn add(a: i64, b: i64) -> i64 { a + b }\n"
	got := WrapperSHA256(body)
	if len(got) != 64 {
		t.Fatalf("expected 64-hex digest, got %d chars: %q", len(got), got)
	}
	if strings.ToLower(got) != got {
		t.Errorf("expected lowercase hex, got %q", got)
	}
	expected := sha256.Sum256([]byte(body))
	if hex.EncodeToString(expected[:]) != got {
		t.Errorf("digest mismatch")
	}
}

func TestWrapperSHA256IsDeterministic(t *testing.T) {
	body := "pub fn add(a: i64, b: i64) -> i64 { a + b }\n"
	a := WrapperSHA256(body)
	b := WrapperSHA256(body)
	if a != b {
		t.Errorf("WrapperSHA256 not deterministic: %q vs %q", a, b)
	}
}

func TestWrapperSHA256DetectsSingleByteChange(t *testing.T) {
	a := WrapperSHA256("pub fn add(a: i64, b: i64) -> i64 { a + b }\n")
	b := WrapperSHA256("pub fn add(a: i64, b: i64) -> i64 { a - b }\n")
	if a == b {
		t.Errorf("single byte change must produce different digest, both = %q", a)
	}
}

func TestRustdocSHA256Empty(t *testing.T) {
	got := RustdocSHA256(nil)
	want := "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
	if got != want {
		t.Errorf("RustdocSHA256(nil) = %q, want %q", got, want)
	}
}

func TestRustdocSHA256JSONLikeInput(t *testing.T) {
	body := []byte(`{"format_version":32,"crate_version":"0.4.3","index":{}}`)
	got := RustdocSHA256(body)
	if len(got) != 64 {
		t.Fatalf("expected 64-hex digest, got %d chars: %q", len(got), got)
	}
	expected := sha256.Sum256(body)
	if hex.EncodeToString(expected[:]) != got {
		t.Errorf("digest mismatch")
	}
}

func TestRustdocSHA256IsDeterministic(t *testing.T) {
	body := []byte(`{"format_version":32}`)
	a := RustdocSHA256(body)
	b := RustdocSHA256(body)
	if a != b {
		t.Errorf("RustdocSHA256 not deterministic: %q vs %q", a, b)
	}
}

func TestRustdocSHA256DetectsSingleByteChange(t *testing.T) {
	a := RustdocSHA256([]byte(`{"format_version":32}`))
	b := RustdocSHA256([]byte(`{"format_version":33}`))
	if a == b {
		t.Errorf("single byte change must produce different digest, both = %q", a)
	}
}
