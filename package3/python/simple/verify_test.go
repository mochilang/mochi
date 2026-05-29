package simple

import (
	"crypto/sha256"
	"encoding/hex"
	"strings"
	"testing"

	"lukechampine.com/blake3"
)

func sha256Hex(s string) string {
	h := sha256.Sum256([]byte(s))
	return hex.EncodeToString(h[:])
}

func blake3Hex(s string) string {
	h := blake3.New(32, nil)
	h.Write([]byte(s))
	return hex.EncodeToString(h.Sum(nil))
}

func TestVerifySHA256Matches(t *testing.T) {
	body := "hello world"
	want := sha256Hex(body)
	err := Verify(strings.NewReader(body), map[string]string{"sha256": want})
	if err != nil {
		t.Errorf("Verify err = %v; want nil", err)
	}
}

func TestVerifySHA256Mismatch(t *testing.T) {
	body := "hello world"
	err := Verify(strings.NewReader(body), map[string]string{"sha256": "00" + sha256Hex(body)[2:]})
	if err == nil {
		t.Fatal("Verify err = nil; want mismatch error")
	}
	if !strings.Contains(err.Error(), "sha256 mismatch") {
		t.Errorf("err = %v; want 'sha256 mismatch'", err)
	}
}

func TestVerifyBlake3Matches(t *testing.T) {
	body := "hello world"
	want := blake3Hex(body)
	err := Verify(strings.NewReader(body), map[string]string{"blake3": want})
	if err != nil {
		t.Errorf("Verify err = %v; want nil", err)
	}
}

func TestVerifyBothAlgosRequireBoth(t *testing.T) {
	body := "hello world"
	err := Verify(strings.NewReader(body), map[string]string{
		"sha256": sha256Hex(body),
		"blake3": blake3Hex(body),
	})
	if err != nil {
		t.Errorf("Verify err = %v; want nil when both match", err)
	}
}

func TestVerifyOneOfTwoMismatches(t *testing.T) {
	body := "hello world"
	err := Verify(strings.NewReader(body), map[string]string{
		"sha256": sha256Hex(body),
		"blake3": "00" + blake3Hex(body)[2:],
	})
	if err == nil {
		t.Fatal("Verify err = nil; want error when one of the algos mismatches")
	}
}

func TestVerifyEmptyExpectedRejected(t *testing.T) {
	err := Verify(strings.NewReader("hello"), nil)
	if err == nil {
		t.Fatal("Verify err = nil; want error on empty expected")
	}
}

func TestVerifyMD5Rejected(t *testing.T) {
	body := "hello world"
	err := Verify(strings.NewReader(body), map[string]string{"md5": "abcd"})
	if err == nil {
		t.Fatal("Verify err = nil; want error: md5 not supported")
	}
	if !strings.Contains(err.Error(), "no supported hash algorithm") {
		t.Errorf("err = %v; want 'no supported hash algorithm'", err)
	}
}

func TestVerifyUppercaseHexAccepted(t *testing.T) {
	body := "hello world"
	want := strings.ToUpper(sha256Hex(body))
	err := Verify(strings.NewReader(body), map[string]string{"sha256": want})
	if err != nil {
		t.Errorf("Verify err = %v; uppercase hex must be normalised", err)
	}
}

func TestHashAll(t *testing.T) {
	body := "hello world"
	got, err := HashAll(strings.NewReader(body))
	if err != nil {
		t.Fatalf("HashAll err = %v", err)
	}
	if got["sha256"] != sha256Hex(body) {
		t.Errorf("sha256 = %q; want %q", got["sha256"], sha256Hex(body))
	}
	if got["blake3"] != blake3Hex(body) {
		t.Errorf("blake3 = %q; want %q", got["blake3"], blake3Hex(body))
	}
}

func TestHashAllEmptyInput(t *testing.T) {
	got, err := HashAll(strings.NewReader(""))
	if err != nil {
		t.Fatalf("HashAll err = %v", err)
	}
	if got["sha256"] != sha256Hex("") {
		t.Errorf("sha256(empty) = %q; want %q", got["sha256"], sha256Hex(""))
	}
}
