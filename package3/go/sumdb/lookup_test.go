package sumdb

import (
	"strings"
	"testing"
)

const sampleLookup = `1234567
example.com/foo v1.2.3 h1:abcDEFghi==
example.com/foo v1.2.3/go.mod h1:xyzPQR012==

go.sum database tree
12345678
TGl4dHV0dW10dXJ0dXBpaXR1aXR1dGZyZWVwYXNzeWVz

— sum.golang.org AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==
`

func TestParseLookupValid(t *testing.T) {
	rec, err := ParseLookup([]byte(sampleLookup))
	if err != nil {
		t.Fatalf("ParseLookup: %v", err)
	}
	if rec.ID != 1234567 {
		t.Errorf("ID = %d; want 1234567", rec.ID)
	}
	if rec.Module != "example.com/foo" {
		t.Errorf("Module = %q; want example.com/foo", rec.Module)
	}
	if rec.Version != "v1.2.3" {
		t.Errorf("Version = %q; want v1.2.3", rec.Version)
	}
	if !strings.HasPrefix(rec.ZipHash, "h1:") {
		t.Errorf("ZipHash %q lacks h1: prefix", rec.ZipHash)
	}
	if !strings.HasPrefix(rec.ModHash, "h1:") {
		t.Errorf("ModHash %q lacks h1: prefix", rec.ModHash)
	}
	if len(rec.TreeNote) == 0 {
		t.Errorf("TreeNote empty")
	}
}

func TestParseLookupRejectsEmpty(t *testing.T) {
	if _, err := ParseLookup(nil); err == nil {
		t.Errorf("ParseLookup(nil) returned nil; want error")
	}
}

func TestParseLookupMissingSeparator(t *testing.T) {
	body := `1234567
example.com/foo v1.2.3 h1:abc
example.com/foo v1.2.3/go.mod h1:def
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup without tree note: want error")
	}
}

func TestParseLookupBadID(t *testing.T) {
	body := `notanumber
example.com/foo v1.2.3 h1:abc
example.com/foo v1.2.3/go.mod h1:def

tree note placeholder
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup with bad id: want error")
	}
}

func TestParseLookupVersionMismatch(t *testing.T) {
	body := `1
example.com/foo v1.2.3 h1:abc
example.com/foo v9.9.9/go.mod h1:def

tree note placeholder
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup with mismatched versions: want error")
	}
}

func TestParseLookupModuleMismatch(t *testing.T) {
	body := `1
example.com/foo v1.2.3 h1:abc
other.example/bar v1.2.3/go.mod h1:def

tree note placeholder
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup with mismatched modules: want error")
	}
}

func TestParseLookupMissingGoModSuffix(t *testing.T) {
	body := `1
example.com/foo v1.2.3 h1:abc
example.com/foo v1.2.3 h1:def

tree note placeholder
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup with no /go.mod suffix: want error")
	}
}

func TestParseLookupBadHashPrefix(t *testing.T) {
	body := `1
example.com/foo v1.2.3 sha256:abc
example.com/foo v1.2.3/go.mod h1:def

tree note placeholder
`
	if _, err := ParseLookup([]byte(body)); err == nil {
		t.Errorf("ParseLookup with non-h1 zip hash: want error")
	}
}
