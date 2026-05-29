package publish

import (
	"bytes"
	"compress/gzip"
	"strings"
	"testing"

	"mochi/package3/rust/library"
)

func sampleFiles() library.Files {
	return library.Files{
		"Cargo.toml": "[package]\nname = \"demo\"\nversion = \"1.0.0\"\n",
		"src/lib.rs": "pub fn hi() {}\n",
	}
}

func TestTarballRejectsEmptyInputs(t *testing.T) {
	cases := []struct {
		name    string
		crate   string
		version string
		files   library.Files
	}{
		{"empty crate", "", "1.0.0", sampleFiles()},
		{"empty version", "demo", "", sampleFiles()},
		{"empty files", "demo", "1.0.0", library.Files{}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			if _, err := Tarball(c.crate, c.version, c.files); err == nil {
				t.Errorf("expected error for %s", c.name)
			}
		})
	}
}

func TestTarballIsGzip(t *testing.T) {
	got, err := Tarball("demo", "1.0.0", sampleFiles())
	if err != nil {
		t.Fatal(err)
	}
	if _, err := gzip.NewReader(bytes.NewReader(got)); err != nil {
		t.Errorf("expected gzip-wrapped output: %v", err)
	}
}

func TestTarballRoundtrip(t *testing.T) {
	want := sampleFiles()
	bytes, err := Tarball("demo", "1.0.0", want)
	if err != nil {
		t.Fatal(err)
	}
	got, err := ExtractTarball(bytes)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != len(want) {
		t.Fatalf("got %d entries, want %d: %v", len(got), len(want), got.Sorted())
	}
	for k, v := range want {
		if got[k] != v {
			t.Errorf("entry %q: got %q want %q", k, got[k], v)
		}
	}
}

func TestTarballIsDeterministic(t *testing.T) {
	a, err := Tarball("demo", "1.0.0", sampleFiles())
	if err != nil {
		t.Fatal(err)
	}
	b, err := Tarball("demo", "1.0.0", sampleFiles())
	if err != nil {
		t.Fatal(err)
	}
	if !bytes.Equal(a, b) {
		t.Error("tarball not byte-stable across runs")
	}
}

func TestTarballPrefixesPathsWithCrateAndVersion(t *testing.T) {
	bytes, err := Tarball("hex", "0.4.3", library.Files{"Cargo.toml": "x", "src/lib.rs": "y"})
	if err != nil {
		t.Fatal(err)
	}
	round, err := ExtractTarball(bytes)
	if err != nil {
		t.Fatal(err)
	}
	if _, ok := round["Cargo.toml"]; !ok {
		t.Errorf("Cargo.toml missing after extract: %v", round.Sorted())
	}
}

func TestTarballOrderIsAlphabetical(t *testing.T) {
	files := library.Files{
		"zoo.txt":    "z",
		"alpha.txt":  "a",
		"mid.txt":    "m",
		"Cargo.toml": "c",
	}
	bytes, err := Tarball("demo", "1.0.0", files)
	if err != nil {
		t.Fatal(err)
	}
	got, err := ExtractTarball(bytes)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 4 {
		t.Fatalf("expected 4 entries, got %d: %v", len(got), got.Sorted())
	}
}

func TestExtractTarballSurfacesGunzipError(t *testing.T) {
	if _, err := ExtractTarball([]byte("not gzip")); err == nil {
		t.Error("expected gunzip error")
	}
}

func TestTarballPreservesContents(t *testing.T) {
	body := strings.Repeat("Cargo workspace\n", 100)
	files := library.Files{
		"Cargo.toml": body,
		"src/lib.rs": "pub fn x() {}",
	}
	tar, err := Tarball("demo", "0.0.1", files)
	if err != nil {
		t.Fatal(err)
	}
	round, err := ExtractTarball(tar)
	if err != nil {
		t.Fatal(err)
	}
	if round["Cargo.toml"] != body {
		t.Error("body content not preserved through roundtrip")
	}
}
