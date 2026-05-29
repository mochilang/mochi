package moduleproxy

import (
	"archive/zip"
	"bytes"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"fmt"
	"io"
	"strings"
	"testing"
)

func TestHash1FilesEmptyList(t *testing.T) {
	open := func(string) (io.ReadCloser, error) { return nil, fmt.Errorf("no opens expected") }
	got, err := Hash1Files(nil, open)
	if err != nil {
		t.Fatalf("Hash1Files: %v", err)
	}
	// sha256("") = e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
	want := H1Prefix + base64.StdEncoding.EncodeToString(sha256.New().Sum(nil))
	if got != want {
		t.Errorf("Hash1Files(nil) = %q; want %q", got, want)
	}
}

func TestHash1FilesNilOpen(t *testing.T) {
	if _, err := Hash1Files([]string{"x"}, nil); err == nil {
		t.Errorf("Hash1Files with nil open returned nil; want error")
	}
}

func TestHash1FilesOneFileMatchesReference(t *testing.T) {
	content := []byte("hello\n")
	open := func(name string) (io.ReadCloser, error) {
		if name != "foo@v1/a.txt" {
			return nil, fmt.Errorf("unexpected open %q", name)
		}
		return io.NopCloser(bytes.NewReader(content)), nil
	}
	got, err := Hash1Files([]string{"foo@v1/a.txt"}, open)
	if err != nil {
		t.Fatalf("Hash1Files: %v", err)
	}
	innerHash := sha256.Sum256(content)
	innerLine := fmt.Sprintf("%s  foo@v1/a.txt\n", hex.EncodeToString(innerHash[:]))
	outerHash := sha256.Sum256([]byte(innerLine))
	want := H1Prefix + base64.StdEncoding.EncodeToString(outerHash[:])
	if got != want {
		t.Errorf("Hash1Files mismatch:\n  got  = %q\n  want = %q", got, want)
	}
}

func TestHash1FilesSortInsensitiveToInputOrder(t *testing.T) {
	files1 := []string{"a", "b", "c"}
	files2 := []string{"c", "a", "b"}
	open := func(name string) (io.ReadCloser, error) {
		return io.NopCloser(strings.NewReader("x")), nil
	}
	h1, err := Hash1Files(files1, open)
	if err != nil {
		t.Fatalf("Hash1Files(files1): %v", err)
	}
	h2, err := Hash1Files(files2, open)
	if err != nil {
		t.Fatalf("Hash1Files(files2): %v", err)
	}
	if h1 != h2 {
		t.Errorf("Hash1Files not order-insensitive: %q vs %q", h1, h2)
	}
}

func TestHash1FilesPropagatesOpenError(t *testing.T) {
	open := func(string) (io.ReadCloser, error) {
		return nil, fmt.Errorf("boom")
	}
	if _, err := Hash1Files([]string{"x"}, open); err == nil {
		t.Errorf("Hash1Files swallowed open error; want propagation")
	}
}

func TestHashGoModMatchesReference(t *testing.T) {
	modBytes := []byte("module example.com/foo\n\ngo 1.21\n")
	got := HashGoMod(modBytes)

	inner := sha256.Sum256(modBytes)
	innerLine := fmt.Sprintf("%s  go.mod\n", hex.EncodeToString(inner[:]))
	outer := sha256.Sum256([]byte(innerLine))
	want := H1Prefix + base64.StdEncoding.EncodeToString(outer[:])

	if got != want {
		t.Errorf("HashGoMod mismatch:\n  got  = %q\n  want = %q", got, want)
	}
}

func TestHashGoModStable(t *testing.T) {
	modBytes := []byte("module example.com/foo\n\ngo 1.21\n")
	first := HashGoMod(modBytes)
	for i := range 5 {
		if got := HashGoMod(modBytes); got != first {
			t.Errorf("iter %d: HashGoMod produced %q (was %q)", i, got, first)
		}
	}
}

func TestHashZipMatchesHash1Files(t *testing.T) {
	zipBuf := mustBuildZip(t, map[string]string{
		"example.com/foo@v1.2.3/go.mod":   "module example.com/foo\n\ngo 1.21\n",
		"example.com/foo@v1.2.3/main.go":  "package main\n\nfunc main() {}\n",
		"example.com/foo@v1.2.3/LICENSE":  "MIT\n",
	})

	got, err := HashZip(bytes.NewReader(zipBuf), "example.com/foo", "v1.2.3")
	if err != nil {
		t.Fatalf("HashZip: %v", err)
	}

	// Independent reference: re-parse the zip, gather (name, content)
	// pairs, run Hash1Files over them. Should match exactly.
	zr, err := zip.NewReader(bytes.NewReader(zipBuf), int64(len(zipBuf)))
	if err != nil {
		t.Fatalf("zip.NewReader: %v", err)
	}
	contents := map[string][]byte{}
	names := []string{}
	for _, f := range zr.File {
		if f.FileInfo().IsDir() {
			continue
		}
		rc, err := f.Open()
		if err != nil {
			t.Fatalf("open zip entry: %v", err)
		}
		buf, err := io.ReadAll(rc)
		rc.Close()
		if err != nil {
			t.Fatalf("read zip entry: %v", err)
		}
		contents[f.Name] = buf
		names = append(names, f.Name)
	}
	open := func(name string) (io.ReadCloser, error) {
		return io.NopCloser(bytes.NewReader(contents[name])), nil
	}
	want, err := Hash1Files(names, open)
	if err != nil {
		t.Fatalf("reference Hash1Files: %v", err)
	}
	if got != want {
		t.Errorf("HashZip mismatch:\n  got  = %q\n  want = %q", got, want)
	}
}

func TestHashZipRejectsMismatchedPrefix(t *testing.T) {
	zipBuf := mustBuildZip(t, map[string]string{
		"actual.module/foo@v1/main.go": "package main\n",
	})
	if _, err := HashZip(bytes.NewReader(zipBuf), "claimed/module/foo", "v1"); err == nil {
		t.Errorf("HashZip accepted mismatched zip prefix; want error")
	}
}

func TestHashZipPropagatesParseError(t *testing.T) {
	if _, err := HashZip(bytes.NewReader([]byte("not a zip")), "x", "v1"); err == nil {
		t.Errorf("HashZip accepted non-zip input; want error")
	}
}

func TestHashZipSkipsDirectoryEntries(t *testing.T) {
	// Build a zip with a directory entry plus a file. The directory
	// must not contribute to the hash.
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	_, err := zw.Create("example.com/foo@v1/subdir/")
	if err != nil {
		t.Fatalf("Create dir: %v", err)
	}
	fw, err := zw.Create("example.com/foo@v1/main.go")
	if err != nil {
		t.Fatalf("Create file: %v", err)
	}
	fw.Write([]byte("package main\n"))
	if err := zw.Close(); err != nil {
		t.Fatalf("zw.Close: %v", err)
	}

	got, err := HashZip(bytes.NewReader(buf.Bytes()), "example.com/foo", "v1")
	if err != nil {
		t.Fatalf("HashZip: %v", err)
	}
	// Equivalent zip without the directory entry should produce the same hash.
	want, err := HashZip(bytes.NewReader(mustBuildZip(t, map[string]string{
		"example.com/foo@v1/main.go": "package main\n",
	})), "example.com/foo", "v1")
	if err != nil {
		t.Fatalf("HashZip ref: %v", err)
	}
	if got != want {
		t.Errorf("directory entry affected hash: got %q want %q", got, want)
	}
}

// mustBuildZip is a test helper that builds an in-memory zip with the
// given filename → contents map. Returns the encoded bytes. Fails the
// test on any error.
func mustBuildZip(t *testing.T, files map[string]string) []byte {
	t.Helper()
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	for name, content := range files {
		fw, err := zw.Create(name)
		if err != nil {
			t.Fatalf("zw.Create(%q): %v", name, err)
		}
		if _, err := fw.Write([]byte(content)); err != nil {
			t.Fatalf("write %q: %v", name, err)
		}
	}
	if err := zw.Close(); err != nil {
		t.Fatalf("zw.Close: %v", err)
	}
	return buf.Bytes()
}
