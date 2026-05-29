package maven

import (
	"archive/zip"
	"bytes"
	"context"
	"crypto/sha1"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
)

// minimalJAR returns a minimal valid JAR (ZIP) with only META-INF/MANIFEST.MF.
func minimalJAR() []byte {
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)
	mf, _ := w.Create("META-INF/MANIFEST.MF")
	io.WriteString(mf, "Manifest-Version: 1.0\n")
	w.Close()
	return buf.Bytes()
}

func sha1Hex(data []byte) string {
	h := sha1.New()
	h.Write(data)
	return fmt.Sprintf("%x", h.Sum(nil))
}

func TestClientFetchJAR(t *testing.T) {
	jarData := minimalJAR()
	checksum := sha1Hex(jarData)

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case r.URL.Path == "/com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar":
			w.Write(jarData)
		case r.URL.Path == "/com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar.sha1":
			fmt.Fprintf(w, "%s  guava-33.4.8-jre.jar\n", checksum)
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	coord := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	data, err := client.FetchJAR(context.Background(), coord)
	if err != nil {
		t.Fatalf("FetchJAR: %v", err)
	}
	if !bytes.Equal(data, jarData) {
		t.Errorf("FetchJAR returned wrong data")
	}
}

func TestClientFetch404(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		http.NotFound(w, r)
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	coord := Coordinate{GroupID: "com.example", ArtifactID: "missing", Version: "1.0"}
	_, err := client.FetchJAR(context.Background(), coord)
	if !errors.Is(err, ErrArtifactNotFound) {
		t.Errorf("FetchJAR 404: got %v; want ErrArtifactNotFound", err)
	}
}

func TestClientFetchChecksumMismatch(t *testing.T) {
	jarData := minimalJAR()

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch {
		case r.URL.Path == "/com/example/foo/1.0/foo-1.0.jar":
			w.Write(jarData)
		case r.URL.Path == "/com/example/foo/1.0/foo-1.0.jar.sha1":
			// Return wrong checksum.
			fmt.Fprintf(w, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef\n")
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	coord := Coordinate{GroupID: "com.example", ArtifactID: "foo", Version: "1.0"}
	_, err := client.FetchJAR(context.Background(), coord)
	if !errors.Is(err, ErrChecksumMismatch) {
		t.Errorf("FetchJAR mismatch: got %v; want ErrChecksumMismatch", err)
	}
}

func TestClientFetchMetadata(t *testing.T) {
	xmlData := []byte(fixtureMetadataXML)
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path == "/com/google/guava/guava/maven-metadata.xml" {
			w.Header().Set("Content-Type", "application/xml")
			w.Write(xmlData)
		} else {
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	coord := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	meta, err := client.FetchMetadata(context.Background(), coord)
	if err != nil {
		t.Fatalf("FetchMetadata: %v", err)
	}
	if len(meta.Versions) != 3 {
		t.Errorf("len(Versions) = %d; want 3", len(meta.Versions))
	}
	if meta.LatestRelease != "33.4.8-jre" {
		t.Errorf("LatestRelease = %q; want 33.4.8-jre", meta.LatestRelease)
	}
}

func TestClientFetchChecksum(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "abc123  guava.jar\n")
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	got, err := client.FetchChecksum(context.Background(), srv.URL+"/checksum.sha1")
	if err != nil {
		t.Fatalf("FetchChecksum: %v", err)
	}
	if got != "abc123" {
		t.Errorf("FetchChecksum = %q; want abc123", got)
	}
}
