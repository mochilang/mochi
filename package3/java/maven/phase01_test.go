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

// TestPhase1MavenClient is the phase-gate sentinel test for MEP-67 phase 1.
// It verifies the Maven Central client, coordinate parsing, and metadata parsing
// work end-to-end with an httptest server.
func TestPhase1MavenClient(t *testing.T) {
	jar := makeTestJAR()
	sha1Checksum := fmt.Sprintf("%x", sha1Sum(jar))
	xmlData := []byte(fixtureMetadataXML)

	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar":
			w.Write(jar)
		case "/com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar.sha1":
			fmt.Fprintf(w, "%s\n", sha1Checksum)
		case "/com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.pom":
			fmt.Fprintf(w, "<project><groupId>com.google.guava</groupId></project>")
		case "/com/google/guava/guava/maven-metadata.xml":
			w.Write(xmlData)
		default:
			http.NotFound(w, r)
		}
	}))
	defer srv.Close()

	client := NewClient(srv.URL + "/")
	coord, err := Parse("com.google.guava:guava@33.4.8-jre")
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}

	t.Run("fetch_jar", func(t *testing.T) {
		data, err := client.FetchJAR(context.Background(), coord)
		if err != nil {
			t.Fatalf("FetchJAR: %v", err)
		}
		if !bytes.Equal(data, jar) {
			t.Errorf("FetchJAR returned wrong data")
		}
	})

	t.Run("fetch_pom", func(t *testing.T) {
		pom, err := client.FetchPOM(context.Background(), coord)
		if err != nil {
			t.Fatalf("FetchPOM: %v", err)
		}
		if len(pom) == 0 {
			t.Error("FetchPOM returned empty data")
		}
	})

	t.Run("fetch_metadata", func(t *testing.T) {
		meta, err := client.FetchMetadata(context.Background(), coord)
		if err != nil {
			t.Fatalf("FetchMetadata: %v", err)
		}
		if len(meta.Versions) == 0 {
			t.Error("FetchMetadata returned no versions")
		}
		if meta.LatestRelease == "" {
			t.Error("FetchMetadata returned empty LatestRelease")
		}
	})

	t.Run("not_found", func(t *testing.T) {
		missing, _ := Parse("com.missing:lib@1.0")
		_, err := client.FetchJAR(context.Background(), missing)
		if !errors.Is(err, ErrArtifactNotFound) {
			t.Errorf("expected ErrArtifactNotFound, got %v", err)
		}
	})

	t.Run("coord_parse_both_formats", func(t *testing.T) {
		c1, err := Parse("com.google.guava:guava@33.4.8-jre")
		if err != nil {
			t.Fatalf("Parse mochi format: %v", err)
		}
		c2, err := Parse("com.google.guava:guava:33.4.8-jre")
		if err != nil {
			t.Fatalf("Parse maven format: %v", err)
		}
		if c1.GroupID != c2.GroupID || c1.ArtifactID != c2.ArtifactID || c1.Version != c2.Version {
			t.Error("both formats should produce same coordinate")
		}
	})
}

func makeTestJAR() []byte {
	var buf bytes.Buffer
	w := zip.NewWriter(&buf)
	mf, _ := w.Create("META-INF/MANIFEST.MF")
	io.WriteString(mf, "Manifest-Version: 1.0\nCreated-By: mochi-test\n")
	w.Close()
	return buf.Bytes()
}

func sha1Sum(data []byte) []byte {
	h := sha1.New()
	h.Write(data)
	return h.Sum(nil)
}
