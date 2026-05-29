package blob

import (
	"bytes"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/mochilang/mochi/package3/kotlin/maven"
)

func testCoord(artifactID string) maven.Coordinate {
	return maven.Coordinate{
		GroupID:    "org.example",
		ArtifactID: artifactID,
		Version:    "1.0.0",
	}
}

func TestPutGetRoundTrip(t *testing.T) {
	root := t.TempDir()
	store := NewStore(root)

	content := []byte("hello, mochi kotlin bridge!")
	coord := testCoord("mylib")

	path, sha256sum, blake3sum, err := store.Put(coord, bytes.NewReader(content), "jar")
	if err != nil {
		t.Fatalf("Put error: %v", err)
	}
	if path == "" {
		t.Fatal("Put returned empty path")
	}

	// Verify SHA-256 matches
	expectedSHA, expectedB3, err := HashReader(bytes.NewReader(content))
	if err != nil {
		t.Fatalf("HashReader error: %v", err)
	}
	if sha256sum != expectedSHA {
		t.Errorf("SHA256 mismatch: got %x, want %x", sha256sum, expectedSHA)
	}
	if blake3sum != expectedB3 {
		t.Errorf("BLAKE3 mismatch: got %x, want %x", blake3sum, expectedB3)
	}

	// GetBySHA256 should find the blob
	gotPath, ok := store.GetBySHA256(sha256sum)
	if !ok {
		t.Fatal("GetBySHA256 returned false")
	}
	if gotPath != path {
		t.Errorf("GetBySHA256 path = %q, want %q", gotPath, path)
	}
}

func TestPutIdempotent(t *testing.T) {
	root := t.TempDir()
	store := NewStore(root)

	content := []byte("idempotent content for testing")
	coord := testCoord("mylib")

	path1, sha1, _, err := store.Put(coord, bytes.NewReader(content), "jar")
	if err != nil {
		t.Fatalf("First Put error: %v", err)
	}

	path2, sha2, _, err := store.Put(coord, bytes.NewReader(content), "jar")
	if err != nil {
		t.Fatalf("Second Put error: %v", err)
	}

	if path1 != path2 {
		t.Errorf("paths differ: %q vs %q", path1, path2)
	}
	if sha1 != sha2 {
		t.Errorf("sha256 differ")
	}
}

func TestSaveLoadMeta(t *testing.T) {
	root := t.TempDir()
	store := NewStore(root)

	sha256hex := strings.Repeat("ab", 32)
	blake3hex := strings.Repeat("cd", 32)

	coord := testCoord("mylib")
	meta := ArtifactMeta{
		Coord:     coord,
		SHA256:    sha256hex,
		BLAKE3:    blake3hex,
		JARPath:   "/tmp/mylib.jar",
		POMPath:   "/tmp/mylib.pom",
		FetchedAt: time.Now().Truncate(time.Second),
		Packaging: "jar",
	}

	if err := store.SaveMeta(meta); err != nil {
		t.Fatalf("SaveMeta error: %v", err)
	}

	loaded, err := store.LoadMeta(coord)
	if err != nil {
		t.Fatalf("LoadMeta error: %v", err)
	}

	if loaded.SHA256 != meta.SHA256 {
		t.Errorf("SHA256: got %q, want %q", loaded.SHA256, meta.SHA256)
	}
	if loaded.BLAKE3 != meta.BLAKE3 {
		t.Errorf("BLAKE3: got %q, want %q", loaded.BLAKE3, meta.BLAKE3)
	}
	if loaded.JARPath != meta.JARPath {
		t.Errorf("JARPath: got %q, want %q", loaded.JARPath, meta.JARPath)
	}
	if loaded.Packaging != meta.Packaging {
		t.Errorf("Packaging: got %q, want %q", loaded.Packaging, meta.Packaging)
	}
}

func TestLoadMetaNotFound(t *testing.T) {
	root := t.TempDir()
	store := NewStore(root)

	_, err := store.LoadMeta(testCoord("nonexistent"))
	if err == nil {
		t.Error("expected error for missing meta, got nil")
	}
}

func TestConcurrentPut(t *testing.T) {
	root := t.TempDir()
	store := NewStore(root)

	const goroutines = 10
	content := []byte("concurrent test content for race detection")

	var wg sync.WaitGroup
	errs := make(chan error, goroutines)
	paths := make(chan string, goroutines)

	for i := 0; i < goroutines; i++ {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			// Each goroutine uses a slightly different content to test different blobs
			data := append(content, []byte(fmt.Sprintf("-%d", n))...)
			c := maven.Coordinate{
				GroupID:    "org.example",
				ArtifactID: fmt.Sprintf("mylib-%d", n),
				Version:    "1.0.0",
			}
			path, _, _, err := store.Put(c, bytes.NewReader(data), "jar")
			if err != nil {
				errs <- err
				return
			}
			paths <- path
		}(i)
	}

	wg.Wait()
	close(errs)
	close(paths)

	for err := range errs {
		t.Errorf("concurrent Put error: %v", err)
	}

	// All paths should be non-empty and distinct (different content)
	seen := make(map[string]bool)
	for p := range paths {
		if p == "" {
			t.Error("got empty path from concurrent Put")
		}
		seen[p] = true
	}
	if len(seen) != goroutines {
		t.Errorf("expected %d distinct paths, got %d", goroutines, len(seen))
	}
}

func TestConcurrentPutSameContent(t *testing.T) {
	// Multiple goroutines writing the same content should all get the same path.
	root := t.TempDir()
	store := NewStore(root)

	content := []byte("same content from many goroutines")
	coord := testCoord("mylib")

	const goroutines = 20
	var wg sync.WaitGroup
	paths := make(chan string, goroutines)
	errs := make(chan error, goroutines)

	for i := 0; i < goroutines; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			path, _, _, err := store.Put(coord, bytes.NewReader(content), "jar")
			if err != nil {
				errs <- err
				return
			}
			paths <- path
		}()
	}

	wg.Wait()
	close(paths)
	close(errs)

	for err := range errs {
		t.Errorf("concurrent Put error: %v", err)
	}

	var firstPath string
	for p := range paths {
		if firstPath == "" {
			firstPath = p
		} else if p != firstPath {
			t.Errorf("paths differ: %q vs %q", firstPath, p)
		}
	}
}
