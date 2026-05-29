package blob

import (
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sync"
	"time"

	"github.com/mochilang/mochi/package3/kotlin/maven"
)

// ArtifactMeta holds metadata about a cached artifact.
type ArtifactMeta struct {
	Coord     maven.Coordinate `json:"coord"`
	SHA256    string           `json:"sha256"`   // hex
	BLAKE3    string           `json:"blake3"`   // hex
	JARPath   string           `json:"jar_path"` // absolute path to the JAR blob
	POMPath   string           `json:"pom_path"` // absolute path to the POM blob
	FetchedAt time.Time        `json:"fetched_at"`
	Packaging string           `json:"packaging"` // "jar" or "aar"
}

// Store is a content-addressed local cache for Maven artifacts.
//
// Layout:
//
//	<root>/blobs/sha256/<aa>/<aabbcc...>  (the actual bytes)
//	<root>/meta/<groupId>/<artifactId>/<version>.json  (ArtifactMeta)
type Store struct {
	root string
	mu   sync.Mutex
}

// DefaultRoot returns the default cache root (~/.cache/mochi/kotlin-deps).
func DefaultRoot() string {
	home, err := os.UserHomeDir()
	if err != nil {
		home = os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "kotlin-deps")
}

// NewStore creates a new Store rooted at the given directory.
// The directory is created if it does not exist.
func NewStore(root string) *Store {
	return &Store{root: root}
}

// blobDir returns the directory for a SHA-256 hash (using first two hex chars as subdir).
func (s *Store) blobDir(sha256sum [32]byte) string {
	h := hex.EncodeToString(sha256sum[:])
	return filepath.Join(s.root, "blobs", "sha256", h[:2])
}

// blobPath returns the full path for a blob identified by its SHA-256.
func (s *Store) blobPath(sha256sum [32]byte) string {
	h := hex.EncodeToString(sha256sum[:])
	return filepath.Join(s.root, "blobs", "sha256", h[:2], h)
}

// metaPath returns the path for a coordinate's metadata JSON.
func (s *Store) metaPath(coord maven.Coordinate) string {
	return filepath.Join(s.root, "meta",
		coord.GroupID, coord.ArtifactID, coord.Version+".json")
}

// Put writes content from r into the store atomically.
// If the blob already exists (same SHA-256), it is not re-written.
// Returns the blob path, SHA-256, and BLAKE3 of the stored content.
func (s *Store) Put(coord maven.Coordinate, r io.Reader, ext string) (path string, sha256sum [32]byte, blake3sum [32]byte, err error) {
	// Write to a temp file first so we can hash it
	tmpDir := filepath.Join(s.root, "tmp")
	if err = os.MkdirAll(tmpDir, 0755); err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: mkdir tmp: %w", err)
	}

	tmp, err := os.CreateTemp(tmpDir, "blob-*")
	if err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: create temp: %w", err)
	}
	tmpPath := tmp.Name()
	defer func() {
		// Clean up temp file on failure
		if err != nil {
			os.Remove(tmpPath)
		}
	}()

	// Hash the content while writing to temp file using a pipe + goroutine
	pr, pw := io.Pipe()
	var hashErr error
	var sha [32]byte
	var b3 [32]byte
	done := make(chan struct{})
	go func() {
		defer close(done)
		sha, b3, hashErr = HashReader(pr)
	}()

	// TeeReader writes to the pipe (which goes to the hasher goroutine) while
	// io.Copy writes to the temp file.
	tee := io.TeeReader(r, pw)
	_, copyErr := io.Copy(tmp, tee)
	pw.Close()
	<-done
	tmp.Close()

	if copyErr != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: write blob: %w", copyErr)
	}
	if hashErr != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: hash blob: %w", hashErr)
	}

	copy(sha256sum[:], sha[:])
	copy(blake3sum[:], b3[:])

	// Check if already exists
	destPath := s.blobPath(sha256sum)
	s.mu.Lock()
	defer s.mu.Unlock()

	if _, statErr := os.Stat(destPath); statErr == nil {
		// Already exists, remove the temp file
		os.Remove(tmpPath)
		return destPath, sha256sum, blake3sum, nil
	}

	// Create blob directory
	if err = os.MkdirAll(s.blobDir(sha256sum), 0755); err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: mkdir blob: %w", err)
	}

	// Atomic rename
	if err = os.Rename(tmpPath, destPath); err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("store: rename to blob: %w", err)
	}

	return destPath, sha256sum, blake3sum, nil
}

// GetBySHA256 returns the path to a blob by its SHA-256 hash.
func (s *Store) GetBySHA256(hash [32]byte) (path string, ok bool) {
	p := s.blobPath(hash)
	if _, err := os.Stat(p); err == nil {
		return p, true
	}
	return "", false
}

// Has returns true if a blob with the given SHA-256 hash exists in the store.
func (s *Store) Has(sha256sum [32]byte) bool {
	_, ok := s.GetBySHA256(sha256sum)
	return ok
}

// SaveMeta writes artifact metadata to the store.
func (s *Store) SaveMeta(meta ArtifactMeta) error {
	p := s.metaPath(meta.Coord)
	if err := os.MkdirAll(filepath.Dir(p), 0755); err != nil {
		return fmt.Errorf("store: mkdir meta: %w", err)
	}

	data, err := json.MarshalIndent(meta, "", "  ")
	if err != nil {
		return fmt.Errorf("store: marshal meta: %w", err)
	}

	// Write atomically via temp file
	tmpPath := p + ".tmp"
	if err := os.WriteFile(tmpPath, data, 0644); err != nil {
		return fmt.Errorf("store: write meta: %w", err)
	}
	if err := os.Rename(tmpPath, p); err != nil {
		os.Remove(tmpPath)
		return fmt.Errorf("store: rename meta: %w", err)
	}
	return nil
}

// LoadMeta loads artifact metadata from the store.
func (s *Store) LoadMeta(coord maven.Coordinate) (*ArtifactMeta, error) {
	p := s.metaPath(coord)
	data, err := os.ReadFile(p)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, fmt.Errorf("store: meta not found for %s", coord)
		}
		return nil, fmt.Errorf("store: read meta: %w", err)
	}

	var meta ArtifactMeta
	if err := json.Unmarshal(data, &meta); err != nil {
		return nil, fmt.Errorf("store: unmarshal meta: %w", err)
	}
	return &meta, nil
}
