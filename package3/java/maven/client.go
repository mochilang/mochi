package maven

import (
	"context"
	"crypto/sha1"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

// DefaultBaseURL is the Maven Central repository root.
const DefaultBaseURL = "https://repo1.maven.org/maven2/"

// DefaultUserAgent identifies the Mochi bridge to Maven Central servers.
const DefaultUserAgent = "mochi-java-bridge/0.1 (+https://mochi-lang.dev)"

// ErrArtifactNotFound is returned when Maven Central responds 404 for an artifact.
var ErrArtifactNotFound = errors.New("maven: artifact not found")

// ErrChecksumMismatch is returned when the downloaded JAR's SHA-1 does not match.
var ErrChecksumMismatch = errors.New("maven: checksum mismatch")

// Client is a Maven Central HTTP client. It is safe for concurrent use.
type Client struct {
	// BaseURL is the Maven repository root. Must end in a slash.
	BaseURL string
	// HTTP is the underlying transport. nil uses a 30s-timeout default client.
	HTTP *http.Client
	// UserAgent is sent as the User-Agent header.
	UserAgent string
}

// NewClient returns a Client pre-configured against Maven Central.
func NewClient(baseURL string) *Client {
	if baseURL == "" {
		baseURL = DefaultBaseURL
	}
	if !strings.HasSuffix(baseURL, "/") {
		baseURL += "/"
	}
	return &Client{
		BaseURL:   baseURL,
		HTTP:      &http.Client{Timeout: 60 * time.Second},
		UserAgent: DefaultUserAgent,
	}
}

// FetchMetadata fetches maven-metadata.xml for the coordinate and parses it.
func (c *Client) FetchMetadata(ctx context.Context, coord Coordinate) (*VersionMetadata, error) {
	url := c.BaseURL + coord.MetadataPath()
	data, err := c.getBytes(ctx, url)
	if err != nil {
		return nil, err
	}
	return parseMetadataXML(data)
}

// FetchJAR downloads the JAR for the coordinate, verifies its SHA-1 checksum,
// and returns the raw bytes.
func (c *Client) FetchJAR(ctx context.Context, coord Coordinate) ([]byte, error) {
	jarURL := c.BaseURL + coord.ArtifactPath()
	sha1URL := c.BaseURL + coord.SHA1Path()

	// Fetch checksum first.
	checksumHex, err := c.FetchChecksum(ctx, sha1URL)
	if err != nil {
		return nil, fmt.Errorf("maven: fetch sha1 for %s: %w", coord, err)
	}

	// Fetch the JAR.
	data, err := c.getBytes(ctx, jarURL)
	if err != nil {
		return nil, err
	}

	// Verify SHA-1.
	h := sha1.New()
	h.Write(data)
	got := fmt.Sprintf("%x", h.Sum(nil))
	if got != checksumHex {
		return nil, fmt.Errorf("%w: %s: want %s got %s", ErrChecksumMismatch, coord, checksumHex, got)
	}
	return data, nil
}

// FetchPOM downloads the POM for the coordinate.
func (c *Client) FetchPOM(ctx context.Context, coord Coordinate) ([]byte, error) {
	url := c.BaseURL + coord.POMPath()
	return c.getBytes(ctx, url)
}

// FetchChecksum downloads a .sha1 or .sha256 file and returns the hex hash string.
func (c *Client) FetchChecksum(ctx context.Context, url string) (string, error) {
	data, err := c.getBytes(ctx, url)
	if err != nil {
		return "", err
	}
	// Some servers include spaces or newlines; trim whitespace.
	// Some also include the filename after the hash.
	raw := strings.TrimSpace(string(data))
	// If there's whitespace in the middle (e.g. "abc123  guava.jar"), take the first token.
	if idx := strings.IndexAny(raw, " \t"); idx > 0 {
		raw = raw[:idx]
	}
	return raw, nil
}

// getBytes performs a GET request and returns the response body as bytes.
// Returns ErrArtifactNotFound on 404.
func (c *Client) getBytes(ctx context.Context, url string) ([]byte, error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("maven: build request: %w", err)
	}
	if c.UserAgent != "" {
		req.Header.Set("User-Agent", c.UserAgent)
	}
	cl := c.HTTP
	if cl == nil {
		cl = http.DefaultClient
	}
	resp, err := cl.Do(req)
	if err != nil {
		return nil, fmt.Errorf("maven: GET %s: %w", url, err)
	}
	defer resp.Body.Close()
	switch resp.StatusCode {
	case http.StatusOK:
		// fall through
	case http.StatusNotFound:
		return nil, fmt.Errorf("%w: %s", ErrArtifactNotFound, url)
	default:
		body, _ := io.ReadAll(io.LimitReader(resp.Body, 1024))
		return nil, fmt.Errorf("maven: GET %s: status %d: %s", url, resp.StatusCode, strings.TrimSpace(string(body)))
	}
	return io.ReadAll(resp.Body)
}
