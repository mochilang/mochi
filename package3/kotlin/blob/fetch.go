package blob

import (
	"context"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/mochilang/mochi/package3/kotlin/maven"
)

// allowedContentTypes lists the MIME types accepted for artifact downloads.
var allowedContentTypes = []string{
	"application/java-archive",
	"application/zip",
	"application/octet-stream",
	"text/xml",
	"application/xml",
}

// isAllowedContentType returns true if the Content-Type header is acceptable.
func isAllowedContentType(ct string) bool {
	// Strip parameters (e.g. "text/xml; charset=utf-8")
	if idx := strings.Index(ct, ";"); idx >= 0 {
		ct = strings.TrimSpace(ct[:idx])
	}
	ct = strings.ToLower(strings.TrimSpace(ct))
	for _, allowed := range allowedContentTypes {
		if ct == allowed {
			return true
		}
	}
	return false
}

// FetchArtifact downloads an artifact from a URL into the store.
// It retries up to 3 times with exponential backoff (1s, 2s, 4s).
// It verifies the Content-Type is acceptable before storing.
// Returns the local path, sha256, and blake3 of the stored blob.
func FetchArtifact(
	ctx context.Context,
	httpClient *http.Client,
	store *Store,
	coord maven.Coordinate,
	url string,
	ext string,
) (path string, sha256sum [32]byte, blake3sum [32]byte, err error) {
	const maxRetries = 3
	backoff := time.Second

	for attempt := 0; attempt < maxRetries; attempt++ {
		if attempt > 0 {
			select {
			case <-ctx.Done():
				return "", sha256sum, blake3sum, ctx.Err()
			case <-time.After(backoff):
				backoff *= 2
			}
		}

		path, sha256sum, blake3sum, err = fetchOnce(ctx, httpClient, store, coord, url, ext)
		if err == nil {
			return path, sha256sum, blake3sum, nil
		}

		// Don't retry on context cancellation
		if ctx.Err() != nil {
			return "", sha256sum, blake3sum, ctx.Err()
		}

		// Don't retry on 4xx errors (client errors, not transient)
		if fe, ok := err.(*fetchError); ok && fe.status >= 400 && fe.status < 500 {
			return "", sha256sum, blake3sum, err
		}
	}

	return "", sha256sum, blake3sum, fmt.Errorf("fetch: %s: failed after %d attempts: %w", url, maxRetries, err)
}

// fetchOnce performs a single fetch attempt.
func fetchOnce(
	ctx context.Context,
	httpClient *http.Client,
	store *Store,
	coord maven.Coordinate,
	url string,
	ext string,
) (path string, sha256sum [32]byte, blake3sum [32]byte, err error) {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("fetch: build request: %w", err)
	}

	resp, err := httpClient.Do(req)
	if err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("fetch: GET %s: %w", url, err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		return "", sha256sum, blake3sum, &fetchError{
			url:    url,
			status: resp.StatusCode,
			msg:    fmt.Sprintf("HTTP %d", resp.StatusCode),
		}
	}

	ct := resp.Header.Get("Content-Type")
	if ct != "" && !isAllowedContentType(ct) {
		return "", sha256sum, blake3sum, fmt.Errorf("fetch: unexpected Content-Type %q for %s", ct, url)
	}

	path, sha256sum, blake3sum, err = store.Put(coord, resp.Body, ext)
	if err != nil {
		return "", sha256sum, blake3sum, fmt.Errorf("fetch: store %s: %w", url, err)
	}

	return path, sha256sum, blake3sum, nil
}

// fetchError represents an HTTP error during artifact fetch.
type fetchError struct {
	url    string
	status int
	msg    string
}

func (e *fetchError) Error() string {
	return fmt.Sprintf("fetch: %s: %s", e.url, e.msg)
}
