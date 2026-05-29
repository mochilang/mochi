package sumdb

import (
	"errors"
	"fmt"
	"strconv"
	"strings"
)

// LookupRecord is one parsed response from the sum.golang.org
// /lookup/<module>@<version> endpoint.
//
// The wire format of a lookup response is:
//
//	<id>\n
//	<module> <version> <zipHash>\n
//	<module> <version>/go.mod <modHash>\n
//	\n
//	<signed note containing the tree head>
//
// Where the id is the leaf index in the transparency log.
type LookupRecord struct {
	// ID is the leaf index of this record in the transparency log
	// tree. Combined with the tree head it identifies the inclusion
	// proof position.
	ID int64
	// Module is the module path the proxy resolved (matches the
	// request).
	Module string
	// Version is the resolved version.
	Version string
	// ZipHash is the "h1:..." digest of the .zip artifact.
	ZipHash string
	// ModHash is the "h1:..." digest of the .mod artifact (i.e. the
	// "<v>/go.mod" line of go.sum).
	ModHash string
	// TreeNote is the raw signed-note bytes carrying the tree head
	// that vouches for this record. Verify with Note + VerifierKey.
	TreeNote []byte
}

// ParseLookup parses the body returned by GET /lookup/<m>@<v>.
//
// Returns an error if the framing is wrong, the id field is not a
// decimal integer, the two hash lines do not share the same module
// and version, or the zip / mod h1: digests have wrong format.
func ParseLookup(raw []byte) (*LookupRecord, error) {
	if len(raw) == 0 {
		return nil, errors.New("sumdb: empty lookup body")
	}
	s := string(raw)
	// Split body and signed-tree note at the blank line.
	sep := strings.Index(s, "\n\n")
	if sep < 0 {
		return nil, errors.New("sumdb: lookup missing blank-line separator")
	}
	header := s[:sep]
	tree := s[sep+2:]
	if tree == "" {
		return nil, errors.New("sumdb: lookup missing tree note")
	}
	lines := strings.Split(header, "\n")
	if len(lines) < 3 {
		return nil, fmt.Errorf("sumdb: lookup header has %d lines; want at least 3", len(lines))
	}
	id, err := strconv.ParseInt(lines[0], 10, 64)
	if err != nil {
		return nil, fmt.Errorf("sumdb: lookup id %q: %w", lines[0], err)
	}
	if id < 0 {
		return nil, fmt.Errorf("sumdb: lookup id %d is negative", id)
	}
	mod, ver, zipH, err := parseHashLine(lines[1])
	if err != nil {
		return nil, fmt.Errorf("sumdb: lookup zip-hash line: %w", err)
	}
	if !strings.HasSuffix(zipH, "") || !strings.HasPrefix(zipH, "h1:") {
		return nil, fmt.Errorf("sumdb: lookup zip-hash %q lacks h1: prefix", zipH)
	}
	mod2, ver2, modH, err := parseHashLine(lines[2])
	if err != nil {
		return nil, fmt.Errorf("sumdb: lookup mod-hash line: %w", err)
	}
	if !strings.HasPrefix(modH, "h1:") {
		return nil, fmt.Errorf("sumdb: lookup mod-hash %q lacks h1: prefix", modH)
	}
	// The mod line is "<module> <version>/go.mod <h1:...>". The
	// parseHashLine helper returns ver2 = "<version>/go.mod"; peel
	// off the suffix.
	if mod2 != mod {
		return nil, fmt.Errorf("sumdb: lookup mod %q != zip mod %q", mod2, mod)
	}
	const gomodSuffix = "/go.mod"
	if !strings.HasSuffix(ver2, gomodSuffix) {
		return nil, fmt.Errorf("sumdb: lookup mod-hash line version %q missing /go.mod suffix", ver2)
	}
	bareVer := ver2[:len(ver2)-len(gomodSuffix)]
	if bareVer != ver {
		return nil, fmt.Errorf("sumdb: lookup mod-hash version %q != zip-hash version %q", bareVer, ver)
	}
	return &LookupRecord{
		ID:       id,
		Module:   mod,
		Version:  ver,
		ZipHash:  zipH,
		ModHash:  modH,
		TreeNote: []byte(tree),
	}, nil
}

func parseHashLine(line string) (module, version, hash string, err error) {
	parts := strings.Fields(line)
	if len(parts) != 3 {
		return "", "", "", fmt.Errorf("expected 3 whitespace-separated fields, got %d in %q", len(parts), line)
	}
	return parts[0], parts[1], parts[2], nil
}
