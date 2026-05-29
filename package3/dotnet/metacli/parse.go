package metacli

import (
	"encoding/json"
	"fmt"
	"io"
)

// Parse parses the JSON output of mochi-dotnet-meta from r.
// Returns a non-nil *AssemblyMetadata on success.
func Parse(r io.Reader) (*AssemblyMetadata, error) {
	var meta AssemblyMetadata
	dec := json.NewDecoder(r)
	dec.DisallowUnknownFields()
	if err := dec.Decode(&meta); err != nil {
		return nil, fmt.Errorf("metacli: decode assembly metadata: %w", err)
	}
	return &meta, nil
}

// ParseBytes parses the JSON output of mochi-dotnet-meta from b.
// Returns a non-nil *AssemblyMetadata on success.
func ParseBytes(b []byte) (*AssemblyMetadata, error) {
	var meta AssemblyMetadata
	if err := json.Unmarshal(b, &meta); err != nil {
		return nil, fmt.Errorf("metacli: decode assembly metadata: %w", err)
	}
	return &meta, nil
}
