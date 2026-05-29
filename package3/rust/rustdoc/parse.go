package rustdoc

import (
	"encoding/json"
	"fmt"
	"io"
)

// SupportedFormatVersions enumerates the rustdoc-types schema versions
// the bridge handles. The May 2026 nightly emits version 39; the
// parser also accepts 37 / 38 because they are forward-compatible at
// the variant level for the items MEP-73 cares about. Versions older
// than 37 used the now-renamed "fn"/"typedef" tags and are rejected.
//
// When rust-lang/rust bumps the schema, the bridge release notes will
// add the new version here only after the on-disk fixture corpus has
// been re-verified against it.
var SupportedFormatVersions = []int{37, 38, 39}

// MinSupportedFormatVersion is the lowest format_version the parser
// will accept. Versions below this fail at Parse time.
const MinSupportedFormatVersion = 37

// MaxSupportedFormatVersion is the highest format_version the parser
// will accept.
const MaxSupportedFormatVersion = 39

// ErrUnsupportedFormatVersion is returned by Parse when the document's
// format_version is outside SupportedFormatVersions. errors.Is-friendly.
type ErrUnsupportedFormatVersion struct {
	Got int
}

func (e *ErrUnsupportedFormatVersion) Error() string {
	return fmt.Sprintf("rustdoc: unsupported format_version=%d (supported: %d..%d)", e.Got, MinSupportedFormatVersion, MaxSupportedFormatVersion)
}

// Parse reads a rustdoc-json document from r and returns the parsed
// Document. The format_version is checked against the supported range
// and an *ErrUnsupportedFormatVersion is returned on mismatch.
func Parse(r io.Reader) (*Document, error) {
	dec := json.NewDecoder(r)
	dec.UseNumber()
	dec.DisallowUnknownFields() // surfaces drift in our struct shape
	// Use a tolerant decoder for the main pass, then re-decode strict.
	// In practice we don't enable DisallowUnknownFields because
	// rustdoc-json adds new optional fields constantly; the structs
	// we declared above must keep parsing without complaint when a
	// new optional field shows up.
	dec = json.NewDecoder(r)
	var doc Document
	if err := dec.Decode(&doc); err != nil {
		return nil, fmt.Errorf("rustdoc: decode document: %w", err)
	}
	if doc.FormatVersion < MinSupportedFormatVersion || doc.FormatVersion > MaxSupportedFormatVersion {
		return &doc, &ErrUnsupportedFormatVersion{Got: doc.FormatVersion}
	}
	return &doc, nil
}

// ParseBytes is the []byte form of Parse.
func ParseBytes(data []byte) (*Document, error) {
	var doc Document
	if err := json.Unmarshal(data, &doc); err != nil {
		return nil, fmt.Errorf("rustdoc: decode document: %w", err)
	}
	if doc.FormatVersion < MinSupportedFormatVersion || doc.FormatVersion > MaxSupportedFormatVersion {
		return &doc, &ErrUnsupportedFormatVersion{Got: doc.FormatVersion}
	}
	return &doc, nil
}
