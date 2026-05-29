package simple

import (
	"fmt"
	"regexp"
	"strings"
)

// Project is the parsed contents of a simple-index project listing
// (`/simple/<name>/`). It is the unified representation produced by both
// the PEP 503 HTML parser and the PEP 691 JSON parser.
type Project struct {
	// Name is the canonical PEP 503 normalised project name (lowercase,
	// runs of `[-_.]` collapsed to "-"). Populated by Normalise.
	Name string
	// Files lists every distribution file the index reports for the project,
	// in the order the parser saw them. The bridge sorts and dedupes per
	// version + ABI tag downstream.
	Files []File
	// Meta echoes the `meta` object from PEP 691 JSON. Empty for PEP 503 HTML.
	Meta Meta
}

// File is a single distribution file (sdist or wheel).
type File struct {
	// Filename is the file's basename (the wheel or sdist name, e.g.
	// "httpx-0.27.0-py3-none-any.whl"). The bridge parses the wheel filename
	// downstream to recover the python-tag, abi-tag, and platform-tag.
	Filename string
	// URL is the absolute download URL.
	URL string
	// Hashes lists the file's content hashes, keyed by algorithm
	// ("sha256", "blake3", "md5"). PEP 503 typically supplies only sha256
	// via the `#sha256=...` fragment; PEP 691 may supply multiple.
	Hashes map[string]string
	// RequiresPython is the optional "requires-python" specifier the
	// index advertised for this file. Empty when absent.
	RequiresPython string
	// Yanked is true when PEP 592 advertises the file as yanked. The
	// optional reason is in YankedReason.
	Yanked       bool
	YankedReason string
	// CoreMetadata is the optional advertised flag for PEP 658 standalone
	// METADATA. When non-empty the value is the digest algorithm; the
	// digest itself is fetched from `<url>.metadata` separately by the
	// bridge in phase 3 (stub ingest).
	CoreMetadata bool
	// Size is the optional file size in bytes (PEP 700).
	Size int64
	// UploadTime is the optional upload timestamp from PEP 691 in ISO 8601.
	UploadTime string
}

// Meta is the optional `meta` object from PEP 691 carrying the API version
// and any future extension fields. The bridge logs it but does not act on
// it in phase 1.
type Meta struct {
	APIVersion string
}

// nameNormaliser implements PEP 503 §"Normalised names": lowercase, then
// collapse runs of `[-_.]` to a single `-`.
var nameNormaliser = regexp.MustCompile(`[-_.]+`)

// Normalise returns the PEP 503 canonical form of a project name.
func Normalise(name string) string {
	return nameNormaliser.ReplaceAllString(strings.ToLower(name), "-")
}

// Validate checks that the parsed Project has at least one file and that
// every file has a populated URL and Filename. The bridge calls Validate
// after either the HTML or the JSON parser populates the struct.
func (p *Project) Validate() error {
	if p.Name == "" {
		return fmt.Errorf("simple: project missing Name")
	}
	if Normalise(p.Name) != p.Name {
		return fmt.Errorf("simple: project Name %q is not PEP 503 normalised (want %q)", p.Name, Normalise(p.Name))
	}
	if len(p.Files) == 0 {
		return fmt.Errorf("simple: project %q has no files", p.Name)
	}
	for i, f := range p.Files {
		if f.Filename == "" {
			return fmt.Errorf("simple: project %q file %d has no Filename", p.Name, i)
		}
		if f.URL == "" {
			return fmt.Errorf("simple: project %q file %q has no URL", p.Name, f.Filename)
		}
	}
	return nil
}

// FilesByFilename returns a map keyed by Filename for fast lookup. The
// bridge's wheel selector calls this after parse to find the file matching
// a specific selected wheel.
func (p *Project) FilesByFilename() map[string]File {
	out := make(map[string]File, len(p.Files))
	for _, f := range p.Files {
		out[f.Filename] = f
	}
	return out
}
