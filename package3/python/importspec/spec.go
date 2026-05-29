package importspec

import (
	"fmt"
	"net/url"
	"path"
	"strings"

	"mochi/package3/python/semver"
)

// Source classifies the spec's resolution path.
type Source int

const (
	// SourceRegistry resolves against the configured PyPI index. Default.
	SourceRegistry Source = iota
	// SourceIndex resolves against an explicit non-default simple index.
	SourceIndex
	// SourceGit resolves a VCS commit.
	SourceGit
	// SourcePath resolves a local on-disk source tree.
	SourcePath
)

// String renders the canonical token.
func (s Source) String() string {
	switch s {
	case SourceRegistry:
		return "registry"
	case SourceIndex:
		return "index"
	case SourceGit:
		return "git"
	case SourcePath:
		return "path"
	default:
		return "unknown"
	}
}

// Spec is one parsed `import python "..."` body.
type Spec struct {
	// Name is the distribution name, normalised to lowercase (PEP 426 §5).
	// Original spelling is preserved in RawName for emit / error display.
	Name    string
	RawName string

	// Source classifies the resolution path.
	Source Source

	// Specifier is the parsed PEP 440 version specifier when Source is
	// SourceRegistry or SourceIndex and the user pinned a version. Empty
	// (zero-value) means "any" (highest matching the manifest constraint).
	Specifier semver.Specifier

	// IndexURL is the non-default index URL for Source=SourceIndex.
	IndexURL string

	// GitURL is the canonical git URL for Source=SourceGit. Lacks the
	// `git+` prefix; that lives on the wire-format only.
	GitURL string
	// GitRev is the revision (branch / tag / commit) for SourceGit. Empty
	// when no `#<rev>` fragment was supplied.
	GitRev string

	// LocalPath is the path component for Source=SourcePath. Stored as
	// supplied; the resolver normalises against the manifest dir.
	LocalPath string
}

// String renders Spec back to the canonical surface form. This is the
// inverse of Parse and is exact except for whitespace inside the version
// specifier (Specifier.String() collapses spaces).
func (s Spec) String() string {
	switch s.Source {
	case SourceGit:
		base := s.RawName + "@git+" + s.GitURL
		if s.GitRev != "" {
			return base + "#" + s.GitRev
		}
		return base
	case SourcePath:
		return s.RawName + "@path+" + s.LocalPath
	case SourceIndex:
		return s.RawName + "@" + s.RawName + "+" + s.IndexURL
	case SourceRegistry:
		ver := s.Specifier.String()
		if ver == "" {
			return s.RawName
		}
		return s.RawName + "@" + ver
	default:
		return s.RawName
	}
}

// Parse reads the body of `import python "..."`. Returns an error if the
// spec is empty, the distribution name is invalid, or the version /
// source qualifier is malformed.
func Parse(raw string) (Spec, error) {
	if raw == "" {
		return Spec{}, fmt.Errorf("importspec: empty spec")
	}
	s := strings.TrimSpace(raw)
	if s != raw {
		return Spec{}, fmt.Errorf("importspec: %q contains leading or trailing whitespace", raw)
	}
	at := strings.IndexByte(s, '@')
	if at < 0 {
		return newRegistry(s, "")
	}
	nameRaw := s[:at]
	rest := s[at+1:]
	if rest == "" {
		return Spec{}, fmt.Errorf("importspec: %q has empty qualifier after '@'", raw)
	}
	// Look for the distinguished qualifier prefixes git+ path+, or
	// <name>+ for index URLs.
	switch {
	case strings.HasPrefix(rest, "git+"):
		return newGit(nameRaw, rest[len("git+"):])
	case strings.HasPrefix(rest, "path+"):
		return newPath(nameRaw, rest[len("path+"):])
	}
	// Index form: <indexname>+<url>. We distinguish from a version spec
	// by looking for a `+` followed by a URL scheme. PEP 440 version
	// strings do not contain `+http` (the `+` local-version segment is
	// followed by alphanumerics, not URL schemes).
	if plus := strings.IndexByte(rest, '+'); plus >= 0 {
		idxName := rest[:plus]
		idxURL := rest[plus+1:]
		if isURLLike(idxURL) {
			return newIndex(nameRaw, idxName, idxURL)
		}
	}
	return newRegistry(nameRaw, rest)
}

func newRegistry(name, ver string) (Spec, error) {
	if err := validateName(name); err != nil {
		return Spec{}, err
	}
	spec := Spec{
		Name:    normaliseName(name),
		RawName: name,
		Source:  SourceRegistry,
	}
	if ver == "" {
		return spec, nil
	}
	sp, err := semver.ParseSpecifier(ver)
	if err != nil {
		return Spec{}, fmt.Errorf("importspec: %q: %w", name+"@"+ver, err)
	}
	spec.Specifier = sp
	return spec, nil
}

func newIndex(name, idxName, idxURL string) (Spec, error) {
	if err := validateName(name); err != nil {
		return Spec{}, err
	}
	if idxName == "" {
		return Spec{}, fmt.Errorf("importspec: %q has empty index identifier", name+"@+"+idxURL)
	}
	if !isURLLike(idxURL) {
		return Spec{}, fmt.Errorf("importspec: %q: index URL %q is not a recognised URL", name, idxURL)
	}
	return Spec{
		Name:     normaliseName(name),
		RawName:  name,
		Source:   SourceIndex,
		IndexURL: idxURL,
	}, nil
}

func newGit(name, rest string) (Spec, error) {
	if err := validateName(name); err != nil {
		return Spec{}, err
	}
	if rest == "" {
		return Spec{}, fmt.Errorf("importspec: %q: empty git URL", name)
	}
	var rev string
	if hash := strings.IndexByte(rest, '#'); hash >= 0 {
		rev = rest[hash+1:]
		rest = rest[:hash]
		if rest == "" {
			return Spec{}, fmt.Errorf("importspec: %q: git URL is empty before fragment", name)
		}
		if rev == "" {
			return Spec{}, fmt.Errorf("importspec: %q: git fragment is empty after '#'", name)
		}
	}
	if !isURLLike(rest) {
		return Spec{}, fmt.Errorf("importspec: %q: git URL %q is not a recognised URL", name, rest)
	}
	return Spec{
		Name:    normaliseName(name),
		RawName: name,
		Source:  SourceGit,
		GitURL:  rest,
		GitRev:  rev,
	}, nil
}

func newPath(name, rest string) (Spec, error) {
	if err := validateName(name); err != nil {
		return Spec{}, err
	}
	if rest == "" {
		return Spec{}, fmt.Errorf("importspec: %q: empty path", name)
	}
	cleaned := path.Clean(rest)
	if cleaned == "." || cleaned == "/" {
		return Spec{}, fmt.Errorf("importspec: %q: path %q is not a usable directory", name, rest)
	}
	return Spec{
		Name:      normaliseName(name),
		RawName:   name,
		Source:    SourcePath,
		LocalPath: rest,
	}, nil
}

// validateName enforces PEP 508 / PEP 426 distribution-name shape.
func validateName(s string) error {
	if s == "" {
		return fmt.Errorf("importspec: empty distribution name")
	}
	// First char must be a letter or digit.
	if !isPyDistChar(rune(s[0]), true) {
		return fmt.Errorf("importspec: distribution name %q must begin with a letter or digit", s)
	}
	// Subsequent chars: letter, digit, dash, dot, underscore.
	for i, r := range s {
		if i == 0 {
			continue
		}
		if !isPyDistChar(r, false) {
			return fmt.Errorf("importspec: distribution name %q contains invalid character %q", s, r)
		}
	}
	// Disallow leading / trailing separators.
	if strings.HasPrefix(s, "-") || strings.HasPrefix(s, ".") || strings.HasPrefix(s, "_") {
		return fmt.Errorf("importspec: distribution name %q has leading separator", s)
	}
	if strings.HasSuffix(s, "-") || strings.HasSuffix(s, ".") || strings.HasSuffix(s, "_") {
		return fmt.Errorf("importspec: distribution name %q has trailing separator", s)
	}
	return nil
}

func isPyDistChar(r rune, first bool) bool {
	switch {
	case r >= 'a' && r <= 'z':
		return true
	case r >= 'A' && r <= 'Z':
		return true
	case r >= '0' && r <= '9':
		return true
	case !first && (r == '-' || r == '.' || r == '_'):
		return true
	default:
		return false
	}
}

// normaliseName lowercases and collapses runs of `-`, `_`, `.` into a single
// `-` per PEP 503.
func normaliseName(s string) string {
	var b strings.Builder
	prevSep := false
	for _, r := range strings.ToLower(s) {
		if r == '_' || r == '.' {
			r = '-'
		}
		if r == '-' {
			if prevSep {
				continue
			}
			prevSep = true
		} else {
			prevSep = false
		}
		b.WriteRune(r)
	}
	out := b.String()
	out = strings.TrimSuffix(out, "-")
	return out
}

func isURLLike(s string) bool {
	if s == "" {
		return false
	}
	// We do not require RFC 3986 validity; we only need to distinguish
	// URL-like strings from PEP 440 version specifiers. The presence of
	// a scheme followed by `:` or `://` is the discriminator.
	if u, err := url.Parse(s); err == nil && u.Scheme != "" {
		switch u.Scheme {
		case "http", "https", "ssh", "git", "file", "ftp":
			return true
		}
	}
	return false
}
