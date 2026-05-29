// Package grammar documents and implements the MEP-68 grammar extension:
//
//	ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
//	Lang := "go" | "python" | "typescript" | "rust" | "dotnet"
//
// The `dotnet` lang token is the only addition to the Mochi grammar introduced
// by MEP-68.
package grammar

import (
	"fmt"
	"strings"
	"unicode"
)

// LangToken is the grammar token that triggers the .NET bridge.
const LangToken = "dotnet"

// ImportSpec is the parsed form of the string literal in:
//
//	import dotnet "<spec>" as <alias>
type ImportSpec struct {
	// ID is the NuGet package id (e.g. "Newtonsoft.Json").
	ID string
	// VersionReq is the optional version requirement (e.g. "^13.0").
	// Empty means "latest satisfying [dotnet-dependencies]".
	VersionReq string
	// Source is the source kind ("registry", "git", "path").
	Source string
	// GitURL is set when Source == "git".
	GitURL string
	// GitRev is the optional git revision when Source == "git".
	GitRev string
	// LocalPath is set when Source == "path".
	LocalPath string
}

// ParseSpec parses the string literal from `import dotnet "<spec>" as <alias>`.
//
// Supported forms:
//
//	"Newtonsoft.Json"                        -- bare id, resolves via [dotnet-dependencies]
//	"Newtonsoft.Json@13.0.3"                 -- explicit version
//	"Newtonsoft.Json@[13.0,14.0)"            -- NuGet range
//	"MyPkg@git+https://github.com/foo/bar#abc123" -- git source
//	"MyPkg@path+../my-pkg"                   -- local path
func ParseSpec(s string) (ImportSpec, error) {
	if s == "" {
		return ImportSpec{}, fmt.Errorf("grammar: empty import spec")
	}

	// Split on the first '@' to separate the package id from version/source.
	id, rest, hasAt := strings.Cut(s, "@")
	if !hasAt {
		// Bare id with no version requirement.
		if err := validateID(id); err != nil {
			return ImportSpec{}, fmt.Errorf("grammar: %w", err)
		}
		return ImportSpec{ID: id, Source: "registry"}, nil
	}

	if err := validateID(id); err != nil {
		return ImportSpec{}, fmt.Errorf("grammar: %w", err)
	}

	if rest == "" {
		return ImportSpec{}, fmt.Errorf("grammar: empty version/source after '@' in %q", s)
	}

	// Check for git+ prefix.
	if strings.HasPrefix(rest, "git+") {
		url := rest[4:]
		gitURL, gitRev := splitGitRev(url)
		if gitURL == "" {
			return ImportSpec{}, fmt.Errorf("grammar: invalid git URL in %q", s)
		}
		return ImportSpec{
			ID:     id,
			Source: "git",
			GitURL: gitURL,
			GitRev: gitRev,
		}, nil
	}

	// Check for path+ prefix.
	if strings.HasPrefix(rest, "path+") {
		localPath := rest[5:]
		if localPath == "" {
			return ImportSpec{}, fmt.Errorf("grammar: empty path in %q", s)
		}
		return ImportSpec{
			ID:        id,
			Source:    "path",
			LocalPath: localPath,
		}, nil
	}

	// Otherwise treat as a version requirement.
	return ImportSpec{
		ID:         id,
		VersionReq: rest,
		Source:     "registry",
	}, nil
}

// EntryPoint builds the canonical C symbol entry-point prefix for a package id.
//
//	"Newtonsoft.Json" -> "mochi_newtonsoftjson"
func EntryPoint(id string) string {
	var b strings.Builder
	b.WriteString("mochi_")
	for _, r := range id {
		switch {
		case r >= 'a' && r <= 'z', r >= '0' && r <= '9':
			b.WriteRune(r)
		case r >= 'A' && r <= 'Z':
			b.WriteRune(unicode.ToLower(r))
		case r == '.', r == '-':
			// drop separators
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}

// validateID checks that a NuGet package id is non-empty and contains only
// valid characters.
func validateID(id string) error {
	if id == "" {
		return fmt.Errorf("empty package id")
	}
	for i, r := range id {
		if !isIDRune(r) {
			return fmt.Errorf("invalid character %q at position %d in package id %q", r, i, id)
		}
	}
	return nil
}

// isIDRune reports whether r is a valid character in a NuGet package id.
func isIDRune(r rune) bool {
	return unicode.IsLetter(r) || unicode.IsDigit(r) || r == '.' || r == '-' || r == '_'
}

// splitGitRev splits a git URL like "https://github.com/foo/bar#abc123" into
// the URL and optional revision hash.
func splitGitRev(url string) (string, string) {
	if url == "" {
		return "", ""
	}
	if i := strings.LastIndexByte(url, '#'); i >= 0 {
		return url[:i], url[i+1:]
	}
	return url, ""
}
