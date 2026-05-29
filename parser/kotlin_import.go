package parser

import "strings"

// KotlinImportRef parses the path of `import kotlin "<group>:<artifact>@<version>" as
// <alias>` into its (group, artifact, version) triple.
//
// The string must have the form "<group>:<artifact>@<version>".  The group and
// artifact parts are validated as Maven coordinates: ASCII letters, digits, `-`,
// `_`, and `.`; must start with a letter or digit; length 1..255.  The version
// part is left to the bridge's semver parser; this routine only enforces the
// overall shape so malformed coordinates become parse-time diagnostics.
//
// An optional trailing "@<classifier>" segment is accepted and returned in
// classifier; callers that do not need classifiers can ignore it.
//
// On success ok is true; on any structural problem ok is false and all
// returned strings are empty.
func KotlinImportRef(path string) (group, artifact, version, classifier string, ok bool) {
	s := strings.Trim(path, "\"")
	if s == "" {
		return "", "", "", "", false
	}

	// Split group:artifact from the rest
	colonIdx := strings.Index(s, ":")
	if colonIdx <= 0 || colonIdx == len(s)-1 {
		return "", "", "", "", false
	}
	grp := s[:colonIdx]
	rest := s[colonIdx+1:]

	// The artifact may contain an @ for the version.
	atIdx := strings.Index(rest, "@")
	var art, verAndClass string
	if atIdx <= 0 {
		// No version — bare coordinate, version resolved from mochi.toml.
		art = rest
		verAndClass = ""
	} else {
		art = rest[:atIdx]
		verAndClass = rest[atIdx+1:]
	}

	if !isMavenCoordPart(grp) || !isMavenCoordPart(art) {
		return "", "", "", "", false
	}

	// Split version from optional classifier (<version>@<classifier>).
	ver := verAndClass
	cls := ""
	if atIdx2 := strings.Index(verAndClass, "@"); atIdx2 >= 0 {
		ver = verAndClass[:atIdx2]
		cls = verAndClass[atIdx2+1:]
	}

	if strings.ContainsAny(ver, " \t\n") || strings.ContainsAny(cls, " \t\n") {
		return "", "", "", "", false
	}

	return grp, art, ver, cls, true
}

// isMavenCoordPart validates a single segment (groupId component or artifactId)
// of a Maven coordinate.  Accepts: ASCII letters, digits, `-`, `_`, `.`.
// Must start with a letter or digit; length 1..255.
func isMavenCoordPart(s string) bool {
	if len(s) == 0 || len(s) > 255 {
		return false
	}
	if !isMavenStartChar(s[0]) {
		return false
	}
	for i := 0; i < len(s); i++ {
		if !isMavenCoordChar(s[i]) {
			return false
		}
	}
	return true
}

func isMavenStartChar(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
}

func isMavenCoordChar(c byte) bool {
	return isMavenStartChar(c) || c == '-' || c == '_' || c == '.'
}
