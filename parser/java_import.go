package parser

import "strings"

// JavaImportRef parses the path of `import java "<group>:<artifact>@<version>" as
// <alias>` into its (group, artifact, version) triple.
//
// The string must have the form "<groupId>:<artifactId>@<version>". Both the
// groupId and artifactId are validated as Maven coordinates (see isMavenCoordPart).
// The version is required for Java imports: MEP-67 binds the generated JNI wrapper
// to a specific JAR, so omitting it would make the build non-reproducible.
// On success ok is true; on any structural problem ok is false.
func JavaImportRef(path string) (group, artifact, version string, ok bool) {
	s := strings.Trim(path, "\"")
	if s == "" {
		return "", "", "", false
	}

	colonIdx := strings.Index(s, ":")
	if colonIdx <= 0 || colonIdx == len(s)-1 {
		return "", "", "", false
	}
	grp := s[:colonIdx]
	rest := s[colonIdx+1:]

	atIdx := strings.Index(rest, "@")
	if atIdx <= 0 || atIdx == len(rest)-1 {
		return "", "", "", false
	}
	art := rest[:atIdx]
	ver := rest[atIdx+1:]

	if !isMavenCoordPart(grp) || !isMavenCoordPart(art) {
		return "", "", "", false
	}
	if strings.ContainsAny(ver, " \t\n") {
		return "", "", "", false
	}

	return grp, art, ver, true
}

// HasJavaMavenCoord reports whether a java import path is in Maven coordinate form
// (contains a colon and an at-sign in the right positions).
func HasJavaMavenCoord(path string) bool {
	_, _, _, ok := JavaImportRef(path)
	return ok
}
