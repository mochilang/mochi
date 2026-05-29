// Package maven provides a Maven Central client, coordinate parsing, and
// content-addressed JAR cache for the MEP-67 Java bridge.
package maven

import (
	"fmt"
	"strings"
)

// Coordinate identifies a Maven artifact uniquely as groupId:artifactId:version.
type Coordinate struct {
	GroupID    string
	ArtifactID string
	Version    string
}

// Parse parses a coordinate string in one of two formats:
//   - "groupId:artifactId@version"  (Mochi import syntax)
//   - "groupId:artifactId:version"  (Maven native syntax)
func Parse(s string) (Coordinate, error) {
	s = strings.TrimSpace(s)
	// Try Mochi format first: groupId:artifactId@version
	if idx := strings.LastIndex(s, "@"); idx >= 0 {
		head := s[:idx]
		version := s[idx+1:]
		parts := strings.SplitN(head, ":", 2)
		if len(parts) != 2 {
			return Coordinate{}, fmt.Errorf("maven: invalid coordinate %q: expected groupId:artifactId@version", s)
		}
		return validateCoord(Coordinate{GroupID: parts[0], ArtifactID: parts[1], Version: version}, s)
	}
	// Try Maven native format: groupId:artifactId:version
	parts := strings.SplitN(s, ":", 3)
	if len(parts) != 3 {
		return Coordinate{}, fmt.Errorf("maven: invalid coordinate %q: expected groupId:artifactId:version", s)
	}
	return validateCoord(Coordinate{GroupID: parts[0], ArtifactID: parts[1], Version: parts[2]}, s)
}

func validateCoord(c Coordinate, orig string) (Coordinate, error) {
	if c.GroupID == "" {
		return Coordinate{}, fmt.Errorf("maven: empty groupId in %q", orig)
	}
	if c.ArtifactID == "" {
		return Coordinate{}, fmt.Errorf("maven: empty artifactId in %q", orig)
	}
	if c.Version == "" {
		return Coordinate{}, fmt.Errorf("maven: empty version in %q", orig)
	}
	return c, nil
}

// GroupPath converts groupId dots to slashes (com.google.guava -> com/google/guava).
func (c Coordinate) GroupPath() string {
	return strings.ReplaceAll(c.GroupID, ".", "/")
}

// ArtifactPath returns the Maven repository path for the JAR, e.g.
// "com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar".
func (c Coordinate) ArtifactPath() string {
	return fmt.Sprintf("%s/%s/%s/%s", c.GroupPath(), c.ArtifactID, c.Version, c.JARFilename())
}

// POMPath returns the path for the POM file.
func (c Coordinate) POMPath() string {
	return fmt.Sprintf("%s/%s/%s/%s-%s.pom", c.GroupPath(), c.ArtifactID, c.Version, c.ArtifactID, c.Version)
}

// SHA1Path returns the path for the SHA-1 checksum.
func (c Coordinate) SHA1Path() string {
	return c.ArtifactPath() + ".sha1"
}

// SHA256Path returns the path for the SHA-256 checksum (not always present).
func (c Coordinate) SHA256Path() string {
	return c.ArtifactPath() + ".sha256"
}

// MetadataPath returns the path for maven-metadata.xml.
func (c Coordinate) MetadataPath() string {
	return fmt.Sprintf("%s/%s/maven-metadata.xml", c.GroupPath(), c.ArtifactID)
}

// JARFilename returns the filename for the JAR, e.g. "guava-33.4.8-jre.jar".
func (c Coordinate) JARFilename() string {
	return fmt.Sprintf("%s-%s.jar", c.ArtifactID, c.Version)
}

// String returns "groupId:artifactId:version" (Maven native format).
func (c Coordinate) String() string {
	return fmt.Sprintf("%s:%s:%s", c.GroupID, c.ArtifactID, c.Version)
}
