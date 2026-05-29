// Package maven provides a Maven Central metadata client and coordinate types.
package maven

import (
	"strings"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// Coordinate identifies a Maven artifact.
type Coordinate struct {
	GroupID    string
	ArtifactID string
	Version    string
	Classifier string
}

// String returns the canonical coordinate string.
func (c Coordinate) String() string {
	s := c.GroupID + ":" + c.ArtifactID
	if c.Version != "" {
		s += "@" + c.Version
	}
	if c.Classifier != "" {
		s += "@" + c.Classifier
	}
	return s
}

// GroupPath returns the group ID with dots replaced by slashes (Maven repo layout).
func (c Coordinate) GroupPath() string {
	return strings.ReplaceAll(c.GroupID, ".", "/")
}

// JARPath returns the relative Maven repository path for the JAR artifact.
func (c Coordinate) JARPath() string {
	name := c.ArtifactID + "-" + c.Version
	if c.Classifier != "" {
		name += "-" + c.Classifier
	}
	return c.GroupPath() + "/" + c.ArtifactID + "/" + c.Version + "/" + name + ".jar"
}

// POMPath returns the relative Maven repository path for the POM.
func (c Coordinate) POMPath() string {
	return c.GroupPath() + "/" + c.ArtifactID + "/" + c.Version + "/" +
		c.ArtifactID + "-" + c.Version + ".pom"
}

// ModulePath returns the relative path for the Gradle Module Metadata (.module) file.
func (c Coordinate) ModulePath() string {
	return c.GroupPath() + "/" + c.ArtifactID + "/" + c.Version + "/" +
		c.ArtifactID + "-" + c.Version + ".module"
}

// MetadataPath returns the maven-metadata.xml path for a groupId:artifactId pair.
func (c Coordinate) MetadataPath() string {
	return c.GroupPath() + "/" + c.ArtifactID + "/maven-metadata.xml"
}

// ParseCoordinate parses a Maven coordinate string of the form
// "<groupId>:<artifactId>[@<version>[@<classifier>]]".
func ParseCoordinate(s string) (Coordinate, error) {
	s = strings.TrimSpace(s)
	// Split on @ first to extract version/classifier
	atParts := strings.SplitN(s, "@", 3)
	gavPart := atParts[0]

	colonParts := strings.SplitN(gavPart, ":", 2)
	if len(colonParts) != 2 || colonParts[0] == "" || colonParts[1] == "" {
		return Coordinate{}, &bridgeerrors.ErrInvalidCoordinate{Input: s}
	}
	c := Coordinate{
		GroupID:    colonParts[0],
		ArtifactID: colonParts[1],
	}
	if len(atParts) >= 2 {
		c.Version = atParts[1]
	}
	if len(atParts) >= 3 {
		c.Classifier = atParts[2]
	}
	return c, nil
}
