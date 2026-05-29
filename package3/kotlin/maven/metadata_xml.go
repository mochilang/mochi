package maven

import (
	"encoding/xml"
	"fmt"
	"io"
)

// MavenMetadata represents the content of a maven-metadata.xml file.
type MavenMetadata struct {
	XMLName    xml.Name    `xml:"metadata"`
	GroupID    string      `xml:"groupId"`
	ArtifactID string      `xml:"artifactId"`
	Versioning Versioning  `xml:"versioning"`
}

// Versioning holds version information from maven-metadata.xml.
type Versioning struct {
	Latest      string   `xml:"latest"`
	Release     string   `xml:"release"`
	Versions    []string `xml:"versions>version"`
	LastUpdated string   `xml:"lastUpdated"`
}

// ParseMavenMetadata parses a maven-metadata.xml file from the given reader.
func ParseMavenMetadata(r io.Reader) (*MavenMetadata, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("maven-metadata: read error: %w", err)
	}

	var m MavenMetadata
	if err := xml.Unmarshal(data, &m); err != nil {
		return nil, fmt.Errorf("maven-metadata: xml parse error: %w", err)
	}

	return &m, nil
}
