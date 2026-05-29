package maven

import (
	"encoding/xml"
)

// VersionMetadata holds the version information from maven-metadata.xml.
type VersionMetadata struct {
	// GroupID is the Maven groupId.
	GroupID string
	// ArtifactID is the Maven artifactId.
	ArtifactID string
	// Versions is the list of all available versions.
	Versions []string
	// LatestRelease is the latest release version from the metadata.
	LatestRelease string
	// LatestVersion is the latest version (may be a snapshot).
	LatestVersion string
}

// mavenMetadataXML is the XML structure for maven-metadata.xml.
type mavenMetadataXML struct {
	XMLName    xml.Name `xml:"metadata"`
	GroupID    string   `xml:"groupId"`
	ArtifactID string   `xml:"artifactId"`
	Versioning struct {
		Latest  string `xml:"latest"`
		Release string `xml:"release"`
		Versions struct {
			Version []string `xml:"version"`
		} `xml:"versions"`
	} `xml:"versioning"`
}

// parseMetadataXML parses maven-metadata.xml bytes into VersionMetadata.
func parseMetadataXML(data []byte) (*VersionMetadata, error) {
	var meta mavenMetadataXML
	if err := xml.Unmarshal(data, &meta); err != nil {
		return nil, err
	}
	return &VersionMetadata{
		GroupID:       meta.GroupID,
		ArtifactID:    meta.ArtifactID,
		Versions:      meta.Versioning.Versions.Version,
		LatestRelease: meta.Versioning.Release,
		LatestVersion: meta.Versioning.Latest,
	}, nil
}
