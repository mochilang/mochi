package maven

import (
	"testing"
)

const fixtureMetadataXML = `<?xml version="1.0" encoding="UTF-8"?>
<metadata>
  <groupId>com.google.guava</groupId>
  <artifactId>guava</artifactId>
  <versioning>
    <latest>33.4.8-jre</latest>
    <release>33.4.8-jre</release>
    <versions>
      <version>32.1.0-jre</version>
      <version>33.0.0-jre</version>
      <version>33.4.8-jre</version>
    </versions>
  </versioning>
</metadata>`

func TestParseMetadataXML(t *testing.T) {
	meta, err := parseMetadataXML([]byte(fixtureMetadataXML))
	if err != nil {
		t.Fatalf("parseMetadataXML: %v", err)
	}
	if meta.GroupID != "com.google.guava" {
		t.Errorf("GroupID = %q; want com.google.guava", meta.GroupID)
	}
	if meta.ArtifactID != "guava" {
		t.Errorf("ArtifactID = %q; want guava", meta.ArtifactID)
	}
	if len(meta.Versions) != 3 {
		t.Errorf("len(Versions) = %d; want 3", len(meta.Versions))
	}
	if meta.LatestRelease != "33.4.8-jre" {
		t.Errorf("LatestRelease = %q; want 33.4.8-jre", meta.LatestRelease)
	}
	if meta.LatestVersion != "33.4.8-jre" {
		t.Errorf("LatestVersion = %q; want 33.4.8-jre", meta.LatestVersion)
	}
}

func TestParseMetadataXMLInvalid(t *testing.T) {
	_, err := parseMetadataXML([]byte("not xml"))
	if err == nil {
		t.Error("expected error for invalid XML")
	}
}
