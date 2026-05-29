package maven

import (
	"testing"
)

func TestCoordParse(t *testing.T) {
	cases := []struct {
		input      string
		wantGroup  string
		wantArt    string
		wantVer    string
		wantErrStr string
	}{
		// Mochi format
		{"com.google.guava:guava@33.4.8-jre", "com.google.guava", "guava", "33.4.8-jre", ""},
		{"org.apache.commons:commons-lang3@3.14.0", "org.apache.commons", "commons-lang3", "3.14.0", ""},
		// Maven native format
		{"com.google.guava:guava:33.4.8-jre", "com.google.guava", "guava", "33.4.8-jre", ""},
		{"io.netty:netty-all:4.1.107.Final", "io.netty", "netty-all", "4.1.107.Final", ""},
		// Error cases
		{"guava@1.0", "", "", "", "invalid coordinate"},
		{"com.foo:@1.0", "", "", "", "empty artifactId"},
		{"", "", "", "", "invalid coordinate"},
	}
	for _, c := range cases {
		coord, err := Parse(c.input)
		if c.wantErrStr != "" {
			if err == nil {
				t.Errorf("Parse(%q): expected error containing %q, got nil", c.input, c.wantErrStr)
			}
			continue
		}
		if err != nil {
			t.Errorf("Parse(%q): unexpected error: %v", c.input, err)
			continue
		}
		if coord.GroupID != c.wantGroup {
			t.Errorf("Parse(%q).GroupID = %q; want %q", c.input, coord.GroupID, c.wantGroup)
		}
		if coord.ArtifactID != c.wantArt {
			t.Errorf("Parse(%q).ArtifactID = %q; want %q", c.input, coord.ArtifactID, c.wantArt)
		}
		if coord.Version != c.wantVer {
			t.Errorf("Parse(%q).Version = %q; want %q", c.input, coord.Version, c.wantVer)
		}
	}
}

func TestCoordArtifactPath(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	want := "com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.jar"
	if got := c.ArtifactPath(); got != want {
		t.Errorf("ArtifactPath() = %q; want %q", got, want)
	}
}

func TestCoordPOMPath(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	want := "com/google/guava/guava/33.4.8-jre/guava-33.4.8-jre.pom"
	if got := c.POMPath(); got != want {
		t.Errorf("POMPath() = %q; want %q", got, want)
	}
}

func TestCoordSHA1Path(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	got := c.SHA1Path()
	if got != c.ArtifactPath()+".sha1" {
		t.Errorf("SHA1Path() = %q; want %q.sha1", got, c.ArtifactPath())
	}
}

func TestCoordMetadataPath(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	want := "com/google/guava/guava/maven-metadata.xml"
	if got := c.MetadataPath(); got != want {
		t.Errorf("MetadataPath() = %q; want %q", got, want)
	}
}

func TestCoordString(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	want := "com.google.guava:guava:33.4.8-jre"
	if got := c.String(); got != want {
		t.Errorf("String() = %q; want %q", got, want)
	}
}

func TestCoordGroupPath(t *testing.T) {
	c := Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "1.0"}
	if got := c.GroupPath(); got != "com/google/guava" {
		t.Errorf("GroupPath() = %q; want com/google/guava", got)
	}
}
