package maven

import "testing"

func TestParseCoordinate(t *testing.T) {
	cases := []struct {
		in         string
		groupID    string
		artifactID string
		version    string
		classifier string
		wantErr    bool
	}{
		{"org.example:mylib", "org.example", "mylib", "", "", false},
		{"org.example:mylib@1.0.0", "org.example", "mylib", "1.0.0", "", false},
		{"org.example:mylib@1.0.0@jdk8", "org.example", "mylib", "1.0.0", "jdk8", false},
		{"org.jetbrains.kotlinx:kotlinx-coroutines-core@1.7.3", "org.jetbrains.kotlinx", "kotlinx-coroutines-core", "1.7.3", "", false},
		{"invalid", "", "", "", "", true},
		{"", "", "", "", "", true},
		{":artifact", "", "", "", "", true},
		{"group:", "", "", "", "", true},
	}
	for _, c := range cases {
		coord, err := ParseCoordinate(c.in)
		if c.wantErr {
			if err == nil {
				t.Errorf("ParseCoordinate(%q) want error, got nil", c.in)
			}
			continue
		}
		if err != nil {
			t.Errorf("ParseCoordinate(%q) error: %v", c.in, err)
			continue
		}
		if coord.GroupID != c.groupID || coord.ArtifactID != c.artifactID ||
			coord.Version != c.version || coord.Classifier != c.classifier {
			t.Errorf("ParseCoordinate(%q) = %+v; want {%s %s %s %s}",
				c.in, coord, c.groupID, c.artifactID, c.version, c.classifier)
		}
	}
}

func TestCoordinatePaths(t *testing.T) {
	c, _ := ParseCoordinate("org.jetbrains.kotlinx:kotlinx-coroutines-core@1.7.3")
	if got := c.GroupPath(); got != "org/jetbrains/kotlinx" {
		t.Errorf("GroupPath() = %q; want %q", got, "org/jetbrains/kotlinx")
	}
	wantJAR := "org/jetbrains/kotlinx/kotlinx-coroutines-core/1.7.3/kotlinx-coroutines-core-1.7.3.jar"
	if got := c.JARPath(); got != wantJAR {
		t.Errorf("JARPath() = %q; want %q", got, wantJAR)
	}
}
