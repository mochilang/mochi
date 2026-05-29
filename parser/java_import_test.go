package parser

import "testing"

func TestJavaImportRef(t *testing.T) {
	cases := []struct {
		name                       string
		in                         string
		wantGroup, wantArt, wantV  string
		wantOK                     bool
	}{
		// standard Maven coordinate with version
		{"basic", `com.google.guava:guava@33.0.0-jre`, "com.google.guava", "guava", "33.0.0-jre", true},
		{"quoted", `"com.google.guava:guava@33.0.0-jre"`, "com.google.guava", "guava", "33.0.0-jre", true},
		{"snapshot", `org.example:my-lib@1.0-SNAPSHOT`, "org.example", "my-lib", "1.0-SNAPSHOT", true},
		{"deep group", `com.google.android.gms:play-services-auth@21.0.0`, "com.google.android.gms", "play-services-auth", "21.0.0", true},
		{"short artifact", `io.grpc:grpc-core@1.62.0`, "io.grpc", "grpc-core", "1.62.0", true},
		{"underscore artifact", `org.example:my_util@0.1.0`, "org.example", "my_util", "0.1.0", true},

		// error cases
		{"empty", ``, "", "", "", false},
		{"no colon", `com.example@1.0`, "", "", "", false},
		{"no version", `com.example:artifact`, "", "", "", false},
		{"missing version after @", `com.example:artifact@`, "", "", "", false},
		{"leading colon", `:artifact@1.0`, "", "", "", false},
		{"trailing colon", `group:`, "", "", "", false},
		{"space in version", `com.example:lib@1.0 rc`, "", "", "", false},
		{"tab in version", "com.example:lib@1.0\t", "", "", "", false},
		{"empty artifact", `com.example:@1.0`, "", "", "", false},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			grp, art, ver, ok := JavaImportRef(c.in)
			if ok != c.wantOK {
				t.Fatalf("JavaImportRef(%q) ok = %v; want %v", c.in, ok, c.wantOK)
			}
			if !ok {
				return
			}
			if grp != c.wantGroup {
				t.Errorf("group = %q; want %q", grp, c.wantGroup)
			}
			if art != c.wantArt {
				t.Errorf("artifact = %q; want %q", art, c.wantArt)
			}
			if ver != c.wantV {
				t.Errorf("version = %q; want %q", ver, c.wantV)
			}
		})
	}
}

func TestParseNormalize_JavaImportAccepted(t *testing.T) {
	src := `import java "com.google.guava:guava@33.0.0-jre" as guava`
	prog, err := ParseString(src)
	if err != nil {
		t.Fatalf("parse error: %v", err)
	}
	if len(prog.Statements) == 0 || prog.Statements[0].Import == nil {
		t.Fatal("expected import statement")
	}
	im := prog.Statements[0].Import
	if im.Lang == nil || *im.Lang != "java" {
		t.Errorf("Lang = %v, want java", im.Lang)
	}
	if im.Path != "com.google.guava:guava@33.0.0-jre" {
		t.Errorf("Path = %q, want com.google.guava:guava@33.0.0-jre", im.Path)
	}
	if im.As != "guava" {
		t.Errorf("As = %q, want guava", im.As)
	}
}

func TestParseNormalize_JavaImportInvalidPath(t *testing.T) {
	src := `import java "notacoord" as lib`
	_, err := ParseString(src)
	if err == nil {
		t.Error("should reject java import path without Maven coordinate form")
	}
}

func TestParseNormalize_JavaImportMissingAlias(t *testing.T) {
	src := `import java "com.google.guava:guava@33.0.0-jre"`
	_, err := ParseString(src)
	if err == nil {
		t.Error("should reject java import without as <alias>")
	}
}

func TestParseNormalize_JavaLangRecognized(t *testing.T) {
	// java must be in knownImportLangs; unknown lang produces P064.
	src := `import java "com.google.guava:guava@33.0.0-jre"`
	_, err := ParseString(src)
	// May fail for other reasons (missing as) but must NOT be P064.
	if err != nil {
		msg := err.Error()
		if contains(msg, "P064") {
			t.Errorf("java should be a known import lang, got P064: %v", err)
		}
	}
}
