package parser

import "testing"

func TestKotlinImportRef(t *testing.T) {
	cases := []struct {
		name                              string
		in                                string
		wantGroup, wantArt, wantV, wantCl string
		wantOK                            bool
	}{
		// bare coordinate (no version)
		{"bare coord", `com.squareup.okhttp3:okhttp`, "com.squareup.okhttp3", "okhttp", "", "", true},
		{"quoted bare", `"com.squareup.okhttp3:okhttp"`, "com.squareup.okhttp3", "okhttp", "", "", true},

		// version present
		{"with version", `org.jetbrains.kotlin:kotlin-stdlib@1.9.23`, "org.jetbrains.kotlin", "kotlin-stdlib", "1.9.23", "", true},
		{"quoted version", `"org.jetbrains.kotlin:kotlin-stdlib@1.9.23"`, "org.jetbrains.kotlin", "kotlin-stdlib", "1.9.23", "", true},

		// classifier
		{"with classifier", `org.jetbrains.kotlin:kotlin-stdlib@1.9.23@sources`, "org.jetbrains.kotlin", "kotlin-stdlib", "1.9.23", "sources", true},
		{"quoted classifier", `"org.jetbrains.kotlin:kotlin-stdlib@1.9.23@sources"`, "org.jetbrains.kotlin", "kotlin-stdlib", "1.9.23", "sources", true},

		// hyphens and underscores in artifact
		{"hyphen artifact", `com.example:my-library@2.0.0`, "com.example", "my-library", "2.0.0", "", true},
		{"underscore artifact", `com.example:my_lib@0.1.0`, "com.example", "my_lib", "0.1.0", "", true},
		{"hyphen group segment", `io.ktor:ktor-client-core@2.3.7`, "io.ktor", "ktor-client-core", "2.3.7", "", true},

		// dots in group
		{"deep group", `com.google.android.gms:play-services-auth@21.0.0`, "com.google.android.gms", "play-services-auth", "21.0.0", "", true},

		// semver range-ish version strings are passed through verbatim
		{"caret version", `com.example:lib@^1.0`, "com.example", "lib", "^1.0", "", true},

		// single-segment group or artifact (length 1)
		{"single char group", `a:artifact@1.0`, "a", "artifact", "1.0", "", true},
		{"single char artifact", `group:b@1.0`, "group", "b", "1.0", "", true},

		// error cases
		{"empty string", ``, "", "", "", "", false},
		{"no colon", `orgjetbrains@1.0`, "", "", "", "", false},
		{"leading colon", `:artifact@1.0`, "", "", "", "", false},
		{"trailing colon", `group:`, "", "", "", "", false},
		{"empty artifact after @", `group:@1.0`, "", "", "", "", false},
		{"space in version", `com.example:lib@1.0 rc`, "", "", "", "", false},
		{"tab in classifier", "com.example:lib@1.0@jdk\t11", "", "", "", "", false},
		// Maven coords allow uppercase (e.g. com.Android.tools); not a constraint here.
		{"uppercase group char", `Com.example:lib@1.0`, "Com.example", "lib", "1.0", "", true},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			grp, art, ver, cls, ok := KotlinImportRef(c.in)
			if ok != c.wantOK {
				t.Fatalf("KotlinImportRef(%q) ok = %v; want %v", c.in, ok, c.wantOK)
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
			if cls != c.wantCl {
				t.Errorf("classifier = %q; want %q", cls, c.wantCl)
			}
		})
	}
}

func TestIsMavenCoordPart(t *testing.T) {
	cases := []struct {
		in   string
		want bool
	}{
		{"okhttp", true},
		{"kotlin-stdlib", true},
		{"my_lib", true},
		{"com.example", true},
		{"a", true},
		{"a1", true},
		{"A1b", true},

		{"", false},
		{"_abc", false},
		{"-abc", false},
		{".abc", false},
		{"abc!", false},
		{"abc abc", false},
	}
	for _, c := range cases {
		if got := isMavenCoordPart(c.in); got != c.want {
			t.Errorf("isMavenCoordPart(%q) = %v; want %v", c.in, got, c.want)
		}
	}
}
