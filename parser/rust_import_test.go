package parser

import "testing"

func TestRustImportRef(t *testing.T) {
	cases := []struct {
		name             string
		in               string
		wantCrate, wantV string
		wantOK           bool
	}{
		{"plain hex", `hex@0.4.3`, "hex", "0.4.3", true},
		{"quoted hex", `"hex@0.4.3"`, "hex", "0.4.3", true},
		{"once_cell with underscore", `once_cell@1.19.0`, "once_cell", "1.19.0", true},
		{"hyphenated crate", `rand-chacha@0.3.1`, "rand-chacha", "0.3.1", true},
		{"caret range", `serde@^1.0`, "serde", "^1.0", true},
		{"tilde range", `serde@~1.0.195`, "serde", "~1.0.195", true},
		{"semver pre", `tokio@1.40.0-rc.1`, "tokio", "1.40.0-rc.1", true},
		{"semver build", `tokio@1.40.0+meta`, "tokio", "1.40.0+meta", true},

		{"missing version", `hex`, "", "", false},
		{"empty crate", `@1.0`, "", "", false},
		{"empty version", `hex@`, "", "", false},
		{"uppercase crate", `Hex@1.0`, "", "", false},
		{"digit start crate", `4hex@1.0`, "", "", false},
		{"dot in crate", `hex.helper@1.0`, "", "", false},
		{"space in version", `hex@1.0 alpha`, "", "", false},
		{"empty", ``, "", "", false},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			gotC, gotV, ok := RustImportRef(c.in)
			if ok != c.wantOK {
				t.Fatalf("RustImportRef(%q) ok = %v; want %v", c.in, ok, c.wantOK)
			}
			if !ok {
				return
			}
			if gotC != c.wantCrate || gotV != c.wantV {
				t.Errorf("RustImportRef(%q) = (%q, %q); want (%q, %q)", c.in, gotC, gotV, c.wantCrate, c.wantV)
			}
		})
	}
}

func TestIsCargoCrateName(t *testing.T) {
	cases := []struct {
		in   string
		want bool
	}{
		{"hex", true},
		{"once_cell", true},
		{"rand-chacha", true},
		{"a", true},
		{"a1", true},
		{"a_b-c", true},

		{"", false},
		{"1abc", false},
		{"_abc", false},
		{"-abc", false},
		{"Hex", false},
		{"foo.bar", false},
		{"foo bar", false},
	}
	for _, c := range cases {
		if got := isCargoCrateName(c.in); got != c.want {
			t.Errorf("isCargoCrateName(%q) = %v; want %v", c.in, got, c.want)
		}
	}
}
