package lockfile

import (
	"strings"
	"testing"
)

func basePkg() RustPackage {
	return RustPackage{
		Name:    "hex",
		Version: "0.4.3",
		Source: Source{
			Kind:     SourceRegistry,
			Registry: "https://github.com/rust-lang/crates.io-index",
		},
		CrateBlake3:          "b3-aaaa",
		CrateSHA256:          "sha-aaaa",
		RustdocTypesVersion:  "32",
		RustdocSHA256:        "rd-aaaa",
		WrapperSHA256:        "wr-aaaa",
		CapabilitiesDeclared: []string{"pure"},
		Features:             []string{"std"},
		Dependencies:         []string{},
	}
}

func TestDriftKindString(t *testing.T) {
	cases := []struct {
		kind DriftKind
		want string
	}{
		{DriftAdded, "added"},
		{DriftRemoved, "removed"},
		{DriftVersion, "version"},
		{DriftSource, "source"},
		{DriftCrateBlake3, "crate-blake3"},
		{DriftCrateSHA256, "crate-sha256"},
		{DriftRustdocTypesVersion, "rustdoc-types-version"},
		{DriftRustdocSHA256, "rustdoc-sha256"},
		{DriftWrapperSHA256, "wrapper-sha256"},
		{DriftCapabilityAdded, "capability-added"},
		{DriftCapabilityRemoved, "capability-removed"},
		{DriftFeatures, "features"},
		{DriftDependencies, "dependencies"},
		{DriftUnknown, "unknown"},
		{DriftKind(99), "unknown"},
	}
	for _, c := range cases {
		if got := c.kind.String(); got != c.want {
			t.Errorf("DriftKind(%d).String() = %q, want %q", c.kind, got, c.want)
		}
	}
}

func TestDriftStringFormatsAdded(t *testing.T) {
	d := Drift{Crate: "hex", Kind: DriftAdded}
	want := "hex: package added (not in lockfile)"
	if got := d.String(); got != want {
		t.Errorf("got %q want %q", got, want)
	}
}

func TestDriftStringFormatsRemoved(t *testing.T) {
	d := Drift{Crate: "hex", Kind: DriftRemoved}
	want := "hex: package removed (in lockfile, not in fresh resolve)"
	if got := d.String(); got != want {
		t.Errorf("got %q want %q", got, want)
	}
}

func TestDriftStringFormatsKeyedWithoutDetail(t *testing.T) {
	d := Drift{Crate: "hex", Kind: DriftVersion, Want: "0.4.3", Have: "0.4.4"}
	want := "hex: version drift: want 0.4.3 have 0.4.4"
	if got := d.String(); got != want {
		t.Errorf("got %q want %q", got, want)
	}
}

func TestDriftStringFormatsKeyedWithDetail(t *testing.T) {
	d := Drift{Crate: "hex", Kind: DriftCapabilityAdded, Want: "[]", Have: "[net]", Detail: "new capabilities: [net]"}
	got := d.String()
	if !strings.Contains(got, "hex: capability-added drift:") {
		t.Errorf("missing prefix: %q", got)
	}
	if !strings.Contains(got, "new capabilities: [net]") {
		t.Errorf("missing detail: %q", got)
	}
}

func TestCheckIdenticalReturnsNil(t *testing.T) {
	a := basePkg()
	b := basePkg()
	if got := Check([]RustPackage{a}, []RustPackage{b}); got != nil {
		t.Fatalf("expected nil for identical input, got %v", got)
	}
}

func TestCheckDetectsAdded(t *testing.T) {
	have := basePkg()
	drifts := Check(nil, []RustPackage{have})
	if len(drifts) != 1 || drifts[0].Kind != DriftAdded || drifts[0].Crate != "hex" {
		t.Fatalf("expected single added drift for hex, got %v", drifts)
	}
}

func TestCheckDetectsRemoved(t *testing.T) {
	want := basePkg()
	drifts := Check([]RustPackage{want}, nil)
	if len(drifts) != 1 || drifts[0].Kind != DriftRemoved || drifts[0].Crate != "hex" {
		t.Fatalf("expected single removed drift for hex, got %v", drifts)
	}
}

func TestCheckDetectsVersion(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.Version = "0.4.4"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftVersion {
		t.Fatalf("expected version drift, got %v", drifts)
	}
	if drifts[0].Want != "0.4.3" || drifts[0].Have != "0.4.4" {
		t.Errorf("unexpected want/have: %+v", drifts[0])
	}
}

func TestCheckDetectsSource(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.Source = Source{Kind: SourceGit, URL: "https://github.com/RustCrypto/utils.git", Rev: "abc"}
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftSource {
		t.Fatalf("expected source drift, got %v", drifts)
	}
	if !strings.Contains(drifts[0].Have, "git ") {
		t.Errorf("expected have to render git source, got %q", drifts[0].Have)
	}
}

func TestCheckDetectsCrateBlake3(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.CrateBlake3 = "b3-bbbb"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftCrateBlake3 {
		t.Fatalf("expected crate-blake3 drift, got %v", drifts)
	}
}

func TestCheckDetectsCrateSHA256(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.CrateSHA256 = "sha-bbbb"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftCrateSHA256 {
		t.Fatalf("expected crate-sha256 drift, got %v", drifts)
	}
}

func TestCheckDetectsRustdocTypesVersion(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.RustdocTypesVersion = "33"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftRustdocTypesVersion {
		t.Fatalf("expected rustdoc-types-version drift, got %v", drifts)
	}
}

func TestCheckDetectsRustdocSHA256(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.RustdocSHA256 = "rd-bbbb"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftRustdocSHA256 {
		t.Fatalf("expected rustdoc-sha256 drift, got %v", drifts)
	}
}

func TestCheckDetectsWrapperSHA256(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.WrapperSHA256 = "wr-bbbb"
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftWrapperSHA256 {
		t.Fatalf("expected wrapper-sha256 drift, got %v", drifts)
	}
}

func TestCheckDetectsCapabilityAdded(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.CapabilitiesDeclared = []string{"pure", "net"}
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 {
		t.Fatalf("expected single drift, got %d: %v", len(drifts), drifts)
	}
	if drifts[0].Kind != DriftCapabilityAdded {
		t.Fatalf("expected capability-added drift, got %v", drifts[0])
	}
	if !strings.Contains(drifts[0].Detail, "net") {
		t.Errorf("expected detail to mention net, got %q", drifts[0].Detail)
	}
}

func TestCheckDetectsCapabilityRemoved(t *testing.T) {
	w := basePkg()
	w.CapabilitiesDeclared = []string{"pure", "net"}
	h := basePkg()
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 {
		t.Fatalf("expected single drift, got %d: %v", len(drifts), drifts)
	}
	if drifts[0].Kind != DriftCapabilityRemoved {
		t.Fatalf("expected capability-removed drift, got %v", drifts[0])
	}
	if !strings.Contains(drifts[0].Detail, "net") {
		t.Errorf("expected detail to mention net, got %q", drifts[0].Detail)
	}
}

func TestCheckDetectsFeatures(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.Features = []string{"std", "alloc"}
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftFeatures {
		t.Fatalf("expected features drift, got %v", drifts)
	}
}

func TestCheckDetectsDependencies(t *testing.T) {
	w := basePkg()
	h := basePkg()
	h.Dependencies = []string{"serde 1.0.0"}
	drifts := Check([]RustPackage{w}, []RustPackage{h})
	if len(drifts) != 1 || drifts[0].Kind != DriftDependencies {
		t.Fatalf("expected dependencies drift, got %v", drifts)
	}
}

func TestCheckSortsByCrateThenKind(t *testing.T) {
	w1 := basePkg()
	w1.Name = "zoo"
	w2 := basePkg()
	w2.Name = "alpha"
	h1 := basePkg()
	h1.Name = "zoo"
	h1.Version = "9.9.9"
	h1.WrapperSHA256 = "wr-zzzz"
	h2 := basePkg()
	h2.Name = "alpha"
	h2.Version = "9.9.9"
	drifts := Check([]RustPackage{w1, w2}, []RustPackage{h1, h2})
	if len(drifts) < 3 {
		t.Fatalf("expected at least 3 drifts, got %d: %v", len(drifts), drifts)
	}
	if drifts[0].Crate != "alpha" {
		t.Errorf("expected alpha first, got %q", drifts[0].Crate)
	}
	zooIdx := -1
	for i, d := range drifts {
		if d.Crate == "zoo" {
			zooIdx = i
			break
		}
	}
	if zooIdx < 0 {
		t.Fatal("zoo not found in drifts")
	}
	if drifts[zooIdx].Kind > drifts[zooIdx+1].Kind {
		t.Errorf("zoo drifts not sorted by kind: %v", drifts[zooIdx:])
	}
}

func TestCheckMultiplePackagesMultiDrift(t *testing.T) {
	w1 := basePkg()
	h1 := basePkg()
	h1.Version = "0.4.4"
	h1.WrapperSHA256 = "wr-bbbb"

	w2 := basePkg()
	w2.Name = "anyhow"
	h2 := basePkg()
	h2.Name = "anyhow"

	drifts := Check([]RustPackage{w1, w2}, []RustPackage{h1, h2})
	if len(drifts) != 2 {
		t.Fatalf("expected 2 drifts (version + wrapper-sha for hex), got %d: %v", len(drifts), drifts)
	}
	for _, d := range drifts {
		if d.Crate != "hex" {
			t.Errorf("unexpected crate in drifts: %v", d)
		}
	}
}

func TestSourceEqual(t *testing.T) {
	a := Source{Kind: SourceRegistry, Registry: "https://x"}
	b := Source{Kind: SourceRegistry, Registry: "https://x"}
	if !sourceEqual(a, b) {
		t.Error("equal registry sources should compare equal")
	}
	b.Registry = "https://y"
	if sourceEqual(a, b) {
		t.Error("different registry should compare unequal")
	}
	g1 := Source{Kind: SourceGit, URL: "u", Rev: "r"}
	g2 := Source{Kind: SourceGit, URL: "u", Rev: "r"}
	if !sourceEqual(g1, g2) {
		t.Error("equal git sources should compare equal")
	}
	g2.Rev = "s"
	if sourceEqual(g1, g2) {
		t.Error("different rev should compare unequal")
	}
	p1 := Source{Kind: SourcePath, Path: "./x"}
	p2 := Source{Kind: SourcePath, Path: "./x"}
	if !sourceEqual(p1, p2) {
		t.Error("equal path sources should compare equal")
	}
}

func TestSourceString(t *testing.T) {
	cases := []struct {
		src      Source
		contains string
	}{
		{Source{Kind: SourceRegistry, Registry: "https://crates.io"}, "registry"},
		{Source{Kind: SourceGit, URL: "https://x.git", Rev: "abc"}, "abc"},
		{Source{Kind: SourceGit, URL: "https://x.git"}, "git"},
		{Source{Kind: SourcePath, Path: "./vendor/x"}, "path"},
	}
	for _, c := range cases {
		got := sourceString(c.src)
		if !strings.Contains(got, c.contains) {
			t.Errorf("sourceString(%+v) = %q, want substring %q", c.src, got, c.contains)
		}
	}
}

func TestStringSetEqual(t *testing.T) {
	cases := []struct {
		a, b []string
		want bool
	}{
		{nil, nil, true},
		{[]string{}, []string{}, true},
		{[]string{"a"}, []string{"a"}, true},
		{[]string{"a", "b"}, []string{"b", "a"}, true},
		{[]string{"a"}, []string{"b"}, false},
		{[]string{"a"}, []string{"a", "a"}, false},
		{[]string{"a", "a"}, []string{"a", "a"}, true},
	}
	for _, c := range cases {
		if got := stringSetEqual(c.a, c.b); got != c.want {
			t.Errorf("stringSetEqual(%v, %v) = %v, want %v", c.a, c.b, got, c.want)
		}
	}
}

func TestStringSetDiff(t *testing.T) {
	got := stringSetDiff([]string{"a", "b", "c"}, []string{"b"})
	if len(got) != 2 || got[0] != "a" || got[1] != "c" {
		t.Errorf("got %v, want [a c]", got)
	}
	if got := stringSetDiff(nil, []string{"a"}); len(got) != 0 {
		t.Errorf("nil minus anything is empty, got %v", got)
	}
	if got := stringSetDiff([]string{"a"}, nil); len(got) != 1 || got[0] != "a" {
		t.Errorf("anything minus nil is anything, got %v", got)
	}
}

func TestJoinSorted(t *testing.T) {
	got := joinSorted([]string{"b", "a", "c"})
	if got != "[a b c]" {
		t.Errorf("got %q, want [a b c]", got)
	}
	if got := joinSorted(nil); got != "[]" {
		t.Errorf("nil joinSorted = %q, want []", got)
	}
}
