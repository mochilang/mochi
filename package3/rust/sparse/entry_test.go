package sparse

import (
	"strings"
	"testing"

	"mochi/package3/rust/semver"
)

const serdeFixture = `{"name":"serde","vers":"1.0.0","deps":[],"cksum":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","features":{},"yanked":false}
{"name":"serde","vers":"1.0.1","deps":[],"cksum":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb","features":{},"yanked":true}
{"name":"serde","vers":"1.0.2","deps":[{"name":"serde_derive","req":"=1.0.2","features":[],"optional":true,"default_features":true,"target":null,"kind":"normal"}],"cksum":"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc","features":{"default":["std"],"std":[],"derive":["serde_derive"]},"yanked":false,"v":2,"rust_version":"1.31"}
{"name":"serde","vers":"2.0.0-alpha.1","deps":[],"cksum":"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd","features":{},"yanked":false}
`

func TestParseIndex(t *testing.T) {
	entries, err := ParseIndex(strings.NewReader(serdeFixture))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	if len(entries) != 4 {
		t.Fatalf("len(entries) = %d; want 4", len(entries))
	}
	if entries[0].Name != "serde" || entries[0].Vers != "1.0.0" {
		t.Errorf("first entry = %+v", entries[0])
	}
	if !entries[1].Yanked {
		t.Errorf("second entry yanked = false")
	}
	if entries[2].V != 2 {
		t.Errorf("third entry V = %d; want 2", entries[2].V)
	}
	if entries[2].RustVersion != "1.31" {
		t.Errorf("third entry rust_version = %q; want 1.31", entries[2].RustVersion)
	}
	if entries[2].Features["default"][0] != "std" {
		t.Errorf("third entry default feature = %v", entries[2].Features["default"])
	}
	if entries[3].Vers != "2.0.0-alpha.1" {
		t.Errorf("fourth entry vers = %q", entries[3].Vers)
	}
}

func TestParseIndexBlankLines(t *testing.T) {
	in := "\n" + serdeFixture + "\n\n"
	entries, err := ParseIndex(strings.NewReader(in))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	if len(entries) != 4 {
		t.Errorf("len(entries) = %d; want 4", len(entries))
	}
}

func TestParseIndexBadJSON(t *testing.T) {
	in := `{"name":"serde","vers":"1.0.0"` + "\n"
	if _, err := ParseIndex(strings.NewReader(in)); err == nil {
		t.Errorf("ParseIndex(bad) err = nil; want error")
	}
}

func TestEntryVersion(t *testing.T) {
	e := CrateEntry{Name: "serde", Vers: "1.2.3-rc.1+build.5"}
	v, err := e.Version()
	if err != nil {
		t.Fatalf("Version: %v", err)
	}
	want := semver.Version{Major: 1, Minor: 2, Patch: 3, Pre: "rc.1", Build: "build.5"}
	if v != want {
		t.Errorf("Version = %+v; want %+v", v, want)
	}
}

func TestEntryVersionBad(t *testing.T) {
	e := CrateEntry{Name: "x", Vers: "not-a-version"}
	if _, err := e.Version(); err == nil {
		t.Errorf("Version(bad) err = nil; want error")
	}
}

func TestMergedFeatures(t *testing.T) {
	e := CrateEntry{
		Features:  map[string][]string{"default": {"std"}, "alloc": {}},
		Features2: map[string][]string{"std": {"alloc"}, "default": {"std", "derive"}},
	}
	got := e.MergedFeatures()
	if len(got) != 3 {
		t.Errorf("len(merged) = %d; want 3", len(got))
	}
	if got["default"][0] != "std" || got["default"][1] != "derive" {
		t.Errorf("default feature = %v; want [std derive] (Features2 wins)", got["default"])
	}
	if got["std"][0] != "alloc" {
		t.Errorf("std feature = %v; want [alloc]", got["std"])
	}
}

func TestMergedFeaturesIndependent(t *testing.T) {
	e := CrateEntry{Features: map[string][]string{"a": {"b"}}}
	out := e.MergedFeatures()
	out["a"][0] = "MUTATED"
	if e.Features["a"][0] != "b" {
		t.Errorf("mutation of merged map should not affect source: %v", e.Features["a"])
	}
}

func TestSortEntriesByVersion(t *testing.T) {
	entries, err := ParseIndex(strings.NewReader(serdeFixture))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	// Shuffle.
	entries[0], entries[3] = entries[3], entries[0]
	SortEntriesByVersion(entries)
	want := []string{"1.0.0", "1.0.1", "1.0.2", "2.0.0-alpha.1"}
	for i, v := range want {
		if entries[i].Vers != v {
			t.Errorf("sorted[%d] = %q; want %q", i, entries[i].Vers, v)
		}
	}
}

func TestSortYankedTieBreaker(t *testing.T) {
	entries := []CrateEntry{
		{Name: "x", Vers: "1.0.0", Yanked: true},
		{Name: "x", Vers: "1.0.0", Yanked: false},
	}
	SortEntriesByVersion(entries)
	if entries[0].Yanked || !entries[1].Yanked {
		t.Errorf("non-yanked should sort before yanked at equal version: got %+v", entries)
	}
}

func TestLatestMatching(t *testing.T) {
	entries, err := ParseIndex(strings.NewReader(serdeFixture))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	// ^1.0 should pick 1.0.2 (1.0.0 is older, 1.0.1 is yanked, 2.x is out of range,
	// 2.0.0-alpha.1 is prerelease and not opted-in).
	got, ok := LatestMatching(entries, semver.MustParseReq("^1.0"))
	if !ok {
		t.Fatalf("LatestMatching(^1.0) returned ok=false")
	}
	if got.Vers != "1.0.2" {
		t.Errorf("LatestMatching(^1.0) = %q; want 1.0.2", got.Vers)
	}
}

func TestLatestMatchingNoCandidate(t *testing.T) {
	entries, err := ParseIndex(strings.NewReader(serdeFixture))
	if err != nil {
		t.Fatalf("ParseIndex: %v", err)
	}
	if _, ok := LatestMatching(entries, semver.MustParseReq("^3")); ok {
		t.Errorf("LatestMatching(^3) ok=true; want false")
	}
}

func TestLatestMatchingSkipsYanked(t *testing.T) {
	entries := []CrateEntry{
		{Name: "x", Vers: "1.0.0", Yanked: false},
		{Name: "x", Vers: "1.0.1", Yanked: true},
	}
	got, ok := LatestMatching(entries, semver.MustParseReq("^1"))
	if !ok {
		t.Fatalf("LatestMatching returned ok=false")
	}
	if got.Vers != "1.0.0" {
		t.Errorf("LatestMatching = %q; want 1.0.0 (1.0.1 yanked)", got.Vers)
	}
}
