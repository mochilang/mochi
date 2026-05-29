package lockfile

import (
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/python/build"
	"mochi/package3/python/importspec"
)

const minimalStub = `def add(a: int, b: int) -> int: ...`

const recordStub = `
from dataclasses import dataclass

@dataclass(frozen=True)
class Pair:
    first: int
    second: int

def make() -> Pair: ...
`

const asyncStub = `
async def fetch(url: str) -> str: ...
`

func makeBuildResult(t *testing.T, raw, alias, pyi string) (build.Request, *build.Result) {
	t.Helper()
	spec, err := importspec.Parse(raw)
	if err != nil {
		t.Fatalf("Parse(%q): %v", raw, err)
	}
	d := build.NewDriver(build.Options{WorkDir: t.TempDir(), CacheDir: filepath.Join(t.TempDir(), "cache")})
	o := build.NewOrchestrator(d)
	req := build.Request{Targets: []build.Target{{Spec: spec, Alias: alias, PYISource: pyi}}}
	res, err := o.Build(req)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	return req, res
}

func TestSourceValid(t *testing.T) {
	for _, s := range []Source{SourceRegistry, SourceIndex, SourceGit, SourcePath} {
		if !s.Valid() {
			t.Errorf("Source(%q) reports invalid", s)
		}
	}
	if Source("nonsense").Valid() {
		t.Errorf("invalid source slipped through")
	}
}

func TestManifestSortByNameThenAlias(t *testing.T) {
	m := Manifest{Entries: []Entry{
		{Name: "z", Alias: "a"},
		{Name: "a", Alias: "b"},
		{Name: "a", Alias: "a"},
	}}
	m.Sort()
	if m.Entries[0].Name != "a" || m.Entries[0].Alias != "a" {
		t.Errorf("first = %+v", m.Entries[0])
	}
	if m.Entries[2].Name != "z" {
		t.Errorf("last = %+v", m.Entries[2])
	}
}

func TestFromBuildResultRegistryEntry(t *testing.T) {
	req, res := makeBuildResult(t, "minimal", "mn", minimalStub)
	m, err := FromBuildResult(req, res)
	if err != nil {
		t.Fatalf("FromBuildResult: %v", err)
	}
	if len(m.Entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(m.Entries))
	}
	e := m.Entries[0]
	if e.Name != "minimal" || e.Alias != "mn" || e.Source != SourceRegistry {
		t.Errorf("entry = %+v", e)
	}
	if e.WrapperSHA256 == "" {
		t.Errorf("WrapperSHA256 is empty")
	}
}

func TestFromBuildResultExactPinPopulatesVersion(t *testing.T) {
	req, res := makeBuildResult(t, "pydantic@==2.6.1", "pyd", minimalStub)
	m, _ := FromBuildResult(req, res)
	if m.Entries[0].Version != "2.6.1" {
		t.Errorf("Version = %q, want 2.6.1", m.Entries[0].Version)
	}
}

func TestFromBuildResultOpenRangeLeavesVersionEmpty(t *testing.T) {
	req, res := makeBuildResult(t, "requests@>=2.0,<3.0", "rq", minimalStub)
	m, _ := FromBuildResult(req, res)
	if m.Entries[0].Version != "" {
		t.Errorf("Version = %q, want empty", m.Entries[0].Version)
	}
	if m.Entries[0].Specifier != ">=2.0, <3.0" {
		t.Errorf("Specifier = %q", m.Entries[0].Specifier)
	}
}

func TestFromBuildResultGitEntry(t *testing.T) {
	req, res := makeBuildResult(t, "mypkg@git+https://github.com/user/repo#main", "mp", minimalStub)
	m, _ := FromBuildResult(req, res)
	e := m.Entries[0]
	if e.Source != SourceGit {
		t.Errorf("Source = %q", e.Source)
	}
	if e.GitURL != "https://github.com/user/repo" || e.GitRev != "main" {
		t.Errorf("Git fields: url=%q rev=%q", e.GitURL, e.GitRev)
	}
}

func TestFromBuildResultPathEntry(t *testing.T) {
	req, res := makeBuildResult(t, "mypkg@path+../sibling", "mp", minimalStub)
	m, _ := FromBuildResult(req, res)
	e := m.Entries[0]
	if e.Source != SourcePath {
		t.Errorf("Source = %q", e.Source)
	}
	if e.LocalPath != "../sibling" {
		t.Errorf("LocalPath = %q", e.LocalPath)
	}
}

func TestFromBuildResultIndexEntry(t *testing.T) {
	req, res := makeBuildResult(t, "torch@torch+https://download.pytorch.org/whl/cu121", "tc", minimalStub)
	m, _ := FromBuildResult(req, res)
	e := m.Entries[0]
	if e.Source != SourceIndex || e.IndexURL == "" {
		t.Errorf("Index fields: source=%q url=%q", e.Source, e.IndexURL)
	}
}

func TestFromBuildResultMismatchRejected(t *testing.T) {
	_, err := FromBuildResult(build.Request{Targets: []build.Target{{Alias: "x"}}}, &build.Result{})
	if err == nil {
		t.Fatal("expected error for mismatched counts")
	}
}

func TestFromBuildResultNilResult(t *testing.T) {
	if _, err := FromBuildResult(build.Request{}, nil); err == nil {
		t.Fatal("expected error for nil Result")
	}
}

func TestRenderTOMLRoundTrip(t *testing.T) {
	m := Manifest{Version: SchemaVersion, Entries: []Entry{
		{
			Name:          "requests",
			Alias:         "rq",
			Source:        SourceRegistry,
			Specifier:     ">=2.0, <3.0",
			WrapperSHA256: "deadbeef",
			Capabilities:  []string{"callable", "optional"},
		},
		{
			Name:          "mypkg",
			Alias:         "mp",
			Source:        SourceGit,
			GitURL:        "https://github.com/user/repo",
			GitRev:        "main",
			WrapperSHA256: "cafebabe",
		},
	}}
	src := RenderTOML(m)
	back, err := ParseTOML(src)
	if err != nil {
		t.Fatalf("ParseTOML: %v\n%s", err, src)
	}
	if len(back.Entries) != 2 {
		t.Fatalf("round-trip entries = %d", len(back.Entries))
	}
	// Sorted by name: mypkg before requests.
	if back.Entries[0].Name != "mypkg" {
		t.Errorf("first entry = %+v", back.Entries[0])
	}
	rq := back.Entries[1]
	if rq.Specifier != ">=2.0, <3.0" || rq.WrapperSHA256 != "deadbeef" {
		t.Errorf("rq = %+v", rq)
	}
	if len(rq.Capabilities) != 2 {
		t.Errorf("capabilities = %v", rq.Capabilities)
	}
}

func TestRenderTOMLOmitsEmptyOptionals(t *testing.T) {
	m := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	src := RenderTOML(m)
	for _, key := range []string{"index-url", "git-url", "git-rev", "local-path", "wrapper-sha256", "specifier", "capabilities"} {
		if strings.Contains(src, key) {
			t.Errorf("rendered output contains optional %q:\n%s", key, src)
		}
	}
	if strings.Contains(src, "\nversion =") {
		t.Errorf("rendered output contains optional version:\n%s", src)
	}
}

func TestRenderTOMLDeterministic(t *testing.T) {
	m := Manifest{Version: SchemaVersion, Entries: []Entry{
		{Name: "b", Alias: "b", Source: SourceRegistry},
		{Name: "a", Alias: "a", Source: SourceRegistry},
	}}
	one := RenderTOML(m)
	two := RenderTOML(m)
	if one != two {
		t.Errorf("RenderTOML not deterministic")
	}
}

func TestParseTOMLRejectsUnknownSchemaVersion(t *testing.T) {
	src := "schema-version = 99\n[[python-package]]\nname=\"x\"\nalias=\"x\"\nsource=\"registry\"\n"
	if _, err := ParseTOML(src); err == nil {
		t.Fatal("expected error for unknown schema-version")
	}
}

func TestParseTOMLRejectsMissingSchemaVersion(t *testing.T) {
	src := "[[python-package]]\nname=\"x\"\nalias=\"x\"\nsource=\"registry\"\n"
	if _, err := ParseTOML(src); err == nil {
		t.Fatal("expected error for missing schema-version")
	}
}

func TestParseTOMLRejectsInvalidSource(t *testing.T) {
	src := "schema-version = 1\n[[python-package]]\nname=\"x\"\nalias=\"x\"\nsource=\"madeup\"\n"
	if _, err := ParseTOML(src); err == nil {
		t.Fatal("expected error for invalid source")
	}
}

func TestParseTOMLRejectsMissingRequiredField(t *testing.T) {
	src := "schema-version = 1\n[[python-package]]\nname=\"x\"\nsource=\"registry\"\n"
	if _, err := ParseTOML(src); err == nil {
		t.Fatal("expected error for missing alias")
	}
}

func TestExtractCapabilitiesAsync(t *testing.T) {
	req, res := makeBuildResult(t, "asynclib", "as", asyncStub)
	m, _ := FromBuildResult(req, res)
	found := false
	for _, c := range m.Entries[0].Capabilities {
		if c == CapAsync {
			found = true
		}
	}
	if !found {
		t.Errorf("async capability missing: %v", m.Entries[0].Capabilities)
	}
}

func TestExtractCapabilitiesDataclass(t *testing.T) {
	req, res := makeBuildResult(t, "reclib", "rc", recordStub)
	m, _ := FromBuildResult(req, res)
	found := false
	for _, c := range m.Entries[0].Capabilities {
		if c == CapDataclass {
			found = true
		}
	}
	if !found {
		t.Errorf("dataclass capability missing: %v", m.Entries[0].Capabilities)
	}
}

func TestExtractCapabilitiesNil(t *testing.T) {
	if ExtractCapabilities(nil) != nil {
		t.Errorf("nil wrapper should produce nil capabilities")
	}
}

func TestCompareManifestsEmpty(t *testing.T) {
	a := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	b := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	d := CompareManifests(a, b)
	if !d.Empty() {
		t.Errorf("diff = %v", d.String())
	}
}

func TestCompareManifestsAdded(t *testing.T) {
	a := Manifest{}
	b := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	d := CompareManifests(a, b)
	if len(d.Entries) != 1 || d.Entries[0].Kind != DiffAdded {
		t.Errorf("diff = %+v", d.Entries)
	}
}

func TestCompareManifestsRemoved(t *testing.T) {
	a := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	b := Manifest{}
	d := CompareManifests(a, b)
	if len(d.Entries) != 1 || d.Entries[0].Kind != DiffRemoved {
		t.Errorf("diff = %+v", d.Entries)
	}
}

func TestCompareManifestsChanged(t *testing.T) {
	a := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry, Version: "1.0"}}}
	b := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry, Version: "2.0"}}}
	d := CompareManifests(a, b)
	if len(d.Entries) != 1 || d.Entries[0].Kind != DiffChanged {
		t.Errorf("diff = %+v", d.Entries)
	}
	if d.Entries[0].Fields[0] != "version" {
		t.Errorf("fields = %v", d.Entries[0].Fields)
	}
}

func TestCheckMatching(t *testing.T) {
	a := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	b := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry}}}
	if err := Check(a, b); err != nil {
		t.Errorf("Check(matching) = %v", err)
	}
}

func TestCheckMismatching(t *testing.T) {
	a := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry, Version: "1.0"}}}
	b := Manifest{Entries: []Entry{{Name: "x", Alias: "x", Source: SourceRegistry, Version: "2.0"}}}
	err := Check(a, b)
	if err == nil {
		t.Fatal("expected drift error")
	}
	if !strings.Contains(err.Error(), "version") {
		t.Errorf("err = %q", err.Error())
	}
}

func TestDiffEntryString(t *testing.T) {
	de := DiffEntry{Kind: DiffChanged, Key: "x:y", Fields: []string{"a", "b"}}
	if got := de.String(); got != "changed x:y (a,b)" {
		t.Errorf("String = %q", got)
	}
	de2 := DiffEntry{Kind: DiffAdded, Key: "x:y"}
	if got := de2.String(); got != "added x:y" {
		t.Errorf("String = %q", got)
	}
}

func TestDiffKindStringCoverage(t *testing.T) {
	cases := []struct {
		k    DiffKind
		want string
	}{
		{DiffAdded, "added"},
		{DiffRemoved, "removed"},
		{DiffChanged, "changed"},
		{DiffKind(99), "unknown"},
	}
	for _, tc := range cases {
		if got := tc.k.String(); got != tc.want {
			t.Errorf("DiffKind(%d).String = %q, want %q", tc.k, got, tc.want)
		}
	}
}

func TestRoundTripFromBuild(t *testing.T) {
	req, res := makeBuildResult(t, "asynclib@>=1.0,<2.0", "as", asyncStub)
	original, err := FromBuildResult(req, res)
	if err != nil {
		t.Fatalf("FromBuildResult: %v", err)
	}
	src := RenderTOML(original)
	parsed, err := ParseTOML(src)
	if err != nil {
		t.Fatalf("ParseTOML: %v\n%s", err, src)
	}
	if err := Check(original, parsed); err != nil {
		t.Errorf("round-trip drift:\n%v\nrendered:\n%s", err, src)
	}
}
