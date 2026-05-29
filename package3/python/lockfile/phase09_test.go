package lockfile

import (
	"strings"
	"testing"

	"mochi/package3/python/build"
	"mochi/package3/python/importspec"
)

// TestPhase9LockfileIntegration is the umbrella sentinel for MEP-71 Phase 9.
// It runs a two-target Build, converts the Result into a Manifest, round-trips
// through TOML, runs Check across an artificial drift, and asserts the
// `--check` mode error surfaces both an added entry and a wrapper-sha256
// change.
func TestPhase9LockfileIntegration(t *testing.T) {
	httpxSpec, err := importspec.Parse("httpx@==0.27.2")
	if err != nil {
		t.Fatalf("Parse httpx: %v", err)
	}
	utilSpec, err := importspec.Parse("util")
	if err != nil {
		t.Fatalf("Parse util: %v", err)
	}
	d := build.NewDriver(build.Options{WorkDir: t.TempDir()})
	o := build.NewOrchestrator(d)
	req := build.Request{Targets: []build.Target{
		{Spec: httpxSpec, Alias: "httpx", PYISource: `async def fetch(url: str) -> str: ...
def get(url: str) -> str: ...`},
		{Spec: utilSpec, Alias: "u", PYISource: `
from dataclasses import dataclass

@dataclass(frozen=True)
class Pair:
    a: int
    b: int

def make() -> Pair: ...
`},
	}}
	res, err := o.Build(req)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	manifest, err := FromBuildResult(req, res)
	if err != nil {
		t.Fatalf("FromBuildResult: %v", err)
	}

	if manifest.Version != SchemaVersion {
		t.Errorf("Version = %d", manifest.Version)
	}
	if len(manifest.Entries) != 2 {
		t.Fatalf("entries = %d", len(manifest.Entries))
	}

	// Verify the httpx entry pinned to ==0.27.2 produced Version=0.27.2.
	var httpxEntry, utilEntry Entry
	for _, e := range manifest.Entries {
		switch e.Name {
		case "httpx":
			httpxEntry = e
		case "util":
			utilEntry = e
		}
	}
	if httpxEntry.Version != "0.27.2" {
		t.Errorf("httpx Version = %q, want 0.27.2", httpxEntry.Version)
	}
	if httpxEntry.WrapperSHA256 == "" {
		t.Errorf("httpx WrapperSHA256 is empty")
	}
	asyncFound := false
	for _, c := range httpxEntry.Capabilities {
		if c == CapAsync {
			asyncFound = true
		}
	}
	if !asyncFound {
		t.Errorf("httpx async capability missing: %v", httpxEntry.Capabilities)
	}
	dcFound := false
	for _, c := range utilEntry.Capabilities {
		if c == CapDataclass {
			dcFound = true
		}
	}
	if !dcFound {
		t.Errorf("util dataclass capability missing: %v", utilEntry.Capabilities)
	}

	// TOML round-trip.
	src := RenderTOML(manifest)
	parsed, err := ParseTOML(src)
	if err != nil {
		t.Fatalf("ParseTOML: %v\n%s", err, src)
	}
	if err := Check(manifest, parsed); err != nil {
		t.Errorf("round-trip drift: %v\nrendered:\n%s", err, src)
	}

	// --check drift scenario: add a third entry + flip a wrapper hash.
	drifted := manifest
	drifted.Entries = append([]Entry(nil), manifest.Entries...)
	for i := range drifted.Entries {
		if drifted.Entries[i].Name == "httpx" {
			drifted.Entries[i].WrapperSHA256 = "0000"
		}
	}
	drifted.Entries = append(drifted.Entries, Entry{
		Name:   "ghost",
		Alias:  "gh",
		Source: SourceRegistry,
	})
	drifted.Sort()
	err = Check(manifest, drifted)
	if err == nil {
		t.Fatal("expected --check to surface drift")
	}
	msg := err.Error()
	if !strings.Contains(msg, "added ghost:gh") {
		t.Errorf("Check err missing added: %q", msg)
	}
	if !strings.Contains(msg, "changed httpx:httpx") || !strings.Contains(msg, "wrapper-sha256") {
		t.Errorf("Check err missing changed wrapper-sha256: %q", msg)
	}
}
