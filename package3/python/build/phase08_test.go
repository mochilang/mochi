package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/python/importspec"
)

// TestPhase8BuildOrchestration is the umbrella sentinel for MEP-71 Phase 8.
// It walks a representative two-target Request end-to-end through
// stubs.ParsePYI -> wrapper.Synthesise -> emit.EmitShim and asserts the
// laid-out python_workspace contains every artifact downstream phases
// expect.
func TestPhase8BuildOrchestration(t *testing.T) {
	const httpxStub = `
from typing import Protocol

class Response(Protocol):
    status_code: int

def get(url: str, timeout: float = 5.0) -> str: ...
async def fetch(url: str) -> str: ...
`
	const utilStub = `
def hash(s: str) -> str: ...
`
	d := NewDriver(Options{WorkDir: t.TempDir()})
	o := NewOrchestrator(d)

	httpxSpec, err := importspec.Parse("httpx@>=0.27,<0.30")
	if err != nil {
		t.Fatalf("Parse httpx: %v", err)
	}
	utilSpec, err := importspec.Parse("util")
	if err != nil {
		t.Fatalf("Parse util: %v", err)
	}

	res, err := o.Build(Request{
		Targets: []Target{
			{Spec: httpxSpec, Alias: "httpx", PYISource: httpxStub},
			{Spec: utilSpec, Alias: "u", PYISource: utilStub},
		},
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}

	if res.VenvRoot == "" {
		t.Fatal("VenvRoot is empty")
	}
	if len(res.Wrappers) != 2 {
		t.Fatalf("wrappers = %d, want 2", len(res.Wrappers))
	}
	if len(res.Shims) != 2 {
		t.Fatalf("shims = %d, want 2", len(res.Shims))
	}

	// Workspace root
	if _, err := os.Stat(filepath.Join(res.VenvRoot, "pyproject.toml")); err != nil {
		t.Errorf("pyproject.toml: %v", err)
	}
	if _, err := os.Stat(filepath.Join(res.VenvRoot, "_mochi_wrap.py")); err != nil {
		t.Errorf("_mochi_wrap.py: %v", err)
	}

	// Per-target artifacts
	for _, alias := range []string{"httpx", "u"} {
		dir := filepath.Join(res.VenvRoot, "python_wrap", alias)
		if _, err := os.Stat(filepath.Join(dir, "__init__.py")); err != nil {
			t.Errorf("alias %q: __init__.py: %v", alias, err)
		}
	}

	// httpx shim text contains both sync extern fun get and async extern fun.
	body, err := os.ReadFile(filepath.Join(res.VenvRoot, "python_wrap", "httpx", "httpx_shim.mochi"))
	if err != nil {
		t.Fatalf("read httpx shim: %v", err)
	}
	src := string(body)
	if !strings.Contains(src, "extern python fun get") {
		t.Errorf("httpx shim missing sync get:\n%s", src)
	}
	if !strings.Contains(src, "extern python fun fetch_sync") {
		t.Errorf("httpx shim missing async fetch_sync:\n%s", src)
	}

	// pyproject.toml mentions the version-pinned dep and both wrapper paths.
	venvBody, err := os.ReadFile(filepath.Join(res.VenvRoot, "pyproject.toml"))
	if err != nil {
		t.Fatalf("read pyproject.toml: %v", err)
	}
	venvSrc := string(venvBody)
	if !strings.Contains(venvSrc, `"httpx>=0.27, <0.30"`) {
		t.Errorf("pyproject.toml missing httpx version spec:\n%s", venvSrc)
	}
	if !strings.Contains(venvSrc, `"util"`) {
		t.Errorf("pyproject.toml missing bare util dep:\n%s", venvSrc)
	}
	if !strings.Contains(venvSrc, `"python_wrap/httpx"`) {
		t.Errorf("pyproject.toml missing httpx wrapper:\n%s", venvSrc)
	}
	if !strings.Contains(venvSrc, `"python_wrap/u"`) {
		t.Errorf("pyproject.toml missing util wrapper:\n%s", venvSrc)
	}
}
