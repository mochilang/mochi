package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/python/emit"
	"mochi/package3/python/importspec"
	"mochi/package3/python/wrapper"
)

const httpxStub = `
from typing import Protocol

class Response(Protocol):
    status_code: int

def get(url: str, timeout: float = 5.0) -> Response: ...

async def fetch(url: str) -> str: ...
`

const minimalStub = `
def add(a: int, b: int) -> int: ...
`

func newTestDriver(t *testing.T) *Driver {
	t.Helper()
	dir := t.TempDir()
	return NewDriver(Options{WorkDir: dir, CacheDir: filepath.Join(dir, "cache")})
}

func makeTarget(t *testing.T, raw, alias, pyi string) Target {
	t.Helper()
	spec, err := importspec.Parse(raw)
	if err != nil {
		t.Fatalf("Parse(%q): %v", raw, err)
	}
	return Target{Spec: spec, Alias: alias, PYISource: pyi}
}

func TestPlanRequiresTargets(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	if _, err := o.Plan(Request{}); err == nil {
		t.Fatal("expected error for empty Targets")
	}
}

func TestPlanRequiresAlias(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	spec, _ := importspec.Parse("requests")
	if _, err := o.Plan(Request{Targets: []Target{{Spec: spec, PYISource: minimalStub}}}); err == nil {
		t.Fatal("expected error for empty alias")
	}
}

func TestPlanRequiresSpecName(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	if _, err := o.Plan(Request{Targets: []Target{{Alias: "rq", PYISource: minimalStub}}}); err == nil {
		t.Fatal("expected error for empty spec name")
	}
}

func TestPlanRejectsDuplicateAlias(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	a := makeTarget(t, "requests", "rq", minimalStub)
	b := makeTarget(t, "httpx", "rq", minimalStub)
	if _, err := o.Plan(Request{Targets: []Target{a, b}}); err == nil {
		t.Fatal("expected error for duplicate alias")
	}
}

func TestPlanPopulatesWrappersAndShims(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "httpx@>=0.27,<0.30", "httpx", httpxStub)
	res, err := o.Plan(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Plan: %v", err)
	}
	if len(res.Wrappers) != 1 || res.Wrappers[0].Package != "httpx" {
		t.Errorf("wrappers = %+v", res.Wrappers)
	}
	if len(res.Shims) != 1 {
		t.Fatalf("shims = %d, want 1", len(res.Shims))
	}
	if res.Shims[0].SHA256 == "" {
		t.Errorf("shim SHA256 is empty")
	}
	if res.Shims[0].Package != "httpx" {
		t.Errorf("shim package = %q", res.Shims[0].Package)
	}
}

func TestPlanAddsWrapperMembersToVenv(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	venv := DefaultVenv()
	tgt := makeTarget(t, "requests@>=2.0,<3.0", "rq", minimalStub)
	if _, err := o.Plan(Request{Targets: []Target{tgt}, Venv: venv}); err != nil {
		t.Fatalf("Plan: %v", err)
	}
	found := false
	for _, m := range venv.Members {
		if m.Name == "rq" && m.Kind == MemberWrapper {
			found = true
		}
	}
	if !found {
		t.Errorf("wrapper member missing: %+v", venv.Members)
	}
	if got := venv.SharedDependencies["requests"]; got != ">=2.0, <3.0" {
		t.Errorf("shared dep = %q", got)
	}
}

func TestPlanSurfacesPyiParseError(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "requests", "rq", "def foo(a, b: # unterminated bracket\n")
	_, err := o.Plan(Request{Targets: []Target{tgt}})
	if err == nil {
		t.Fatal("expected error for malformed pyi")
	}
	if !strings.Contains(err.Error(), "parse pyi") {
		t.Errorf("err = %q, want substring 'parse pyi'", err.Error())
	}
}

func TestBuildWritesWorkspace(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "httpx@>=0.27,<0.30", "httpx", httpxStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if res.WorkDir == "" || res.VenvRoot == "" {
		t.Fatalf("Result missing paths: %+v", res)
	}
	expect := []string{
		filepath.Join(res.VenvRoot, "pyproject.toml"),
		filepath.Join(res.VenvRoot, ".gitignore"),
		filepath.Join(res.VenvRoot, "_mochi_wrap.py"),
		filepath.Join(res.VenvRoot, "_mochi_wrap.pyi"),
		filepath.Join(res.VenvRoot, "python_wrap", "httpx", "__init__.py"),
		filepath.Join(res.VenvRoot, "python_wrap", "httpx", "httpx_externs.py"),
		filepath.Join(res.VenvRoot, "python_wrap", "httpx", "httpx_externs.pyi"),
		filepath.Join(res.VenvRoot, "python_wrap", "httpx", "httpx_shim.mochi"),
	}
	for _, want := range expect {
		if _, err := os.Stat(want); err != nil {
			t.Errorf("missing file %q: %v", want, err)
		}
	}
}

func TestBuildVenvPyprojectMentionsDep(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "requests@>=2.0,<3.0", "rq", minimalStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	body, err := os.ReadFile(filepath.Join(res.VenvRoot, "pyproject.toml"))
	if err != nil {
		t.Fatalf("read pyproject.toml: %v", err)
	}
	if !strings.Contains(string(body), `"requests>=2.0, <3.0"`) {
		t.Errorf("pyproject.toml missing requests dep:\n%s", body)
	}
	if !strings.Contains(string(body), `"python_wrap/rq"`) {
		t.Errorf("pyproject.toml missing wrapper member:\n%s", body)
	}
}

func TestBuildShimFileContainsExternFun(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "minimal", "mn", minimalStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	body, err := os.ReadFile(filepath.Join(res.VenvRoot, "python_wrap", "mn", "minimal_shim.mochi"))
	if err != nil {
		t.Fatalf("read shim: %v", err)
	}
	if !strings.Contains(string(body), "extern python fun add") {
		t.Errorf("shim missing extern fun:\n%s", body)
	}
}

func TestBuildEmitsSkippedTxtWhenWrapperSkipped(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	// Mutable @dataclass is refused; only frozen=True is accepted.
	pyi := `
from dataclasses import dataclass

@dataclass
class Mutable:
    x: int
    y: int

def keep() -> int: ...
`
	tgt := makeTarget(t, "skiplib", "sk", pyi)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if len(res.Skipped) == 0 {
		t.Fatal("expected at least one skip")
	}
	skipPath := filepath.Join(res.VenvRoot, "python_wrap", "sk", "SKIPPED.txt")
	if _, err := os.Stat(skipPath); err != nil {
		t.Errorf("SKIPPED.txt not written: %v", err)
	}
}

func TestBuildSourceCanonicalised(t *testing.T) {
	d1 := newTestDriver(t)
	d2 := newTestDriver(t)
	tgt := makeTarget(t, "minimal", "mn", minimalStub)
	r1, err := NewOrchestrator(d1).Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build #1: %v", err)
	}
	r2, err := NewOrchestrator(d2).Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build #2: %v", err)
	}
	if r1.Shims[0].SHA256 != r2.Shims[0].SHA256 {
		t.Errorf("shim SHA differs: %s vs %s", r1.Shims[0].SHA256, r2.Shims[0].SHA256)
	}
	if r1.Shims[0].Source != r2.Shims[0].Source {
		t.Errorf("shim Source differs across builds")
	}
}

func TestBuildSourceGitDepHasEmptyVersion(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "mypkg@git+https://github.com/user/repo#main", "mp", minimalStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	body, _ := os.ReadFile(filepath.Join(res.VenvRoot, "pyproject.toml"))
	if !strings.Contains(string(body), `"mypkg"`) {
		t.Errorf("pyproject.toml missing bare mypkg dep:\n%s", body)
	}
}

func TestBuildPropagatesWrapperOpts(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	// AllowPartial = true should let an Any-returning fn through.
	pyi := `
from typing import Any
def returns_any() -> Any: ...
`
	tgt := makeTarget(t, "anylib", "an", pyi)
	res, err := o.Build(Request{
		Targets:     []Target{tgt},
		WrapperOpts: wrapper.Options{AllowPartial: true},
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if len(res.Wrappers[0].Items) == 0 {
		t.Errorf("AllowPartial should let returns_any through; got 0 items")
	}
}

func TestBuildPropagatesShimOpts(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "minimal", "mn", minimalStub)
	res, err := o.Build(Request{
		Targets:  []Target{tgt},
		ShimOpts: emit.Options{Header: false, IncludeSkipReport: false},
	})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	body, _ := os.ReadFile(filepath.Join(res.VenvRoot, "python_wrap", "mn", "minimal_shim.mochi"))
	if strings.Contains(string(body), "Auto-generated") {
		t.Errorf("Header should have been suppressed:\n%s", body)
	}
}

func TestBuildWrittenFilesSorted(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "minimal", "mn", minimalStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	for i := 1; i < len(res.WrittenFiles); i++ {
		if res.WrittenFiles[i-1] > res.WrittenFiles[i] {
			t.Errorf("WrittenFiles not sorted at index %d: %q > %q", i, res.WrittenFiles[i-1], res.WrittenFiles[i])
		}
	}
}

func TestBuildMultipleTargets(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	a := makeTarget(t, "minimal", "mn", minimalStub)
	b := makeTarget(t, "httpx@>=0.27,<0.30", "httpx", httpxStub)
	res, err := o.Build(Request{Targets: []Target{a, b}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if len(res.Wrappers) != 2 || len(res.Shims) != 2 {
		t.Fatalf("wrappers=%d shims=%d", len(res.Wrappers), len(res.Shims))
	}
	for _, alias := range []string{"mn", "httpx"} {
		if _, err := os.Stat(filepath.Join(res.VenvRoot, "python_wrap", alias)); err != nil {
			t.Errorf("missing wrapper dir for alias %q: %v", alias, err)
		}
	}
}

func TestPlanNilDriverRejected(t *testing.T) {
	o := &Orchestrator{}
	if _, err := o.Plan(Request{Targets: []Target{makeTarget(t, "rq", "rq", minimalStub)}}); err == nil {
		t.Fatal("expected error for nil driver")
	}
}

func TestBuildSourcePathDepHasEmptyVersion(t *testing.T) {
	d := newTestDriver(t)
	o := NewOrchestrator(d)
	tgt := makeTarget(t, "mypkg@path+../sibling", "mp", minimalStub)
	res, err := o.Build(Request{Targets: []Target{tgt}})
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	body, _ := os.ReadFile(filepath.Join(res.VenvRoot, "pyproject.toml"))
	if !strings.Contains(string(body), `"mypkg"`) {
		t.Errorf("pyproject.toml missing path-source dep:\n%s", body)
	}
}

func TestSharedDepVersionAcrossSources(t *testing.T) {
	mustParse := func(s string) importspec.Spec {
		sp, err := importspec.Parse(s)
		if err != nil {
			t.Fatalf("Parse(%q): %v", s, err)
		}
		return sp
	}
	if got := sharedDepVersion(mustParse("rq@>=2.0,<3.0")); got != ">=2.0, <3.0" {
		t.Errorf("registry = %q", got)
	}
	if got := sharedDepVersion(mustParse("rq")); got != "" {
		t.Errorf("bare = %q", got)
	}
	if got := sharedDepVersion(mustParse("rq@torch+https://example.org")); got != "" {
		t.Errorf("index (empty specifier) = %q", got)
	}
	if got := sharedDepVersion(mustParse("rq@git+https://github.com/u/r#main")); got != "" {
		t.Errorf("git = %q", got)
	}
	if got := sharedDepVersion(mustParse("rq@path+../sibling")); got != "" {
		t.Errorf("path = %q", got)
	}
}
