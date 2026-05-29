package build

import (
	"strings"
	"testing"
)

func TestDefaultVenv(t *testing.T) {
	v := DefaultVenv()
	if v.RequiresPython != ">=3.12,<3.15" {
		t.Errorf("RequiresPython = %q; want >=3.12,<3.15", v.RequiresPython)
	}
	if v.Implementation != "cpython" {
		t.Errorf("Implementation = %q; want cpython", v.Implementation)
	}
	if v.RuntimeMode != "embedded" {
		t.Errorf("RuntimeMode = %q; want embedded", v.RuntimeMode)
	}
	if v.AsyncMode != "per-call" {
		t.Errorf("AsyncMode = %q; want per-call", v.AsyncMode)
	}
	if v.FreeThreaded {
		t.Errorf("FreeThreaded = true; want false (phase 0 default)")
	}
	if !v.StubgenFallback {
		t.Errorf("StubgenFallback = false; want true")
	}
	if v.SidecarGlob != "*_externs.py" {
		t.Errorf("SidecarGlob = %q; want *_externs.py", v.SidecarGlob)
	}
	if len(v.Members) != 0 {
		t.Errorf("DefaultVenv().Members = %d; want 0", len(v.Members))
	}
	if got, ok := v.SharedDependencies["mochi-python-runtime"]; !ok || got != ">=0.6,<0.7" {
		t.Errorf("SharedDependencies[mochi-python-runtime] = %q,%v; want >=0.6,<0.7,true", got, ok)
	}
}

func TestAddMemberKeepsSorted(t *testing.T) {
	v := DefaultVenv()
	v.AddMember(VenvMember{Name: "regex", Path: "python_wrap/regex", Kind: MemberWrapper})
	v.AddMember(VenvMember{Name: "httpx", Path: "python_wrap/httpx", Kind: MemberWrapper})
	v.AddMember(VenvMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})
	got := []string{}
	for _, m := range v.Members {
		got = append(got, m.Path)
	}
	want := []string{"mochi_user", "python_wrap/httpx", "python_wrap/regex"}
	if !equalSlices(got, want) {
		t.Errorf("Members order = %v; want %v", got, want)
	}
}

func TestAddMemberIdempotent(t *testing.T) {
	v := DefaultVenv()
	v.AddMember(VenvMember{Name: "a", Path: "python_wrap/a", Kind: MemberWrapper})
	v.AddMember(VenvMember{Name: "a", Path: "python_wrap/a", Kind: MemberWrapper})
	if len(v.Members) != 1 {
		t.Errorf("duplicate AddMember produced %d members; want 1", len(v.Members))
	}
}

func TestAddSharedDep(t *testing.T) {
	v := DefaultVenv()
	v.AddSharedDep("httpx", ">=0.27,<1.0")
	v.AddSharedDep("httpx", "==0.27.0") // replace
	if got := v.SharedDependencies["httpx"]; got != "==0.27.0" {
		t.Errorf("SharedDependencies[httpx] = %q; want ==0.27.0", got)
	}
}

func TestRenderPyprojectTomlBasic(t *testing.T) {
	v := DefaultVenv()
	v.AddMember(VenvMember{Name: "httpx", Path: "python_wrap/httpx", Kind: MemberWrapper})
	v.AddMember(VenvMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})
	v.AddSharedDep("httpx", "==0.27.0")
	got := v.RenderPyprojectToml()

	wantContains := []string{
		"[build-system]",
		`requires = ["setuptools>=68", "wheel"]`,
		`build-backend = "setuptools.build_meta"`,
		"[project]",
		`name = "mochi-app"`,
		`version = "0.0.0"`,
		`requires-python = ">=3.12,<3.15"`,
		"dependencies = [",
		`    "httpx==0.27.0",`,
		`    "mochi-python-runtime>=0.6,<0.7",`,
		"[python]",
		`implementation = "cpython"`,
		`runtime-mode = "embedded"`,
		`async-mode = "per-call"`,
		"free-threaded = false",
		"stubgen-fallback = true",
		`sidecar-glob = "*_externs.py"`,
		"[python.wrappers]",
		`"python_wrap/httpx" = { name = "httpx", kind = "wrapper" }`,
		"[python.members]",
		`"mochi_user" = { name = "mochi_user", kind = "user" }`,
	}
	for _, sub := range wantContains {
		if !strings.Contains(got, sub) {
			t.Errorf("rendered TOML missing %q\n--- output ---\n%s", sub, got)
		}
	}
}

func TestRenderPyprojectTomlDeterministic(t *testing.T) {
	build := func() string {
		v := DefaultVenv()
		v.AddMember(VenvMember{Name: "z", Path: "python_wrap/z", Kind: MemberWrapper})
		v.AddMember(VenvMember{Name: "a", Path: "python_wrap/a", Kind: MemberWrapper})
		v.AddSharedDep("zarr", "==2.18.0")
		v.AddSharedDep("anyio", "==4.4.0")
		return v.RenderPyprojectToml()
	}
	first := build()
	for i := 0; i < 10; i++ {
		if got := build(); got != first {
			t.Fatalf("RenderPyprojectToml is non-deterministic on iter %d:\n--- first ---\n%s\n--- got ---\n%s", i, first, got)
		}
	}
}

func TestRenderPyprojectTomlSharedDepsSorted(t *testing.T) {
	v := DefaultVenv()
	v.AddSharedDep("zarr", "==2.18.0")
	v.AddSharedDep("anyio", "==4.4.0")
	v.AddSharedDep("httpx", "==0.27.0")
	got := v.RenderPyprojectToml()

	idxAnyio := strings.Index(got, `"anyio==4.4.0"`)
	idxHttpx := strings.Index(got, `"httpx==0.27.0"`)
	idxRuntime := strings.Index(got, `"mochi-python-runtime>=0.6,<0.7"`)
	idxZarr := strings.Index(got, `"zarr==2.18.0"`)
	if !(idxAnyio > 0 && idxHttpx > idxAnyio && idxRuntime > idxHttpx && idxZarr > idxRuntime) {
		t.Errorf("shared deps not sorted alphabetically. Indices: anyio=%d httpx=%d mochi-python-runtime=%d zarr=%d\n%s",
			idxAnyio, idxHttpx, idxRuntime, idxZarr, got)
	}
}

func TestRenderPyprojectTomlFreeThreadedToggle(t *testing.T) {
	v := DefaultVenv()
	v.FreeThreaded = true
	got := v.RenderPyprojectToml()
	if !strings.Contains(got, "free-threaded = true") {
		t.Errorf("rendered TOML missing free-threaded = true\n%s", got)
	}
}

func TestRenderPyprojectTomlStubgenToggle(t *testing.T) {
	v := DefaultVenv()
	v.StubgenFallback = false
	got := v.RenderPyprojectToml()
	if !strings.Contains(got, "stubgen-fallback = false") {
		t.Errorf("rendered TOML missing stubgen-fallback = false\n%s", got)
	}
}

func TestRenderPyprojectTomlEmptyMembersOmitsTable(t *testing.T) {
	v := DefaultVenv()
	got := v.RenderPyprojectToml()
	if strings.Contains(got, "[python.wrappers]") {
		t.Errorf("rendered TOML should omit [python.wrappers] when no members:\n%s", got)
	}
	if strings.Contains(got, "[python.members]") {
		t.Errorf("rendered TOML should omit [python.members] when no members:\n%s", got)
	}
}

func TestRenderPyprojectTomlWrapperVsUserSplit(t *testing.T) {
	v := DefaultVenv()
	v.AddMember(VenvMember{Name: "httpx", Path: "python_wrap/httpx", Kind: MemberWrapper})
	v.AddMember(VenvMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})
	v.AddMember(VenvMember{Name: "mochi_python_runtime", Path: "python_wrap/runtime", Kind: MemberRuntime})
	got := v.RenderPyprojectToml()
	wrappersBlock := strings.Index(got, "[python.wrappers]")
	membersBlock := strings.Index(got, "[python.members]")
	if wrappersBlock < 0 || membersBlock < 0 {
		t.Fatalf("blocks missing: wrappers=%d members=%d\n%s", wrappersBlock, membersBlock, got)
	}
	wrappersSection := got[wrappersBlock:membersBlock]
	if strings.Contains(wrappersSection, `"mochi_user"`) {
		t.Errorf("[python.wrappers] should not include the user member:\n%s", wrappersSection)
	}
	membersSection := got[membersBlock:]
	if !strings.Contains(membersSection, `"mochi_user"`) {
		t.Errorf("[python.members] should include the user member:\n%s", membersSection)
	}
	if !strings.Contains(membersSection, `kind = "runtime"`) {
		t.Errorf("[python.members] should include the runtime kind\n%s", membersSection)
	}
}

func TestVenvValidateRejectsBadImplementation(t *testing.T) {
	v := &Venv{Implementation: "pypy"}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted Implementation=pypy; expected error")
	}
}

func TestVenvValidateRejectsBadRuntimeMode(t *testing.T) {
	v := &Venv{RuntimeMode: "wasm"}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted RuntimeMode=wasm; expected error")
	}
}

func TestVenvValidateRejectsBadAsyncMode(t *testing.T) {
	v := &Venv{AsyncMode: "trio"}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted AsyncMode=trio; expected error")
	}
}

func TestVenvValidateAcceptsEmptyOptionalFields(t *testing.T) {
	v := &Venv{}
	if err := v.Validate(); err != nil {
		t.Errorf("Validate rejected empty defaults: %v", err)
	}
}

func TestVenvValidateAcceptsSubprocessAndPersistent(t *testing.T) {
	v := &Venv{Implementation: "cpython", RuntimeMode: "subprocess", AsyncMode: "persistent"}
	if err := v.Validate(); err != nil {
		t.Errorf("Validate rejected subprocess+persistent: %v", err)
	}
}

func TestVenvValidateRejectsDuplicatePath(t *testing.T) {
	v := &Venv{
		Implementation: "cpython",
		Members: []VenvMember{
			{Name: "a", Path: "python_wrap/x"},
			{Name: "b", Path: "python_wrap/x"},
		},
	}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted duplicate paths; expected error")
	}
}

func TestVenvValidateRejectsEmptyMember(t *testing.T) {
	v := &Venv{
		Implementation: "cpython",
		Members:        []VenvMember{{Name: "", Path: "x"}},
	}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted empty member name; expected error")
	}
	v = &Venv{
		Implementation: "cpython",
		Members:        []VenvMember{{Name: "x", Path: ""}},
	}
	if err := v.Validate(); err == nil {
		t.Errorf("Validate accepted empty member path; expected error")
	}
}

func TestVenvMemberKindString(t *testing.T) {
	cases := map[VenvMemberKind]string{
		MemberUser:         "user",
		MemberWrapper:      "wrapper",
		MemberRuntime:      "runtime",
		VenvMemberKind(99): "unknown",
	}
	for k, want := range cases {
		if got := k.String(); got != want {
			t.Errorf("(%d).String() = %q; want %q", int(k), got, want)
		}
	}
}

func TestRenderPyprojectTomlHasGeneratedHeader(t *testing.T) {
	v := DefaultVenv()
	got := v.RenderPyprojectToml()
	if !strings.Contains(got, "Auto-generated by MEP-71 bridge") {
		t.Errorf("rendered TOML missing auto-generated header\n%s", got)
	}
}

func equalSlices(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
