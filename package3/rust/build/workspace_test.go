package build

import (
	"strings"
	"testing"
)

func TestDefaultWorkspace(t *testing.T) {
	w := DefaultWorkspace()
	if w.Resolver != "2" {
		t.Errorf("Resolver = %q; want %q", w.Resolver, "2")
	}
	if w.Edition != "2021" {
		t.Errorf("Edition = %q; want %q", w.Edition, "2021")
	}
	if w.RustVersion != "1.78" {
		t.Errorf("RustVersion = %q; want %q", w.RustVersion, "1.78")
	}
	if len(w.Members) != 0 {
		t.Errorf("DefaultWorkspace().Members = %d; want 0", len(w.Members))
	}
	if v, ok := w.SharedDependencies["mochi-runtime"]; !ok || v != "0.6" {
		t.Errorf("SharedDependencies[mochi-runtime] = %q,%v; want %q,true", v, ok, "0.6")
	}
	if len(w.Profiles) != 2 {
		t.Errorf("Profiles count = %d; want 2", len(w.Profiles))
	}
	if w.Profiles[0].Name != "release" {
		t.Errorf("Profiles[0].Name = %q; want release", w.Profiles[0].Name)
	}
	if w.Profiles[0].Lto != "fat" || w.Profiles[0].Panic != "abort" {
		t.Errorf("release profile missing lto=fat or panic=abort: %+v", w.Profiles[0])
	}
	if w.Profiles[1].Name != "dev" {
		t.Errorf("Profiles[1].Name = %q; want dev", w.Profiles[1].Name)
	}
}

func TestAddMemberKeepsSorted(t *testing.T) {
	w := DefaultWorkspace()
	w.AddMember(WorkspaceMember{Name: "rust_wrap_regex", Path: "rust_wrap/regex", Kind: MemberWrapper})
	w.AddMember(WorkspaceMember{Name: "rust_wrap_anyhow", Path: "rust_wrap/anyhow", Kind: MemberWrapper})
	w.AddMember(WorkspaceMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})
	got := []string{}
	for _, m := range w.Members {
		got = append(got, m.Path)
	}
	want := []string{"mochi_user", "rust_wrap/anyhow", "rust_wrap/regex"}
	if !equalSlices(got, want) {
		t.Errorf("Members order = %v; want %v", got, want)
	}
}

func TestAddMemberIdempotent(t *testing.T) {
	w := DefaultWorkspace()
	w.AddMember(WorkspaceMember{Name: "a", Path: "rust_wrap/a", Kind: MemberWrapper})
	w.AddMember(WorkspaceMember{Name: "a", Path: "rust_wrap/a", Kind: MemberWrapper})
	if len(w.Members) != 1 {
		t.Errorf("duplicate AddMember produced %d members; want 1", len(w.Members))
	}
}

func TestAddSharedDep(t *testing.T) {
	w := DefaultWorkspace()
	w.AddSharedDep("serde", "1.0")
	w.AddSharedDep("serde", "1.0.150") // replace
	if got := w.SharedDependencies["serde"]; got != "1.0.150" {
		t.Errorf("SharedDependencies[serde] = %q; want 1.0.150", got)
	}
}

func TestRenderRootCargoTomlBasic(t *testing.T) {
	w := DefaultWorkspace()
	w.AddMember(WorkspaceMember{Name: "rust_wrap_serde", Path: "rust_wrap/serde", Kind: MemberWrapper})
	w.AddMember(WorkspaceMember{Name: "mochi_user", Path: "mochi_user", Kind: MemberUser})
	got := w.RenderRootCargoToml()

	wantContains := []string{
		"[workspace]",
		`resolver = "2"`,
		"members = [",
		`    "mochi_user",`,
		`    "rust_wrap/serde",`,
		"[workspace.package]",
		`edition = "2021"`,
		`rust-version = "1.78"`,
		"[workspace.dependencies]",
		`mochi-runtime = "0.6"`,
		"[profile.release]",
		"opt-level = 3",
		"lto = true",
		`panic = "abort"`,
		`strip = "symbols"`,
		"codegen-units = 1",
		"debug = false",
		"[profile.dev]",
		"opt-level = 0",
		"debug = true",
	}
	for _, sub := range wantContains {
		if !strings.Contains(got, sub) {
			t.Errorf("rendered TOML missing %q\n--- output ---\n%s", sub, got)
		}
	}
}

func TestRenderRootCargoTomlDeterministic(t *testing.T) {
	build := func() string {
		w := DefaultWorkspace()
		w.AddMember(WorkspaceMember{Name: "z", Path: "rust_wrap/z", Kind: MemberWrapper})
		w.AddMember(WorkspaceMember{Name: "a", Path: "rust_wrap/a", Kind: MemberWrapper})
		w.AddSharedDep("zlib", "1.0")
		w.AddSharedDep("anyhow", "1.0")
		return w.RenderRootCargoToml()
	}
	first := build()
	for i := 0; i < 10; i++ {
		if got := build(); got != first {
			t.Fatalf("RenderRootCargoToml is non-deterministic on iter %d:\n--- first ---\n%s\n--- got ---\n%s", i, first, got)
		}
	}
}

func TestRenderRootCargoTomlSharedDepsSorted(t *testing.T) {
	w := DefaultWorkspace()
	w.AddSharedDep("zlib", "1.0")
	w.AddSharedDep("anyhow", "1.0")
	w.AddSharedDep("blake3", "1.5")
	got := w.RenderRootCargoToml()

	idxAnyhow := strings.Index(got, "anyhow = ")
	idxBlake3 := strings.Index(got, "blake3 = ")
	idxRuntime := strings.Index(got, "mochi-runtime = ")
	idxZlib := strings.Index(got, "zlib = ")
	if !(idxAnyhow < idxBlake3 && idxBlake3 < idxRuntime && idxRuntime < idxZlib) {
		t.Errorf("shared deps not sorted alphabetically. Indices: anyhow=%d blake3=%d mochi-runtime=%d zlib=%d\n%s",
			idxAnyhow, idxBlake3, idxRuntime, idxZlib, got)
	}
}

func TestRenderRootCargoTomlLtoVariants(t *testing.T) {
	cases := []struct {
		lto         string
		wantSubstr  string
	}{
		{"off", "lto = false"},
		{"fat", "lto = true"},
		{"thin", `lto = "thin"`},
	}
	for _, tc := range cases {
		w := &Workspace{
			Resolver: "2",
			Profiles: []WorkspaceProfile{{Name: "release", Lto: tc.lto}},
		}
		got := w.RenderRootCargoToml()
		if !strings.Contains(got, tc.wantSubstr) {
			t.Errorf("lto=%q rendered missing %q\n%s", tc.lto, tc.wantSubstr, got)
		}
	}
}

func TestRenderRootCargoTomlOptLevelVariants(t *testing.T) {
	cases := []struct {
		level      string
		wantSubstr string
	}{
		{"0", "opt-level = 0"},
		{"3", "opt-level = 3"},
		{"z", `opt-level = "z"`},
		{"s", `opt-level = "s"`},
	}
	for _, tc := range cases {
		w := &Workspace{
			Resolver: "2",
			Profiles: []WorkspaceProfile{{Name: "release", OptLevel: tc.level}},
		}
		got := w.RenderRootCargoToml()
		if !strings.Contains(got, tc.wantSubstr) {
			t.Errorf("opt-level=%q rendered missing %q\n%s", tc.level, tc.wantSubstr, got)
		}
	}
}

func TestWorkspaceValidateRejectsUnknownResolver(t *testing.T) {
	w := &Workspace{Resolver: "1"}
	if err := w.Validate(); err == nil {
		t.Errorf("Validate accepted resolver=1; expected error")
	}
}

func TestWorkspaceValidateRejectsDuplicatePath(t *testing.T) {
	w := &Workspace{
		Resolver: "2",
		Members: []WorkspaceMember{
			{Name: "a", Path: "rust_wrap/x"},
			{Name: "b", Path: "rust_wrap/x"},
		},
	}
	if err := w.Validate(); err == nil {
		t.Errorf("Validate accepted duplicate paths; expected error")
	}
}

func TestWorkspaceValidateRejectsEmptyMember(t *testing.T) {
	w := &Workspace{
		Resolver: "2",
		Members:  []WorkspaceMember{{Name: "", Path: "x"}},
	}
	if err := w.Validate(); err == nil {
		t.Errorf("Validate accepted empty member name; expected error")
	}
	w = &Workspace{
		Resolver: "2",
		Members:  []WorkspaceMember{{Name: "x", Path: ""}},
	}
	if err := w.Validate(); err == nil {
		t.Errorf("Validate accepted empty member path; expected error")
	}
}

func TestWorkspaceValidateRejectsBuiltinProfileInherits(t *testing.T) {
	w := &Workspace{
		Resolver: "2",
		Profiles: []WorkspaceProfile{{Name: "release", Inherits: "dev"}},
	}
	if err := w.Validate(); err == nil {
		t.Errorf("Validate accepted built-in profile inheriting; expected error")
	}
}

func TestWorkspaceValidateAcceptsCustomProfileInherits(t *testing.T) {
	w := &Workspace{
		Resolver: "2",
		Profiles: []WorkspaceProfile{{Name: "fast-release", Inherits: "release", OptLevel: "3"}},
	}
	if err := w.Validate(); err != nil {
		t.Errorf("Validate rejected custom profile inheriting: %v", err)
	}
}

func TestWorkspaceMemberKindString(t *testing.T) {
	cases := map[WorkspaceMemberKind]string{
		MemberUser:    "user",
		MemberWrapper: "wrapper",
		MemberRuntime: "runtime",
		WorkspaceMemberKind(99): "unknown",
	}
	for k, want := range cases {
		if got := k.String(); got != want {
			t.Errorf("(%d).String() = %q; want %q", int(k), got, want)
		}
	}
}

func TestRenderRootCargoTomlHasGeneratedHeader(t *testing.T) {
	w := DefaultWorkspace()
	got := w.RenderRootCargoToml()
	if !strings.Contains(got, "Auto-generated by MEP-73 bridge") {
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
