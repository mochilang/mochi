package build

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	rerr "mochi/package3/rust/errors"
	"mochi/package3/rust/rustdoc"
)

// hexLikeSurface mirrors the wrapper-package fixture: the hex crate's
// three public functions plus one phase-2 SkipReport. We re-declare
// the fixture here so the build package does not take a test-only
// dependency on the wrapper package's _test sources.
func hexLikeSurface() *rustdoc.ApiSurface {
	str := rustdoc.Type{Primitive: "str"}
	u8 := rustdoc.Type{Primitive: "u8"}
	char := rustdoc.Type{Primitive: "char"}
	bytesSlice := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a",
		Type:     rustdoc.Type{Slice: &u8},
	}}
	strBorrow := rustdoc.Type{BorrowedRef: &rustdoc.BorrowedRefType{
		Lifetime: "'a",
		Type:     str,
	}}
	stringPath := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:String", Path: "std::string::String",
	}}
	vecU8 := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:Vec_u8", Path: "std::vec::Vec",
		Args: &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{
			Args: []rustdoc.GenericArg{{Type: &u8}},
		}},
	}}
	fromHexError := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:FromHexError", Path: "hex::FromHexError",
	}}
	resultVecU8 := rustdoc.Type{ResolvedPath: &rustdoc.PathType{
		ID: "id:Result", Path: "core::result::Result",
		Args: &rustdoc.GenericArgs{AngleBracketed: &rustdoc.AngleBracketedArgs{
			Args: []rustdoc.GenericArg{{Type: &vecU8}, {Type: &fromHexError}},
		}},
	}}
	return &rustdoc.ApiSurface{
		CrateName:    "hex",
		CrateVersion: "0.4.3",
		Functions: []rustdoc.FunctionEntry{
			{
				ID:   "fn:encode",
				Path: []string{"hex", "encode"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "data", Type: bytesSlice},
				},
				Output: &stringPath,
			},
			{
				ID:   "fn:decode",
				Path: []string{"hex", "decode"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "input", Type: strBorrow},
				},
				Output: &resultVecU8,
			},
			{
				ID:   "fn:to_upper_hex",
				Path: []string{"hex", "to_upper_hex"},
				Inputs: []rustdoc.ParamEntry{
					{Name: "value", Type: u8},
				},
				Output: &char,
			},
		},
		Skipped: []rerr.SkipReport{
			{
				ItemPath: "hex::decode_to_slice",
				Reason:   rerr.SkipGeneric,
				Detail:   "generic parameter T",
			},
		},
	}
}

// staticProvider serves canned surfaces by (crate, version) key.
type staticProvider map[string]*rustdoc.ApiSurface

func (s staticProvider) Surface(crate, version string) (*rustdoc.ApiSurface, error) {
	if s == nil {
		return nil, errors.New("nil provider")
	}
	key := crate + "@" + version
	surf, ok := s[key]
	if !ok {
		return nil, fmt.Errorf("no fixture for %s", key)
	}
	return surf, nil
}

func newHexProvider() staticProvider {
	return staticProvider{"hex@0.4.3": hexLikeSurface()}
}

func newPipeline(t *testing.T) *Pipeline {
	t.Helper()
	d := NewDriver(Options{NoCache: true})
	t.Cleanup(func() { _ = d.Cleanup() })
	return &Pipeline{Driver: d, Provider: newHexProvider()}
}

func TestPipelineResolveBasic(t *testing.T) {
	p := newPipeline(t)
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got := len(res.Resolved); got != 1 {
		t.Fatalf("Resolved count = %d; want 1", got)
	}
	rc := res.Resolved[0]
	if rc.Crate.Name != "mochi_wrap_hex" {
		t.Errorf("Crate.Name = %q; want mochi_wrap_hex", rc.Crate.Name)
	}
	if rc.MemberPath != "rust_wrap/hex" {
		t.Errorf("MemberPath = %q; want rust_wrap/hex", rc.MemberPath)
	}
	if rc.Mochi.ExternMochi == "" || rc.Mochi.AliasMochi == "" {
		t.Errorf("Mochi files empty: %+v", rc.Mochi)
	}
}

func TestPipelineResolveRegistersMember(t *testing.T) {
	p := newPipeline(t)
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if len(res.Workspace.Members) != 1 {
		t.Fatalf("Members = %d; want 1", len(res.Workspace.Members))
	}
	m := res.Workspace.Members[0]
	if m.Name != "mochi_wrap_hex" || m.Path != "rust_wrap/hex" || m.Kind != MemberWrapper {
		t.Errorf("Member = %+v; want {mochi_wrap_hex, rust_wrap/hex, MemberWrapper}", m)
	}
}

func TestPipelineResolveRegistersSharedDep(t *testing.T) {
	p := newPipeline(t)
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	got, ok := res.Workspace.SharedDependencies["hex"]
	if !ok {
		t.Fatalf("SharedDependencies missing hex: %+v", res.Workspace.SharedDependencies)
	}
	if got != "=0.4.3" {
		t.Errorf("SharedDependencies[hex] = %q; want =0.4.3", got)
	}
}

func TestPipelineResolveDeduplicates(t *testing.T) {
	p := newPipeline(t)
	res, err := p.Resolve([]ImportRef{
		{Crate: "hex", Version: "0.4.3", Alias: "hex"},
		{Crate: "hex", Version: "0.4.3", Alias: "hex2"},
	})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got := len(res.Resolved); got != 1 {
		t.Errorf("duplicate refs produced %d resolved entries; want 1", got)
	}
}

func TestPipelineResolveDeterministicOrder(t *testing.T) {
	provider := staticProvider{
		"hex@0.4.3":     hexLikeSurface(),
		"anyhow@1.0.86": emptySurface("anyhow", "1.0.86"),
	}
	mkPipeline := func() *Pipeline {
		d := NewDriver(Options{NoCache: true})
		t.Cleanup(func() { _ = d.Cleanup() })
		return &Pipeline{Driver: d, Provider: provider}
	}
	p1 := mkPipeline()
	p2 := mkPipeline()
	res1, err := p1.Resolve([]ImportRef{
		{Crate: "hex", Version: "0.4.3"},
		{Crate: "anyhow", Version: "1.0.86"},
	})
	if err != nil {
		t.Fatalf("Resolve 1: %v", err)
	}
	res2, err := p2.Resolve([]ImportRef{
		{Crate: "anyhow", Version: "1.0.86"},
		{Crate: "hex", Version: "0.4.3"},
	})
	if err != nil {
		t.Fatalf("Resolve 2: %v", err)
	}
	paths := func(rs []ResolvedCrate) []string {
		out := make([]string, len(rs))
		for i, r := range rs {
			out[i] = r.MemberPath
		}
		return out
	}
	gotA := paths(res1.Resolved)
	gotB := paths(res2.Resolved)
	want := []string{"rust_wrap/anyhow", "rust_wrap/hex"}
	if !equalSlices(gotA, want) || !equalSlices(gotB, want) {
		t.Errorf("non-deterministic order: gotA=%v gotB=%v want=%v", gotA, gotB, want)
	}
}

func emptySurface(name, version string) *rustdoc.ApiSurface {
	return &rustdoc.ApiSurface{CrateName: name, CrateVersion: version}
}

func TestPipelineResolveRejectsEmptyCrate(t *testing.T) {
	p := newPipeline(t)
	_, err := p.Resolve([]ImportRef{{Crate: "", Version: "1.0"}})
	var refErr *ImportRefError
	if !errors.As(err, &refErr) {
		t.Fatalf("err = %v; want *ImportRefError", err)
	}
}

func TestPipelineResolveRejectsEmptyVersion(t *testing.T) {
	p := newPipeline(t)
	_, err := p.Resolve([]ImportRef{{Crate: "hex", Version: ""}})
	var refErr *ImportRefError
	if !errors.As(err, &refErr) {
		t.Fatalf("err = %v; want *ImportRefError", err)
	}
}

func TestPipelineResolveBubblesProviderError(t *testing.T) {
	p := &Pipeline{
		Driver:   NewDriver(Options{NoCache: true}),
		Provider: SurfaceProviderFunc(func(crate, version string) (*rustdoc.ApiSurface, error) {
			return nil, errors.New("boom")
		}),
	}
	t.Cleanup(func() { _ = p.Driver.Cleanup() })
	_, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3"}})
	if err == nil || !strings.Contains(err.Error(), "boom") {
		t.Fatalf("err = %v; want wrap of 'boom'", err)
	}
}

func TestPipelineResolveRejectsNilSurface(t *testing.T) {
	p := &Pipeline{
		Driver:   NewDriver(Options{NoCache: true}),
		Provider: SurfaceProviderFunc(func(crate, version string) (*rustdoc.ApiSurface, error) {
			return nil, nil
		}),
	}
	t.Cleanup(func() { _ = p.Driver.Cleanup() })
	_, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3"}})
	if err == nil || !strings.Contains(err.Error(), "nil surface") {
		t.Fatalf("err = %v; want 'nil surface' message", err)
	}
}

func TestPipelineResolveRejectsNilProvider(t *testing.T) {
	p := &Pipeline{Driver: NewDriver(Options{NoCache: true})}
	t.Cleanup(func() { _ = p.Driver.Cleanup() })
	_, err := p.Resolve(nil)
	if err == nil || !strings.Contains(err.Error(), "SurfaceProvider") {
		t.Fatalf("err = %v; want 'SurfaceProvider' message", err)
	}
}

func TestPipelineResolveRejectsNilReceiver(t *testing.T) {
	var p *Pipeline
	_, err := p.Resolve(nil)
	if err == nil {
		t.Fatalf("nil receiver Resolve did not error")
	}
}

func TestMaterialiseWorkspaceWritesAllFiles(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	p := &Pipeline{Driver: d, Provider: newHexProvider()}
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	root, err := p.MaterialiseWorkspace(res)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}
	wantFiles := []string{
		"Cargo.toml",
		".gitignore",
		"rust_wrap/hex/Cargo.toml",
		"rust_wrap/hex/src/lib.rs",
		"rust_wrap/hex/SKIPPED.txt",
		"mochi/hex_extern.mochi",
		"mochi/hex.mochi",
	}
	for _, rel := range wantFiles {
		path := filepath.Join(root, filepath.FromSlash(rel))
		info, err := os.Stat(path)
		if err != nil {
			t.Errorf("missing %s: %v", rel, err)
			continue
		}
		if info.Size() == 0 {
			t.Errorf("%s is zero bytes", rel)
		}
	}
}

func TestMaterialiseWorkspaceContentsAreFromEmitters(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	p := &Pipeline{Driver: d, Provider: newHexProvider()}
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	root, err := p.MaterialiseWorkspace(res)
	if err != nil {
		t.Fatalf("MaterialiseWorkspace: %v", err)
	}
	// Wrapper Cargo.toml pins upstream version
	cargo, err := os.ReadFile(filepath.Join(root, "rust_wrap", "hex", "Cargo.toml"))
	if err != nil {
		t.Fatalf("read wrapper Cargo.toml: %v", err)
	}
	if !strings.Contains(string(cargo), `hex = "=0.4.3"`) {
		t.Errorf("wrapper Cargo.toml missing hex = \"=0.4.3\"\n%s", cargo)
	}
	if !strings.Contains(string(cargo), `crate-type = ["cdylib", "rlib"]`) {
		t.Errorf("wrapper Cargo.toml missing crate-type cdylib+rlib\n%s", cargo)
	}
	// Wrapper lib.rs carries the extern-C symbol
	lib, err := os.ReadFile(filepath.Join(root, "rust_wrap", "hex", "src", "lib.rs"))
	if err != nil {
		t.Fatalf("read wrapper lib.rs: %v", err)
	}
	for _, want := range []string{
		`pub unsafe extern "C" fn mochi_hex_encode`,
		`pub unsafe extern "C" fn mochi_hex_decode`,
		`pub unsafe extern "C" fn mochi_hex_to_upper_hex`,
	} {
		if !strings.Contains(string(lib), want) {
			t.Errorf("lib.rs missing %q\n%s", want, lib)
		}
	}
	// Mochi extern file carries the extern fun lines
	ex, err := os.ReadFile(filepath.Join(root, "mochi", "hex_extern.mochi"))
	if err != nil {
		t.Fatalf("read hex_extern.mochi: %v", err)
	}
	if !strings.Contains(string(ex), "extern fun mochi_hex_encode") {
		t.Errorf("hex_extern.mochi missing extern fun mochi_hex_encode\n%s", ex)
	}
	// Mochi alias file carries the short re-exports
	al, err := os.ReadFile(filepath.Join(root, "mochi", "hex.mochi"))
	if err != nil {
		t.Fatalf("read hex.mochi: %v", err)
	}
	if !strings.Contains(string(al), "fun encode(") {
		t.Errorf("hex.mochi missing fun encode(\n%s", al)
	}
	// Workspace root Cargo.toml lists the wrapper as a member
	rootCargo, err := os.ReadFile(filepath.Join(root, "Cargo.toml"))
	if err != nil {
		t.Fatalf("read root Cargo.toml: %v", err)
	}
	if !strings.Contains(string(rootCargo), `"rust_wrap/hex"`) {
		t.Errorf("workspace Cargo.toml missing member rust_wrap/hex\n%s", rootCargo)
	}
	if !strings.Contains(string(rootCargo), `hex = "=0.4.3"`) {
		t.Errorf("workspace Cargo.toml missing shared dep hex = \"=0.4.3\"\n%s", rootCargo)
	}
}

func TestMaterialiseWorkspaceIsIdempotent(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	if _, err := d.PrepareWorkspace(); err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	p := &Pipeline{Driver: d, Provider: newHexProvider()}
	res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3"}})
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	root1, err := p.MaterialiseWorkspace(res)
	if err != nil {
		t.Fatalf("first MaterialiseWorkspace: %v", err)
	}
	first, err := os.ReadFile(filepath.Join(root1, "rust_wrap", "hex", "src", "lib.rs"))
	if err != nil {
		t.Fatalf("first lib.rs read: %v", err)
	}
	root2, err := p.MaterialiseWorkspace(res)
	if err != nil {
		t.Fatalf("second MaterialiseWorkspace: %v", err)
	}
	if root1 != root2 {
		t.Errorf("MaterialiseWorkspace returned different roots: %q vs %q", root1, root2)
	}
	second, err := os.ReadFile(filepath.Join(root2, "rust_wrap", "hex", "src", "lib.rs"))
	if err != nil {
		t.Fatalf("second lib.rs read: %v", err)
	}
	if string(first) != string(second) {
		t.Errorf("MaterialiseWorkspace not idempotent: lib.rs differs across runs")
	}
}

func TestMaterialiseWorkspaceRejectsNilResult(t *testing.T) {
	p := &Pipeline{Driver: NewDriver(Options{NoCache: true})}
	t.Cleanup(func() { _ = p.Driver.Cleanup() })
	if _, err := p.MaterialiseWorkspace(nil); err == nil {
		t.Errorf("MaterialiseWorkspace(nil) did not error")
	}
}

func TestMaterialiseWorkspaceRejectsNilDriver(t *testing.T) {
	p := &Pipeline{}
	if _, err := p.MaterialiseWorkspace(&PipelineResult{Workspace: DefaultWorkspace()}); err == nil {
		t.Errorf("MaterialiseWorkspace with nil Driver did not error")
	}
}

func TestSanitisePathSegmentLowercases(t *testing.T) {
	if got := sanitisePathSegment("FooBar"); got != "foobar" {
		t.Errorf("FooBar = %q; want foobar", got)
	}
}

func TestSanitisePathSegmentReplacesUnsafe(t *testing.T) {
	if got := sanitisePathSegment("a/b"); got != "a_b" {
		t.Errorf("a/b = %q; want a_b", got)
	}
}

func TestSanitisePathSegmentPreservesHyphen(t *testing.T) {
	if got := sanitisePathSegment("rand-chacha"); got != "rand-chacha" {
		t.Errorf("rand-chacha = %q; want rand-chacha", got)
	}
}

func TestImportRefErrorMessage(t *testing.T) {
	err := &ImportRefError{
		Ref:    ImportRef{Crate: "", Version: "1.0"},
		Reason: "empty crate",
	}
	if got := err.Error(); !strings.Contains(got, "empty crate") || !strings.Contains(got, `crate=""`) {
		t.Errorf("Error() = %q; want it to mention 'empty crate' and crate=\"\"", got)
	}
}

func TestSurfaceProviderFuncDispatches(t *testing.T) {
	called := false
	f := SurfaceProviderFunc(func(crate, version string) (*rustdoc.ApiSurface, error) {
		called = true
		if crate != "x" || version != "1" {
			t.Errorf("got (%q, %q); want (x, 1)", crate, version)
		}
		return &rustdoc.ApiSurface{}, nil
	})
	if _, err := f.Surface("x", "1"); err != nil {
		t.Fatalf("Surface: %v", err)
	}
	if !called {
		t.Errorf("SurfaceProviderFunc did not dispatch")
	}
}
