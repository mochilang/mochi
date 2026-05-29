// Package build is the MEP-73 bridge build-orchestration package. Phase 0
// lands the skeleton: the Driver struct (WorkDir / CacheDir), the Workspace
// type that models the cargo workspace topology, and the Cargo.toml renderer.
// Later phases extend this package with cargo invocation, artefact linking,
// and incremental rebuild keying.
package build

import (
	"fmt"
	"sort"
	"strings"
)

// Workspace models the cargo workspace topology the bridge synthesises at
// build time. A workspace has a root Cargo.toml and zero or more member
// crates (the wrapper crates for each imported Rust dep, the runtime crate,
// and the user's top-level crate emitted by MEP-53's TargetRust).
//
// The workspace root TOML follows the cargo workspace inheritance model:
//
//	[workspace]
//	resolver = "2"
//	members = ["mochi_user", "rust_wrap/serde", "rust_wrap/regex"]
//
//	[workspace.package]
//	edition = "2021"
//	rust-version = "1.78"
//
//	[workspace.dependencies]
//	mochi-runtime = "0.6"
//
// The bridge does NOT manage the user's own Cargo.toml dependencies; those
// come from [rust-dependencies] in mochi.toml. The workspace is a build-time
// concern only and lives entirely under <workdir>/rust_workspace/.
type Workspace struct {
	// Resolver pins the cargo dependency resolver version. Cargo v2 resolver
	// is the recommended default for edition 2021+. Phase 0 ships only the
	// v2 resolver; later phases may add v3 when it stabilises.
	Resolver string
	// Edition is the Rust edition shared across all member crates by default.
	// Individual member crates may override.
	Edition string
	// RustVersion is the MSRV declared in [workspace.package] rust-version.
	RustVersion string
	// Members is the ordered list of member crate paths relative to the
	// workspace root. The renderer sorts members alphabetically for
	// reproducibility.
	Members []WorkspaceMember
	// SharedDependencies are entries that appear under [workspace.dependencies]
	// and are inherited by member crates via dep = { workspace = true }.
	// Keyed by crate name, value is the version requirement.
	SharedDependencies map[string]string
	// Profiles control [profile.<name>] sections of the workspace root.
	// Phase 0 ships release + dev profiles with panic = "abort" and
	// the wrapper-recommended optimisation settings.
	Profiles []WorkspaceProfile
}

// WorkspaceMember describes a single cargo crate that participates in the
// workspace. The Path is relative to the workspace root.
type WorkspaceMember struct {
	// Name is the crate name as it will appear in Cargo.toml's [package].
	Name string
	// Path is the directory relative to the workspace root that contains
	// the member's Cargo.toml.
	Path string
	// Kind classifies the member's role for documentation purposes; it
	// does not affect the rendered TOML.
	Kind WorkspaceMemberKind
}

// WorkspaceMemberKind classifies a workspace member's role.
type WorkspaceMemberKind int

const (
	// MemberUser is the user's Mochi-emitted top-level crate.
	MemberUser WorkspaceMemberKind = iota
	// MemberWrapper is a synthesised rust_wrap/<crate>/ wrapper crate.
	MemberWrapper
	// MemberRuntime is a vendored mochi-runtime-* crate.
	MemberRuntime
)

// String renders the kind as a short token. Used in diagnostics.
func (k WorkspaceMemberKind) String() string {
	switch k {
	case MemberUser:
		return "user"
	case MemberWrapper:
		return "wrapper"
	case MemberRuntime:
		return "runtime"
	default:
		return "unknown"
	}
}

// WorkspaceProfile is a [profile.<name>] entry in the workspace root TOML.
type WorkspaceProfile struct {
	// Name is the profile name (release, dev, test, bench, or a user-defined
	// profile that inherits from one of these).
	Name string
	// Inherits is the profile this one inherits from. Empty for built-in
	// profiles.
	Inherits string
	// OptLevel is the cargo opt-level. May be a numeric string ("0", "1",
	// "2", "3", "z", "s") or empty to leave the cargo default.
	OptLevel string
	// Lto controls link-time optimisation. May be "off", "thin", "fat", or
	// empty.
	Lto string
	// Panic is "abort" or "unwind" or empty.
	Panic string
	// Strip is "none", "symbols", "debuginfo", or empty.
	Strip string
	// CodegenUnits is the cargo codegen-units setting; 0 leaves the default.
	CodegenUnits int
	// Debug controls debug-info emission. Phase 0 defaults release to false,
	// dev to true. nil leaves the cargo default.
	Debug *bool
}

// DefaultWorkspace returns a Workspace with the bridge's recommended defaults:
// resolver v2, edition 2021, rust-version 1.78, release + dev profiles with
// panic = "abort" and the wrapper-recommended optimisation flags. Callers
// add members via AddMember and shared deps via AddSharedDep.
func DefaultWorkspace() *Workspace {
	debugTrue := true
	debugFalse := false
	return &Workspace{
		Resolver:    "2",
		Edition:     "2021",
		RustVersion: "1.78",
		Members:     nil,
		SharedDependencies: map[string]string{
			"mochi-runtime": "0.6",
		},
		Profiles: []WorkspaceProfile{
			{
				Name:         "release",
				OptLevel:     "3",
				Lto:          "fat",
				Panic:        "abort",
				Strip:        "symbols",
				CodegenUnits: 1,
				Debug:        &debugFalse,
			},
			{
				Name:     "dev",
				OptLevel: "0",
				Panic:    "abort",
				Debug:    &debugTrue,
			},
		},
	}
}

// AddMember inserts a member crate into the workspace. The members slice is
// kept sorted by Path so the rendered TOML is deterministic.
func (w *Workspace) AddMember(m WorkspaceMember) {
	for _, existing := range w.Members {
		if existing.Path == m.Path {
			return
		}
	}
	w.Members = append(w.Members, m)
	sort.Slice(w.Members, func(i, j int) bool {
		return w.Members[i].Path < w.Members[j].Path
	})
}

// AddSharedDep adds (or replaces) a [workspace.dependencies] entry. Called
// during phase 7 build orchestration when each wrapper crate declares its
// upstream dep.
func (w *Workspace) AddSharedDep(name, versionReq string) {
	if w.SharedDependencies == nil {
		w.SharedDependencies = map[string]string{}
	}
	w.SharedDependencies[name] = versionReq
}

// RenderRootCargoToml returns the workspace root Cargo.toml as a string. The
// output is deterministic: members are alphabetised, shared deps are sorted
// by crate name, and profiles render in declaration order.
//
// The renderer uses a small hand-rolled TOML writer rather than a third-party
// library because (1) the schema is fixed and small, (2) the output must be
// byte-stable for the workspace-cache key, and (3) avoiding a dep on
// burntsushi/toml or pelletier/go-toml keeps the package self-contained.
func (w *Workspace) RenderRootCargoToml() string {
	var b strings.Builder

	b.WriteString("# Auto-generated by MEP-73 bridge. Do not edit by hand.\n")
	b.WriteString("# Regenerate via `mochi pkg lock`.\n\n")

	b.WriteString("[workspace]\n")
	if w.Resolver != "" {
		fmt.Fprintf(&b, "resolver = %q\n", w.Resolver)
	}
	b.WriteString("members = [\n")
	for _, m := range w.Members {
		fmt.Fprintf(&b, "    %q,\n", m.Path)
	}
	b.WriteString("]\n\n")

	if w.Edition != "" || w.RustVersion != "" {
		b.WriteString("[workspace.package]\n")
		if w.Edition != "" {
			fmt.Fprintf(&b, "edition = %q\n", w.Edition)
		}
		if w.RustVersion != "" {
			fmt.Fprintf(&b, "rust-version = %q\n", w.RustVersion)
		}
		b.WriteString("\n")
	}

	if len(w.SharedDependencies) > 0 {
		b.WriteString("[workspace.dependencies]\n")
		names := make([]string, 0, len(w.SharedDependencies))
		for name := range w.SharedDependencies {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			fmt.Fprintf(&b, "%s = %q\n", name, w.SharedDependencies[name])
		}
		b.WriteString("\n")
	}

	for _, p := range w.Profiles {
		renderProfile(&b, p)
	}

	return b.String()
}

func renderProfile(b *strings.Builder, p WorkspaceProfile) {
	fmt.Fprintf(b, "[profile.%s]\n", p.Name)
	if p.Inherits != "" {
		fmt.Fprintf(b, "inherits = %q\n", p.Inherits)
	}
	if p.OptLevel != "" {
		// opt-level is rendered as a quoted string for "z" / "s" and as a
		// number for "0"-"3". Cargo accepts both but the bare-int form is
		// canonical for numeric levels.
		if isNumericOptLevel(p.OptLevel) {
			fmt.Fprintf(b, "opt-level = %s\n", p.OptLevel)
		} else {
			fmt.Fprintf(b, "opt-level = %q\n", p.OptLevel)
		}
	}
	if p.Lto != "" {
		// "off" renders as bare false, "fat" as bare true, "thin" as quoted
		// string. Cargo accepts all three forms.
		switch p.Lto {
		case "off":
			b.WriteString("lto = false\n")
		case "fat":
			b.WriteString("lto = true\n")
		default:
			fmt.Fprintf(b, "lto = %q\n", p.Lto)
		}
	}
	if p.Panic != "" {
		fmt.Fprintf(b, "panic = %q\n", p.Panic)
	}
	if p.Strip != "" {
		fmt.Fprintf(b, "strip = %q\n", p.Strip)
	}
	if p.CodegenUnits > 0 {
		fmt.Fprintf(b, "codegen-units = %d\n", p.CodegenUnits)
	}
	if p.Debug != nil {
		fmt.Fprintf(b, "debug = %t\n", *p.Debug)
	}
	b.WriteString("\n")
}

func isNumericOptLevel(s string) bool {
	return len(s) == 1 && s[0] >= '0' && s[0] <= '3'
}

// Validate checks the workspace for structural problems that would produce a
// broken Cargo.toml. Phase 0 enforces:
//   - resolver is "2" (the only supported value),
//   - every member has a non-empty Name and Path,
//   - member paths are unique,
//   - no profile is named "" or has both Inherits and a built-in name.
func (w *Workspace) Validate() error {
	if w.Resolver != "" && w.Resolver != "2" {
		return fmt.Errorf("workspace: unsupported resolver %q (only \"2\" is supported)", w.Resolver)
	}
	seen := map[string]struct{}{}
	for _, m := range w.Members {
		if m.Name == "" {
			return fmt.Errorf("workspace: member at path %q has empty Name", m.Path)
		}
		if m.Path == "" {
			return fmt.Errorf("workspace: member %q has empty Path", m.Name)
		}
		if _, dup := seen[m.Path]; dup {
			return fmt.Errorf("workspace: duplicate member path %q", m.Path)
		}
		seen[m.Path] = struct{}{}
	}
	for _, p := range w.Profiles {
		if p.Name == "" {
			return fmt.Errorf("workspace: profile with empty name")
		}
		if p.Inherits != "" && isBuiltinProfile(p.Name) {
			return fmt.Errorf("workspace: built-in profile %q cannot inherit from another", p.Name)
		}
	}
	return nil
}

func isBuiltinProfile(name string) bool {
	switch name {
	case "release", "dev", "test", "bench":
		return true
	default:
		return false
	}
}
