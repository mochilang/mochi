// Package build is the MEP-71 bridge build-orchestration package. Phase 0
// lands the skeleton: the Driver struct (WorkDir / CacheDir), the Venv type
// that models the per-build Python virtual environment topology, and the
// pyproject.toml renderer. Later phases extend this package with the
// libpython link, the wheel install loop, the wrapper compile, and the
// embedded-vs-subprocess runtime split.
package build

import (
	"fmt"
	"sort"
	"strings"
)

// Venv models the per-build Python virtual environment the bridge synthesises
// to host the user's program plus every wrapper module the import grammar
// pulled in. A Venv has a root pyproject.toml and zero or more member modules
// (one wrapper module per imported PyPI distribution, the runtime module, and
// the user's Mochi-emitted top-level module).
//
// Unlike cargo, Python has no native multi-package workspace, so the bridge
// synthesises a single editable-installable [project] whose dependencies enum
// every wrapper module. The [python.wrappers] table is a bridge-private MEP-71
// extension that records the wrapper module layout so phase 8's build pass
// can locate the per-wrapper PEP 517 backends.
//
//	[project]
//	name = "mochi-app"
//	version = "0.0.0"
//	requires-python = ">=3.12,<3.15"
//
//	[python]
//	implementation = "cpython"
//	runtime-mode = "embedded"
//	async-mode = "per-call"
//
//	[python.wrappers]
//	"python_wrap/httpx" = { version = "0.27.0" }
//
// The bridge does NOT touch the user's Mochi-side mochi.toml; the Venv is a
// build-time concern only and lives entirely under <workdir>/python_workspace/.
type Venv struct {
	// RequiresPython pins the supported CPython range in PEP 440 specifier
	// form (e.g. ">=3.12,<3.15"). Phase 0 supports the 3.12 - 3.14 window.
	RequiresPython string
	// Implementation is the Python implementation tag ("cpython", "pypy",
	// "graalpy"). Phase 0 ships only "cpython".
	Implementation string
	// RuntimeMode selects how Mochi hosts the interpreter. Valid values are
	// "embedded" (link libpython into the Mochi binary) and "subprocess"
	// (fork a Python child and speak JSON-RPC). Phase 0 ships "embedded";
	// phase 14 ships "subprocess".
	RuntimeMode string
	// AsyncMode selects the asyncio bridging strategy. Valid values are
	// "per-call" (a fresh asyncio.run per Mochi await) and "persistent"
	// (a single asyncio loop hosted by the runtime). Phase 0 ships
	// "per-call"; phase 12 ships "persistent".
	AsyncMode string
	// FreeThreaded selects the PEP 703 cp3XYt ABI when true. Phase 0
	// defaults to false; phase 17 turns it on for opt-in users.
	FreeThreaded bool
	// Members is the ordered list of member modules in the venv. The
	// renderer sorts members alphabetically by Path for reproducibility.
	Members []VenvMember
	// SharedDependencies are PyPI requirements (PEP 508 strings, but the
	// value form here is just the version specifier; the name is the map
	// key). They are written into [project.dependencies] in the rendered
	// pyproject.toml.
	SharedDependencies map[string]string
	// StubgenFallback enables the mypy stubgen sandbox when an imported
	// package ships neither inline annotations nor a typeshed stub. Default
	// true; toggleable via [python] in mochi.toml for sandbox-hostile envs.
	StubgenFallback bool
	// SidecarGlob is the glob the wrapper synthesiser uses to discover the
	// user's hand-authored override module per import alias. Default
	// "*_externs.py". Matches the MEP-51 Phase 12 sidecar convention.
	SidecarGlob string
}

// VenvMember describes a single module that participates in the venv. The
// Path is the on-disk directory relative to the venv root.
type VenvMember struct {
	// Name is the Python package import name as it will appear in
	// `import <name>` statements. For wrappers this is the Mochi alias the
	// user picked in their import grammar, not the upstream PyPI name.
	Name string
	// Path is the directory relative to the venv root that contains the
	// member module's __init__.py (or its PEP 517 backend metadata, for
	// wrapper modules).
	Path string
	// Kind classifies the member's role. The kind does not affect the
	// rendered pyproject.toml directly; phase 8 uses it to decide whether
	// to invoke the wrapper compile loop or the user Mochi emit pass.
	Kind VenvMemberKind
}

// VenvMemberKind classifies a venv member's role.
type VenvMemberKind int

const (
	// MemberUser is the user's Mochi-emitted top-level module. Phase 0
	// emits a placeholder; MEP-71 Phase 10 fills it in.
	MemberUser VenvMemberKind = iota
	// MemberWrapper is a synthesised python_wrap/<alias>/ wrapper module
	// produced by the MEP-71 wrapper synthesiser (phase 5).
	MemberWrapper
	// MemberRuntime is the vendored mochi_python_runtime module that hosts
	// the libpython embed shim, the asyncio bridge, and the GC root table.
	MemberRuntime
)

// String renders the kind as a short token. Used in diagnostics.
func (k VenvMemberKind) String() string {
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

// DefaultVenv returns a Venv with the bridge's recommended defaults:
// CPython >=3.12,<3.15, embedded runtime, per-call asyncio, stubgen fallback
// enabled. Callers add members via AddMember and shared deps via AddSharedDep.
func DefaultVenv() *Venv {
	return &Venv{
		RequiresPython:  ">=3.12,<3.15",
		Implementation:  "cpython",
		RuntimeMode:     "embedded",
		AsyncMode:       "per-call",
		FreeThreaded:    false,
		Members:         nil,
		StubgenFallback: true,
		SidecarGlob:     "*_externs.py",
		SharedDependencies: map[string]string{
			"mochi-python-runtime": ">=0.6,<0.7",
		},
	}
}

// AddMember inserts a member module into the venv. The members slice is kept
// sorted by Path so the rendered pyproject.toml is deterministic.
func (v *Venv) AddMember(m VenvMember) {
	for _, existing := range v.Members {
		if existing.Path == m.Path {
			return
		}
	}
	v.Members = append(v.Members, m)
	sort.Slice(v.Members, func(i, j int) bool {
		return v.Members[i].Path < v.Members[j].Path
	})
}

// AddSharedDep adds (or replaces) a [project.dependencies] entry. Called
// during phase 8 build orchestration when each wrapper module declares its
// upstream PyPI dep.
func (v *Venv) AddSharedDep(name, versionReq string) {
	if v.SharedDependencies == nil {
		v.SharedDependencies = map[string]string{}
	}
	v.SharedDependencies[name] = versionReq
}

// RenderPyprojectToml returns the venv root pyproject.toml as a string. The
// output is deterministic: members are alphabetised, shared deps are sorted
// by distribution name, and the bridge-private [python.wrappers] table is
// alphabetised by wrapper path.
//
// The renderer uses a small hand-rolled TOML writer rather than a third-party
// library because (1) the schema is fixed and small, (2) the output must be
// byte-stable for the venv-cache key, and (3) avoiding a dep on
// pelletier/go-toml or burntsushi/toml keeps the package self-contained.
func (v *Venv) RenderPyprojectToml() string {
	var b strings.Builder

	b.WriteString("# Auto-generated by MEP-71 bridge. Do not edit by hand.\n")
	b.WriteString("# Regenerate via `mochi pkg lock`.\n\n")

	b.WriteString("[build-system]\n")
	b.WriteString(`requires = ["setuptools>=68", "wheel"]` + "\n")
	b.WriteString(`build-backend = "setuptools.build_meta"` + "\n\n")

	b.WriteString("[project]\n")
	b.WriteString(`name = "mochi-app"` + "\n")
	b.WriteString(`version = "0.0.0"` + "\n")
	if v.RequiresPython != "" {
		fmt.Fprintf(&b, "requires-python = %q\n", v.RequiresPython)
	}
	if len(v.SharedDependencies) > 0 {
		b.WriteString("dependencies = [\n")
		names := make([]string, 0, len(v.SharedDependencies))
		for name := range v.SharedDependencies {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			req := v.SharedDependencies[name]
			if req == "" {
				fmt.Fprintf(&b, "    %q,\n", name)
			} else {
				fmt.Fprintf(&b, "    %q,\n", name+req)
			}
		}
		b.WriteString("]\n")
	}
	b.WriteString("\n")

	b.WriteString("[python]\n")
	if v.Implementation != "" {
		fmt.Fprintf(&b, "implementation = %q\n", v.Implementation)
	}
	if v.RuntimeMode != "" {
		fmt.Fprintf(&b, "runtime-mode = %q\n", v.RuntimeMode)
	}
	if v.AsyncMode != "" {
		fmt.Fprintf(&b, "async-mode = %q\n", v.AsyncMode)
	}
	fmt.Fprintf(&b, "free-threaded = %t\n", v.FreeThreaded)
	fmt.Fprintf(&b, "stubgen-fallback = %t\n", v.StubgenFallback)
	if v.SidecarGlob != "" {
		fmt.Fprintf(&b, "sidecar-glob = %q\n", v.SidecarGlob)
	}
	b.WriteString("\n")

	if len(v.Members) > 0 {
		b.WriteString("[python.wrappers]\n")
		for _, m := range v.Members {
			if m.Kind != MemberWrapper {
				continue
			}
			fmt.Fprintf(&b, "%q = { name = %q, kind = %q }\n", m.Path, m.Name, m.Kind.String())
		}
		b.WriteString("\n")
		b.WriteString("[python.members]\n")
		for _, m := range v.Members {
			fmt.Fprintf(&b, "%q = { name = %q, kind = %q }\n", m.Path, m.Name, m.Kind.String())
		}
		b.WriteString("\n")
	}

	return b.String()
}

// Validate checks the venv for structural problems that would produce a
// broken pyproject.toml. Phase 0 enforces:
//   - Implementation is "cpython" (the only supported value),
//   - RuntimeMode is "embedded" or "subprocess",
//   - AsyncMode is "per-call" or "persistent",
//   - every member has a non-empty Name and Path,
//   - member paths are unique.
func (v *Venv) Validate() error {
	if v.Implementation != "" && v.Implementation != "cpython" {
		return fmt.Errorf("venv: unsupported implementation %q (only \"cpython\" is supported in phase 0)", v.Implementation)
	}
	switch v.RuntimeMode {
	case "", "embedded", "subprocess":
	default:
		return fmt.Errorf("venv: unsupported runtime-mode %q (want \"embedded\" or \"subprocess\")", v.RuntimeMode)
	}
	switch v.AsyncMode {
	case "", "per-call", "persistent":
	default:
		return fmt.Errorf("venv: unsupported async-mode %q (want \"per-call\" or \"persistent\")", v.AsyncMode)
	}
	seen := map[string]struct{}{}
	for _, m := range v.Members {
		if m.Name == "" {
			return fmt.Errorf("venv: member at path %q has empty Name", m.Path)
		}
		if m.Path == "" {
			return fmt.Errorf("venv: member %q has empty Path", m.Name)
		}
		if _, dup := seen[m.Path]; dup {
			return fmt.Errorf("venv: duplicate member path %q", m.Path)
		}
		seen[m.Path] = struct{}{}
	}
	return nil
}
