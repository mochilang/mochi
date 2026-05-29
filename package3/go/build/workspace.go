// Package build is the MEP-74 bridge build-orchestration package. Phase 0
// lands the skeleton: the Driver struct (WorkDir / CacheDir), the Workspace
// type that models the go.work multi-module workspace topology, and the
// go.work renderer. Later phases extend this package with the `go build
// -buildmode=c-archive` invocation, artefact linking, and incremental
// rebuild keying.
package build

import (
	"fmt"
	"sort"
	"strings"
)

// Workspace models the Go workspace topology the bridge synthesises at
// build time. A workspace is a `go.work` file (Go 1.18+) that pulls in
// member modules: the wrapper modules for each imported Go dep, the
// optional bridge runtime helper module, and the user's top-level module
// emitted by MEP-54's TargetGo.
//
// The workspace file follows the go.work syntax:
//
//	go 1.21
//
//	use (
//	    ./mochi_user
//	    ./go_wrap/github.com/spf13/cobra
//	)
//
//	replace example.com/foo => ./local_fork/foo
//
// The bridge does NOT manage the user's own go.mod requires; those come
// from `[go-dependencies]` in mochi.toml. The workspace is a build-time
// concern only and lives entirely under `<workdir>/go_workspace/`.
type Workspace struct {
	// GoVersion is the Go-toolchain floor declared in the `go` directive
	// of the synthesised go.work. The bridge picks the maximum of this
	// floor and each source module's `go` directive so the resulting
	// toolchain pick is the highest required by any participant.
	GoVersion string
	// Modules is the ordered list of `use` directives. Each entry is one
	// member module rooted at a path relative to the workspace directory.
	// The renderer sorts modules alphabetically for reproducibility.
	Modules []WorkspaceModule
	// ReplaceDirectives is the set of `replace` lines emitted into the
	// go.work file. Keyed by module path (the LHS of the replace), value
	// is the local path or module@version pair (the RHS).
	ReplaceDirectives map[string]string
	// CgoBuildMode controls the `-buildmode=` flag passed to `go build`
	// during the build phase (phase 9). Defaults to CgoArchive.
	CgoBuildMode CgoBuildMode
	// Trimpath toggles the `-trimpath` flag for reproducible builds.
	// Defaults to true.
	Trimpath bool
	// LdFlags is the extra `-ldflags` value passed to `go build`. The
	// bridge sets `-s -w` here by default to strip the resulting archive.
	LdFlags string
}

// WorkspaceModule describes a single Go module that participates in the
// synthesised workspace. The Path is relative to the workspace root.
type WorkspaceModule struct {
	// ImportPath is the module path declared in the module's go.mod
	// `module` directive. For wrapper modules, this is the synthesised
	// path (e.g. `mochilang.local/go_wrap/cobra`).
	ImportPath string
	// Path is the directory relative to the workspace root that contains
	// the module's go.mod.
	Path string
	// Kind classifies the module's role for documentation purposes; it
	// does not affect the rendered go.work file.
	Kind WorkspaceModuleKind
}

// WorkspaceModuleKind classifies a workspace module's role.
type WorkspaceModuleKind int

const (
	// ModuleUser is the user's Mochi-emitted top-level Go module.
	ModuleUser WorkspaceModuleKind = iota
	// ModuleWrapper is a synthesised `go_wrap/<module>` cgo wrapper.
	ModuleWrapper
	// ModuleRuntime is a vendored bridge-runtime helper module.
	ModuleRuntime
)

// String renders the kind as a short token. Used in diagnostics.
func (k WorkspaceModuleKind) String() string {
	switch k {
	case ModuleUser:
		return "user"
	case ModuleWrapper:
		return "wrapper"
	case ModuleRuntime:
		return "runtime"
	default:
		return "unknown"
	}
}

// CgoBuildMode is the build mode the bridge passes to `go build`.
type CgoBuildMode int

const (
	// CgoArchive emits a static archive (`-buildmode=c-archive`) plus
	// header. This is the bridge's default for the consume direction.
	CgoArchive CgoBuildMode = iota
	// CgoShared emits a shared library (`-buildmode=c-shared`).
	// Available as an opt-in for the publish direction.
	CgoShared
	// CgoNone disables cgo entirely. Used for TinyGo embedded and
	// wasm-wasip1 targets where cgo is not available.
	CgoNone
)

// String renders the build mode as the `go build -buildmode=<value>`
// argument. CgoNone has no `-buildmode` equivalent and renders as the
// empty string; the build phase must select an alternate flag for it.
func (m CgoBuildMode) String() string {
	switch m {
	case CgoArchive:
		return "c-archive"
	case CgoShared:
		return "c-shared"
	case CgoNone:
		return ""
	default:
		return "unknown"
	}
}

// DefaultWorkspace returns a Workspace with the bridge's recommended
// defaults: go-version floor 1.21, no modules, no replace directives,
// CgoArchive build mode, trimpath enabled, ldflags `-s -w`. Callers add
// modules via AddModule and replace directives via AddReplace.
func DefaultWorkspace() *Workspace {
	return &Workspace{
		GoVersion:         "1.21",
		Modules:           nil,
		ReplaceDirectives: map[string]string{},
		CgoBuildMode:      CgoArchive,
		Trimpath:          true,
		LdFlags:           "-s -w",
	}
}

// AddModule inserts a `use` directive into the workspace. The modules
// slice is kept sorted by Path so the rendered go.work is deterministic.
// Duplicate Paths are silently dropped.
func (w *Workspace) AddModule(m WorkspaceModule) {
	for _, existing := range w.Modules {
		if existing.Path == m.Path {
			return
		}
	}
	w.Modules = append(w.Modules, m)
	sort.Slice(w.Modules, func(i, j int) bool {
		return w.Modules[i].Path < w.Modules[j].Path
	})
}

// AddReplace adds (or replaces) a `replace` directive. Called during the
// build phase when a wrapper module needs a local path override (for
// example when the user pointed `[go-dependencies] foo = { path = "..." }`).
func (w *Workspace) AddReplace(module, target string) {
	if w.ReplaceDirectives == nil {
		w.ReplaceDirectives = map[string]string{}
	}
	w.ReplaceDirectives[module] = target
}

// RenderGoWork returns the synthesised go.work file as a string. The
// output is deterministic: modules are alphabetised by Path, replace
// directives are sorted by module name, and the comment header identifies
// the bridge.
//
// The renderer uses a small hand-rolled go.work writer rather than
// importing `golang.org/x/mod/modfile` because (1) the schema is fixed and
// small, (2) the output must be byte-stable for the workspace-cache key,
// and (3) avoiding the dep keeps the package self-contained at phase 0.
// Phase 9 may switch to modfile once it joins the bridge's dependency set.
func (w *Workspace) RenderGoWork() string {
	var b strings.Builder

	b.WriteString("// Auto-generated by MEP-74 bridge. Do not edit by hand.\n")
	b.WriteString("// Regenerate via `mochi pkg lock`.\n\n")

	if w.GoVersion != "" {
		fmt.Fprintf(&b, "go %s\n\n", w.GoVersion)
	}

	if len(w.Modules) > 0 {
		b.WriteString("use (\n")
		for _, m := range w.Modules {
			fmt.Fprintf(&b, "\t./%s\n", m.Path)
		}
		b.WriteString(")\n")
	}

	if len(w.ReplaceDirectives) > 0 {
		if len(w.Modules) > 0 {
			b.WriteString("\n")
		}
		names := make([]string, 0, len(w.ReplaceDirectives))
		for name := range w.ReplaceDirectives {
			names = append(names, name)
		}
		sort.Strings(names)
		for _, name := range names {
			fmt.Fprintf(&b, "replace %s => %s\n", name, w.ReplaceDirectives[name])
		}
	}

	return b.String()
}

// Validate checks the workspace for structural problems that would
// produce a broken go.work file. Phase 0 enforces:
//   - GoVersion is a dotted-numeric string like "1.21" (basic shape check),
//   - every module has a non-empty ImportPath and Path,
//   - module Paths are unique,
//   - module Paths are relative (no leading slash, no `..`).
func (w *Workspace) Validate() error {
	if w.GoVersion != "" && !isValidGoVersion(w.GoVersion) {
		return fmt.Errorf("workspace: unsupported go-version %q (expected like \"1.21\")", w.GoVersion)
	}
	seen := map[string]struct{}{}
	for _, m := range w.Modules {
		if m.ImportPath == "" {
			return fmt.Errorf("workspace: module at path %q has empty ImportPath", m.Path)
		}
		if m.Path == "" {
			return fmt.Errorf("workspace: module %q has empty Path", m.ImportPath)
		}
		if strings.HasPrefix(m.Path, "/") || strings.Contains(m.Path, "..") {
			return fmt.Errorf("workspace: module path %q must be relative without parent traversal", m.Path)
		}
		if _, dup := seen[m.Path]; dup {
			return fmt.Errorf("workspace: duplicate module path %q", m.Path)
		}
		seen[m.Path] = struct{}{}
	}
	return nil
}

// isValidGoVersion does a shape check on a go-version string. It accepts
// dotted-numeric strings with at least one dot (e.g. "1.21", "1.23.4")
// and rejects everything else.
func isValidGoVersion(v string) bool {
	if v == "" {
		return false
	}
	parts := strings.Split(v, ".")
	if len(parts) < 2 {
		return false
	}
	for _, p := range parts {
		if p == "" {
			return false
		}
		for _, c := range p {
			if c < '0' || c > '9' {
				return false
			}
		}
	}
	return true
}
