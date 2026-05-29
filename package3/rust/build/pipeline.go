package build

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"mochi/package3/rust/emit"
	"mochi/package3/rust/rustdoc"
	"mochi/package3/rust/wrapper"
)

// ImportRef is one resolved `import rust "<crate>@<version>" as <alias>`
// statement from the user program. The Alias is the binding name the
// user introduced; the bridge does not consult it during synthesis but
// stores it so later code generation can map back from a Mochi call
// site to the wrapper's emitted shim file.
type ImportRef struct {
	Crate   string
	Version string
	Alias   string
}

// ImportRefError signals a structurally invalid ImportRef (empty crate
// or version). It is returned by Pipeline.Resolve at the start of a
// run so the caller can fail fast.
type ImportRefError struct {
	Ref    ImportRef
	Reason string
}

func (e *ImportRefError) Error() string {
	return fmt.Sprintf("invalid import rust ref {crate=%q version=%q alias=%q}: %s",
		e.Ref.Crate, e.Ref.Version, e.Ref.Alias, e.Reason)
}

// SurfaceProvider resolves a (crate, version) pair to a phase-2
// ApiSurface. Phase 7 is content-agnostic about how the surface comes
// to exist: a production driver wires this to the rustdoc-JSON
// ingest, but tests and the embedded fixture corpus inject precanned
// surfaces directly. The provider is consulted once per ImportRef in
// the order the refs are supplied.
type SurfaceProvider interface {
	Surface(crate, version string) (*rustdoc.ApiSurface, error)
}

// SurfaceProviderFunc adapts a plain function to the SurfaceProvider
// interface. Useful for tests and one-shot in-memory providers.
type SurfaceProviderFunc func(crate, version string) (*rustdoc.ApiSurface, error)

// Surface dispatches to the underlying function.
func (f SurfaceProviderFunc) Surface(crate, version string) (*rustdoc.ApiSurface, error) {
	return f(crate, version)
}

// ResolvedCrate is the result of running one ImportRef through the
// pipeline: the synthesised wrapper crate plus the Mochi-side extern
// + alias emit. It carries enough state for the caller to either
// materialise the files directly or to consult the in-memory bundle
// (the lockfile integration in phase 8 will need both).
type ResolvedCrate struct {
	Ref     ImportRef
	Crate   *wrapper.Crate
	Mochi   emit.Files
	// MemberPath is the workspace-root-relative path the wrapper
	// crate is materialised under, e.g. "rust_wrap/hex".
	MemberPath string
}

// PipelineResult is the bundle returned by Pipeline.Resolve. The
// Workspace already has every wrapper registered as a member and
// every upstream crate registered under [workspace.dependencies].
type PipelineResult struct {
	Workspace *Workspace
	Resolved  []ResolvedCrate
}

// Pipeline drives the end-to-end MEP-73 bridge synthesis:
//
//	import rust refs --[SurfaceProvider]--> rustdoc.ApiSurface
//	  --[wrapper.Synth]--> *wrapper.Crate
//	    --[wrapper.EmitLibRS / EmitCargoTOML / EmitSkippedTXT]--> Rust files
//	    --[emit.Emit]--> Mochi extern + alias files
//	  --[Workspace.AddMember / AddSharedDep]--> workspace registration
//
// The pipeline is stateless: each Resolve call produces a fresh
// PipelineResult. MaterialiseWorkspace is an idempotent on-disk
// projection of a PipelineResult.
type Pipeline struct {
	Driver   *Driver
	Provider SurfaceProvider
}

// Resolve runs each ImportRef through the synthesis pipeline. Refs
// are processed in input order; their wrappers register under their
// canonical "rust_wrap/<crate>" path so duplicate refs collapse
// deterministically.
//
// Resolve does NOT touch the filesystem. Callers wanting on-disk
// artefacts call MaterialiseWorkspace next.
func (p *Pipeline) Resolve(refs []ImportRef) (*PipelineResult, error) {
	if p == nil {
		return nil, errors.New("pipeline: nil receiver")
	}
	if p.Provider == nil {
		return nil, errors.New("pipeline: no SurfaceProvider configured")
	}
	w := DefaultWorkspace()
	seen := map[string]struct{}{}
	out := make([]ResolvedCrate, 0, len(refs))
	for _, ref := range refs {
		if err := validateRef(ref); err != nil {
			return nil, err
		}
		key := ref.Crate + "@" + ref.Version
		if _, dup := seen[key]; dup {
			continue
		}
		seen[key] = struct{}{}

		surface, err := p.Provider.Surface(ref.Crate, ref.Version)
		if err != nil {
			return nil, fmt.Errorf("pipeline: surface for %s: %w", key, err)
		}
		if surface == nil {
			return nil, fmt.Errorf("pipeline: surface for %s: provider returned nil surface", key)
		}
		c := wrapper.Synth(ref.Crate, ref.Version, surface)
		files := emit.Emit(c)
		memberPath := "rust_wrap/" + sanitisePathSegment(ref.Crate)
		w.AddMember(WorkspaceMember{
			Name: c.Name,
			Path: memberPath,
			Kind: MemberWrapper,
		})
		w.AddSharedDep(ref.Crate, "="+ref.Version)
		out = append(out, ResolvedCrate{
			Ref:        ref,
			Crate:      c,
			Mochi:      files,
			MemberPath: memberPath,
		})
	}
	// Order resolved entries by member path so the result is byte-stable
	// against arbitrary input permutations of an otherwise-identical
	// dependency set.
	sort.Slice(out, func(i, j int) bool {
		return out[i].MemberPath < out[j].MemberPath
	})
	return &PipelineResult{Workspace: w, Resolved: out}, nil
}

// MaterialiseWorkspace projects a PipelineResult onto disk under the
// Driver's work-dir. The directory layout matches MEP-73 spec §6:
//
//	<workdir>/rust_workspace/
//	  Cargo.toml                       # workspace root manifest
//	  .gitignore
//	  rust_wrap/<crate>/
//	    Cargo.toml                     # wrapper.EmitCargoTOML
//	    src/lib.rs                     # wrapper.EmitLibRS
//	    SKIPPED.txt                    # wrapper.EmitSkippedTXT
//	  mochi/
//	    <crate>_extern.mochi           # emit.Files.ExternMochi
//	    <crate>.mochi                  # emit.Files.AliasMochi
//
// The function is idempotent: re-running it with the same result
// rewrites identical bytes to the same paths.
//
// The Driver MUST have had PrepareWorkspace called first.
func (p *Pipeline) MaterialiseWorkspace(result *PipelineResult) (string, error) {
	if p == nil || p.Driver == nil {
		return "", errors.New("pipeline: nil pipeline or driver")
	}
	if result == nil {
		return "", errors.New("pipeline: nil PipelineResult")
	}
	root, err := p.Driver.WriteWorkspaceRoot(result.Workspace)
	if err != nil {
		return "", err
	}
	for _, rc := range result.Resolved {
		wrapDir := filepath.Join(root, filepath.FromSlash(rc.MemberPath))
		srcDir := filepath.Join(wrapDir, "src")
		if err := os.MkdirAll(srcDir, 0o755); err != nil {
			return "", fmt.Errorf("pipeline: mkdir %s: %w", srcDir, err)
		}
		write := func(rel, body string) error {
			path := filepath.Join(wrapDir, filepath.FromSlash(rel))
			return os.WriteFile(path, []byte(body), 0o644)
		}
		if err := write("Cargo.toml", wrapper.EmitCargoTOML(rc.Crate)); err != nil {
			return "", err
		}
		if err := write("src/lib.rs", wrapper.EmitLibRS(rc.Crate)); err != nil {
			return "", err
		}
		if err := write("SKIPPED.txt", wrapper.EmitSkippedTXT(rc.Crate)); err != nil {
			return "", err
		}
	}
	mochiDir := filepath.Join(root, "mochi")
	if len(result.Resolved) > 0 {
		if err := os.MkdirAll(mochiDir, 0o755); err != nil {
			return "", fmt.Errorf("pipeline: mkdir %s: %w", mochiDir, err)
		}
	}
	for _, rc := range result.Resolved {
		stem := sanitisePathSegment(rc.Ref.Crate)
		externPath := filepath.Join(mochiDir, stem+"_extern.mochi")
		aliasPath := filepath.Join(mochiDir, stem+".mochi")
		if err := os.WriteFile(externPath, []byte(rc.Mochi.ExternMochi), 0o644); err != nil {
			return "", fmt.Errorf("pipeline: write %s: %w", externPath, err)
		}
		if err := os.WriteFile(aliasPath, []byte(rc.Mochi.AliasMochi), 0o644); err != nil {
			return "", fmt.Errorf("pipeline: write %s: %w", aliasPath, err)
		}
	}
	return root, nil
}

func validateRef(ref ImportRef) error {
	if ref.Crate == "" {
		return &ImportRefError{Ref: ref, Reason: "empty crate"}
	}
	if ref.Version == "" {
		return &ImportRefError{Ref: ref, Reason: "empty version"}
	}
	return nil
}

// sanitisePathSegment lower-cases and replaces non-portable bytes
// with '_' so each crate's wrapper directory is a safe filesystem
// path. The bridge already enforces a Cargo-conservative crate-name
// shape upstream (lowercase + digits + '_' / '-' / leading letter)
// in the parser, so the transformation here is a defence-in-depth
// projection.
func sanitisePathSegment(s string) string {
	if s == "" {
		return ""
	}
	b := make([]byte, 0, len(s))
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case c >= 'a' && c <= 'z', c >= '0' && c <= '9', c == '_', c == '-':
			b = append(b, c)
		case c >= 'A' && c <= 'Z':
			b = append(b, c-'A'+'a')
		default:
			b = append(b, '_')
		}
	}
	out := string(b)
	out = strings.TrimRight(out, "/")
	return out
}
