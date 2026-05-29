package build

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"

	"mochi/package3/python/emit"
	pyerrors "mochi/package3/python/errors"
	"mochi/package3/python/importspec"
	"mochi/package3/python/stubs"
	"mochi/package3/python/wrapper"
)

// Target is one `import python "<spec>" as <alias>` statement the caller
// asks the orchestrator to materialise into the venv. The PYISource is the
// merged Phase 3 stub content for the distribution; the caller is responsible
// for running stub discovery and producing the bytes.
type Target struct {
	// Spec is the parsed `import python` body (Phase 7).
	Spec importspec.Spec
	// Alias is the Mochi alias the user picked (`as <alias>`). The wrapper
	// module path inside the venv is keyed by Alias.
	Alias string
	// PYISource is the .pyi text Phase 3 produced for Spec.Name. The
	// orchestrator feeds it to stubs.ParsePYI -> wrapper.Synthesise -> EmitShim.
	PYISource string
}

// Request is the input to Orchestrator.Build. It carries one Target per
// `import python` declaration plus optional overrides for the synthesised
// venv, wrapper, and shim emitter.
type Request struct {
	Targets     []Target
	Venv        *Venv
	WrapperOpts wrapper.Options
	ShimOpts    emit.Options
}

// Result is what Orchestrator.Build returns. WorkDir is the driver's scratch
// directory; VenvRoot is the python_workspace directory inside it. Wrappers
// and Shims have one entry per Target in declaration order. Skipped is the
// concatenated SkipReport list (qualified with `<pkg>.<item>` by Phase 5).
// WrittenFiles lists every path the orchestrator wrote, in the order it
// wrote them.
type Result struct {
	WorkDir      string
	VenvRoot     string
	Wrappers     []*wrapper.Wrapper
	Shims        []*emit.Shim
	Skipped      []pyerrors.SkipReport
	WrittenFiles []string
}

// Orchestrator drives the per-target wrapper + shim synthesis loop and lays
// the result out under the driver's work-dir.
type Orchestrator struct {
	driver *Driver
}

// NewOrchestrator wraps an existing Driver. The Driver supplies the work-dir
// allocation and Cleanup semantics; the Orchestrator only handles per-target
// composition.
func NewOrchestrator(d *Driver) *Orchestrator {
	return &Orchestrator{driver: d}
}

// Plan validates a Request without touching the filesystem. It returns the
// resolved Venv (with wrapper members added per Target) and the per-target
// wrappers / shims, so callers can run `mochi pkg lock --dry-run` cheaply.
func (o *Orchestrator) Plan(req Request) (*Result, error) {
	if o.driver == nil {
		return nil, fmt.Errorf("orchestrator: nil driver")
	}
	if len(req.Targets) == 0 {
		return nil, fmt.Errorf("orchestrator: at least one Target required")
	}
	seenAlias := map[string]struct{}{}
	for i, t := range req.Targets {
		if t.Alias == "" {
			return nil, fmt.Errorf("orchestrator: target %d (%q) has empty alias", i, t.Spec.RawName)
		}
		if t.Spec.Name == "" {
			return nil, fmt.Errorf("orchestrator: target %d alias %q has empty spec name", i, t.Alias)
		}
		if _, dup := seenAlias[t.Alias]; dup {
			return nil, fmt.Errorf("orchestrator: duplicate alias %q", t.Alias)
		}
		seenAlias[t.Alias] = struct{}{}
	}
	venv := req.Venv
	if venv == nil {
		venv = DefaultVenv()
	}
	res := &Result{}
	for _, t := range req.Targets {
		surface, err := stubs.ParsePYI(t.PYISource)
		if err != nil {
			return nil, fmt.Errorf("orchestrator: target %q parse pyi: %w", t.Alias, err)
		}
		w, err := wrapper.Synthesise(t.Spec.Name, surface, req.WrapperOpts)
		if err != nil {
			return nil, fmt.Errorf("orchestrator: target %q synthesise: %w", t.Alias, err)
		}
		shim, err := emit.EmitShim(w, req.ShimOpts)
		if err != nil {
			return nil, fmt.Errorf("orchestrator: target %q emit: %w", t.Alias, err)
		}
		res.Wrappers = append(res.Wrappers, w)
		res.Shims = append(res.Shims, shim)
		res.Skipped = append(res.Skipped, w.Skipped...)
		venv.AddMember(VenvMember{
			Name: t.Alias,
			Path: "python_wrap/" + t.Alias,
			Kind: MemberWrapper,
		})
		venv.AddSharedDep(t.Spec.Name, sharedDepVersion(t.Spec))
	}
	req.Venv = venv
	if err := venv.Validate(); err != nil {
		return nil, fmt.Errorf("orchestrator: venv: %w", err)
	}
	return res, nil
}

// Build runs Plan and then materialises the workspace under the driver's
// work-dir. Layout:
//
//	<work-dir>/python_workspace/
//	  pyproject.toml
//	  .gitignore
//	  _mochi_wrap.py                       # shared runtime
//	  python_wrap/<alias>/
//	    __init__.py
//	    <pkg>_externs.py
//	    <pkg>_externs.pyi
//	    <pkg>_shim.mochi
//	    SKIPPED.txt                        # only when wrapper had skipped items
//
// Build is idempotent for a stable Request: the SHA-256 in each Shim is
// recomputed and the on-disk content is byte-stable across calls.
func (o *Orchestrator) Build(req Request) (*Result, error) {
	plan, err := o.Plan(req)
	if err != nil {
		return nil, err
	}
	venv := req.Venv
	if venv == nil {
		venv = DefaultVenv()
		// Re-run Plan side: AddMember/AddSharedDep already mutated the user
		// Venv inside Plan; the nil case fell through with a fresh Venv that
		// Plan also populated. We mirror the same Venv that Plan used.
		// Reuse the populated members by walking req.Targets.
		for _, t := range req.Targets {
			venv.AddMember(VenvMember{
				Name: t.Alias,
				Path: "python_wrap/" + t.Alias,
				Kind: MemberWrapper,
			})
			venv.AddSharedDep(t.Spec.Name, sharedDepVersion(t.Spec))
		}
	}
	if _, err := o.driver.PrepareVenv(); err != nil {
		return nil, err
	}
	plan.WorkDir = o.driver.WorkDir()
	root, err := o.driver.WriteVenvRoot(venv)
	if err != nil {
		return nil, err
	}
	plan.VenvRoot = root
	plan.WrittenFiles = append(plan.WrittenFiles,
		filepath.Join(root, "pyproject.toml"),
		filepath.Join(root, ".gitignore"),
	)
	runtimePath := filepath.Join(root, "_mochi_wrap.py")
	if err := os.WriteFile(runtimePath, []byte(wrapper.Runtime()), 0o644); err != nil {
		return nil, fmt.Errorf("orchestrator: write runtime: %w", err)
	}
	plan.WrittenFiles = append(plan.WrittenFiles, runtimePath)
	runtimeStubPath := filepath.Join(root, "_mochi_wrap.pyi")
	if err := os.WriteFile(runtimeStubPath, []byte(wrapper.RuntimeStub()), 0o644); err != nil {
		return nil, fmt.Errorf("orchestrator: write runtime stub: %w", err)
	}
	plan.WrittenFiles = append(plan.WrittenFiles, runtimeStubPath)
	for i, t := range req.Targets {
		w := plan.Wrappers[i]
		shim := plan.Shims[i]
		dir := filepath.Join(root, "python_wrap", t.Alias)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return nil, fmt.Errorf("orchestrator: target %q mkdir: %w", t.Alias, err)
		}
		entries := []struct {
			path, body string
		}{
			{filepath.Join(dir, "__init__.py"), initPyTemplate(w)},
			{filepath.Join(dir, w.Module+".py"), w.PySource},
			{filepath.Join(dir, w.Module+".pyi"), w.PYISource},
			{filepath.Join(dir, t.Spec.Name+"_shim.mochi"), shim.Source},
		}
		for _, e := range entries {
			if err := os.WriteFile(e.path, []byte(e.body), 0o644); err != nil {
				return nil, fmt.Errorf("orchestrator: target %q write %s: %w", t.Alias, filepath.Base(e.path), err)
			}
			plan.WrittenFiles = append(plan.WrittenFiles, e.path)
		}
		if len(w.Skipped) > 0 {
			skippedPath := filepath.Join(dir, "SKIPPED.txt")
			if err := os.WriteFile(skippedPath, []byte(renderSkipped(w.Skipped)), 0o644); err != nil {
				return nil, fmt.Errorf("orchestrator: target %q write SKIPPED.txt: %w", t.Alias, err)
			}
			plan.WrittenFiles = append(plan.WrittenFiles, skippedPath)
		}
	}
	sort.Strings(plan.WrittenFiles)
	return plan, nil
}

func initPyTemplate(w *wrapper.Wrapper) string {
	return "# Auto-generated by MEP-71 bridge for " + w.Package + ".\n" +
		"from . import " + w.Module + " as externs\n" +
		"__all__ = [\"externs\"]\n"
}

func renderSkipped(reports []pyerrors.SkipReport) string {
	out := "# Auto-generated SKIPPED.txt. Items the wrapper refused.\n"
	for _, r := range reports {
		out += r.ItemPath + "\t" + r.Reason.String() + "\t" + r.Detail + "\n"
	}
	return out
}

// sharedDepVersion renders the [project.dependencies] version requirement for
// a Spec. SourceRegistry / SourceIndex pass the PEP 440 specifier through;
// SourceGit and SourcePath produce empty (the dependency is satisfied by an
// editable / VCS install configured elsewhere).
func sharedDepVersion(s importspec.Spec) string {
	switch s.Source {
	case importspec.SourceRegistry, importspec.SourceIndex:
		return s.Specifier.String()
	default:
		return ""
	}
}
