// Package build phase 9 lands the build-orchestration surface on top
// of the phase-0 workspace skeleton. The entry point is
// Driver.Build, which:
//
//  1. assembles the workspace topology from the wrapper.Result tree
//     (a wrapper module per imported Go dep);
//  2. writes each wrapper module to disk under
//     <work-dir>/go_workspace/go_wrap/<flat-module>/;
//  3. renders the workspace go.work;
//  4. invokes `go build -buildmode=c-archive` on every wrapper module
//     with the deterministic flag set (-trimpath, -buildvcs=false,
//     -ldflags=-s -w);
//  5. links the produced *.a + *.h artefacts into the BuildArtefacts
//     return value, which the MEP-54 link step consumes.
//
// The build is keyed on a content-addressed cache key derived from
// every wrapper file's sha256, the workspace go.work bytes, and the
// caller-supplied target tuple (GOOS / GOARCH). A cache hit copies
// the cached artefacts back into the work-dir and short-circuits
// the `go build` invocation.
package build

import (
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sort"
	"strings"

	"mochi/package3/go/wrapper"
)

// BuildPlan is the input to Driver.Build. It describes one or more
// wrapper modules that need to be compiled into c-archive artefacts.
type BuildPlan struct {
	// Wrappers is the per-module set of synthesised cgo wrappers, one
	// per `import go` line in the user program. Each entry is a
	// wrapper.Result produced by phase 6.
	Wrappers []wrapper.Result
	// Target chooses the GOOS / GOARCH the wrappers compile for.
	// Empty fields default to the host (runtime.GOOS / runtime.GOARCH).
	Target Target
	// CgoEnabled controls the CGO_ENABLED env var. Phase 9 defaults
	// to true on host-cgo-capable targets, false on wasm.
	CgoEnabled bool
	// ExtraEnv is appended to the `go build` env. Useful for callers
	// that need to set GOFLAGS, CC, or similar.
	ExtraEnv []string
	// SkipBuild lets callers exercise the workspace-write path only.
	// When true, Driver.Build writes the workspace tree, computes the
	// cache key, but does not invoke `go build`. Used in unit tests
	// that don't want to pay the cgo toolchain cost.
	SkipBuild bool
}

// Target is a (GOOS, GOARCH) tuple plus optional GOARM, used to
// drive cross-compilation. Empty fields fall back to the host.
type Target struct {
	GOOS   string
	GOARCH string
	GOARM  string
}

// String renders the target as a `GOOS/GOARCH` token. Used in cache
// keys and diagnostics.
func (t Target) String() string {
	goos := t.GOOS
	if goos == "" {
		goos = runtime.GOOS
	}
	goarch := t.GOARCH
	if goarch == "" {
		goarch = runtime.GOARCH
	}
	s := goos + "/" + goarch
	if t.GOARM != "" {
		s += "/v" + t.GOARM
	}
	return s
}

// BuildResult captures the artefacts produced by Driver.Build.
type BuildResult struct {
	// Artefacts is one BuildArtefact per wrapper module, indexed by
	// the wrapper's ModuleName. Each entry records the on-disk path
	// to the `.a` static archive and the matching `_cgo_export.h`
	// header.
	Artefacts map[string]BuildArtefact
	// WorkspaceRoot is the directory the workspace go.work was written
	// to. The path is preserved across the call so callers can wire
	// it into a higher-level build cache.
	WorkspaceRoot string
	// CacheKey is the content-addressed key the build was performed
	// (or cache-hit) against. Stable across runs.
	CacheKey string
	// CacheHit reports whether the cache lookup found a pre-built
	// archive set and the `go build` invocation was skipped.
	CacheHit bool
}

// BuildArtefact captures the output of one `go build -buildmode=c-archive`
// invocation: the path to the static archive and the matching cgo
// header file. Both paths live inside the driver's work-dir.
type BuildArtefact struct {
	// ModuleName is the wrapper.Result.ModuleName the artefact came from.
	ModuleName string
	// ArchivePath is the absolute path to the `<module>.a` file.
	ArchivePath string
	// HeaderPath is the absolute path to the `<module>.h` cgo header.
	HeaderPath string
}

// Build executes the phase 9 pipeline end-to-end. The Driver must
// have been initialised but does not need PrepareWorkspace called
// in advance, Build does that itself.
func (d *Driver) Build(plan BuildPlan) (*BuildResult, error) {
	if len(plan.Wrappers) == 0 {
		return nil, fmt.Errorf("build: plan has no wrappers")
	}
	for _, w := range plan.Wrappers {
		if w.ModuleName == "" {
			return nil, fmt.Errorf("build: wrapper has empty ModuleName")
		}
		if len(w.Files) == 0 {
			return nil, fmt.Errorf("build: wrapper %q has no files", w.ModuleName)
		}
	}

	ws, err := d.PrepareWorkspace()
	if err != nil {
		return nil, err
	}

	for _, w := range plan.Wrappers {
		ws.AddModule(WorkspaceModule{
			ImportPath: synthesisedImportPath(w.ModuleName),
			Path:       filepath.Join("go_wrap", w.ModuleName),
			Kind:       ModuleWrapper,
		})
	}

	root, err := d.WriteWorkspaceRoot(ws)
	if err != nil {
		return nil, err
	}

	for _, w := range plan.Wrappers {
		modDir := filepath.Join(root, "go_wrap", w.ModuleName)
		if err := writeWrapperModule(modDir, w); err != nil {
			return nil, err
		}
	}

	cacheKey := computeCacheKey(plan, ws)
	cachePath := d.cacheArtefactDir(cacheKey)

	if hit, err := d.tryCacheHit(plan, cachePath, root); err != nil {
		return nil, err
	} else if hit != nil {
		hit.CacheKey = cacheKey
		hit.WorkspaceRoot = root
		hit.CacheHit = true
		return hit, nil
	}

	artefacts := map[string]BuildArtefact{}
	if !plan.SkipBuild {
		for _, w := range plan.Wrappers {
			a, err := d.buildOne(plan, root, w)
			if err != nil {
				return nil, err
			}
			artefacts[w.ModuleName] = a
		}
		if err := d.populateCache(cachePath, artefacts); err != nil {
			return nil, fmt.Errorf("build: populate cache: %w", err)
		}
	} else {
		for _, w := range plan.Wrappers {
			modDir := filepath.Join(root, "go_wrap", w.ModuleName)
			artefacts[w.ModuleName] = BuildArtefact{
				ModuleName:  w.ModuleName,
				ArchivePath: filepath.Join(modDir, w.ModuleName+".a"),
				HeaderPath:  filepath.Join(modDir, w.ModuleName+".h"),
			}
		}
	}

	return &BuildResult{
		Artefacts:     artefacts,
		WorkspaceRoot: root,
		CacheKey:      cacheKey,
		CacheHit:      false,
	}, nil
}

// writeWrapperModule writes the wrapper.Result's Files into modDir
// and synthesises the matching go.mod. The wrapper's Files are written
// verbatim (the emitter already produced cgo-ready source); the go.mod
// is generated here because phase 6 doesn't own the module-path
// decision.
func writeWrapperModule(modDir string, w wrapper.Result) error {
	if err := os.MkdirAll(modDir, 0o755); err != nil {
		return fmt.Errorf("build: mkdir %s: %w", modDir, err)
	}
	for name, content := range w.Files {
		// Refuse path components that escape modDir.
		clean := filepath.Clean(name)
		if strings.HasPrefix(clean, "..") || filepath.IsAbs(clean) {
			return fmt.Errorf("build: wrapper file %q escapes module dir", name)
		}
		dest := filepath.Join(modDir, clean)
		if err := os.MkdirAll(filepath.Dir(dest), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(dest, []byte(content), 0o644); err != nil {
			return fmt.Errorf("build: write %s: %w", dest, err)
		}
	}
	gomod := renderWrapperGoMod(w.ModuleName)
	if err := os.WriteFile(filepath.Join(modDir, "go.mod"), []byte(gomod), 0o644); err != nil {
		return fmt.Errorf("build: write go.mod: %w", err)
	}
	return nil
}

// renderWrapperGoMod produces the synthesised go.mod for a wrapper
// module. The Go-version floor is held at 1.21 to match the workspace
// default; phase 10's lockfile may bump it per-module from the source
// module's go.mod.
func renderWrapperGoMod(moduleName string) string {
	return fmt.Sprintf("// Auto-generated by MEP-74 bridge. Do not edit by hand.\nmodule %s\n\ngo 1.21\n",
		synthesisedImportPath(moduleName))
}

// synthesisedImportPath maps a wrapper.Result.ModuleName (already
// flattened, e.g. "mochi_go_github_com_spf13_cobra") to the import
// path used inside the workspace. The bridge-local prefix
// `mochilang.local/` keeps the synthesised modules clearly distinct
// from any user module that happens to share a name.
func synthesisedImportPath(moduleName string) string {
	return "mochilang.local/" + moduleName
}

// buildOne runs `go build -buildmode=c-archive` over a single wrapper
// module and returns the resulting artefact pair.
func (d *Driver) buildOne(plan BuildPlan, root string, w wrapper.Result) (BuildArtefact, error) {
	modDir := filepath.Join(root, "go_wrap", w.ModuleName)
	archive := filepath.Join(modDir, w.ModuleName+".a")
	header := filepath.Join(modDir, w.ModuleName+".h")

	args := []string{
		"build",
		"-buildmode=c-archive",
		"-trimpath",
		"-buildvcs=false",
	}
	if d.opts.Verbose {
		args = append(args, "-v")
	}
	ldflags := "-s -w"
	args = append(args, "-ldflags="+ldflags)
	args = append(args, "-o", archive)
	args = append(args, ".")

	cmd := exec.Command("go", args...)
	cmd.Dir = modDir
	cmd.Env = d.buildEnv(plan)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return BuildArtefact{}, fmt.Errorf("build: go build %s: %w\n%s", w.ModuleName, err, out)
	}

	if _, err := os.Stat(archive); err != nil {
		return BuildArtefact{}, fmt.Errorf("build: %s not produced: %w", archive, err)
	}
	if _, err := os.Stat(header); err != nil {
		// `go build -buildmode=c-archive` may emit a `<basename>.h`
		// with the trailing .a swapped for .h. Compute the alternate.
		alt := strings.TrimSuffix(archive, ".a") + ".h"
		if _, e2 := os.Stat(alt); e2 == nil {
			header = alt
		} else {
			return BuildArtefact{}, fmt.Errorf("build: header %s not produced: %w", header, err)
		}
	}

	return BuildArtefact{
		ModuleName:  w.ModuleName,
		ArchivePath: archive,
		HeaderPath:  header,
	}, nil
}

// buildEnv assembles the env passed to `go build`. The bridge sets
// GOOS / GOARCH / GOARM from the plan target and forwards CGO_ENABLED
// from plan.CgoEnabled. Caller-supplied ExtraEnv is appended last so
// it can override anything earlier.
func (d *Driver) buildEnv(plan BuildPlan) []string {
	env := os.Environ()
	if plan.Target.GOOS != "" {
		env = setEnv(env, "GOOS", plan.Target.GOOS)
	}
	if plan.Target.GOARCH != "" {
		env = setEnv(env, "GOARCH", plan.Target.GOARCH)
	}
	if plan.Target.GOARM != "" {
		env = setEnv(env, "GOARM", plan.Target.GOARM)
	}
	cgo := "1"
	if !plan.CgoEnabled {
		cgo = "0"
	}
	env = setEnv(env, "CGO_ENABLED", cgo)
	if d.opts.Deterministic {
		env = setEnv(env, "GOFLAGS", "-trimpath -buildvcs=false")
		env = setEnv(env, "SOURCE_DATE_EPOCH", "0")
	}
	env = append(env, plan.ExtraEnv...)
	return env
}

// setEnv replaces (or appends) a key in env. Used by buildEnv to
// override toolchain-relevant variables without mutating the caller's
// os.Environ snapshot.
func setEnv(env []string, key, val string) []string {
	prefix := key + "="
	for i, e := range env {
		if strings.HasPrefix(e, prefix) {
			env[i] = prefix + val
			return env
		}
	}
	return append(env, prefix+val)
}

// computeCacheKey derives a stable hex sha256 key from every wrapper
// file's content, the rendered go.work, and the target tuple. Phase
// 10's mochi.lock will fold this key into the lockfile so a
// repository-level `mochi pkg lock --check` notices wrapper drift.
func computeCacheKey(plan BuildPlan, ws *Workspace) string {
	h := sha256.New()
	for _, w := range sortedWrappers(plan.Wrappers) {
		fmt.Fprintf(h, "module:%s\n", w.ModuleName)
		names := make([]string, 0, len(w.Files))
		for n := range w.Files {
			names = append(names, n)
		}
		sort.Strings(names)
		for _, n := range names {
			fmt.Fprintf(h, "file:%s:%d\n", n, len(w.Files[n]))
			h.Write([]byte(w.Files[n]))
			h.Write([]byte("\x00"))
		}
	}
	fmt.Fprintf(h, "workspace:%d\n", len(ws.RenderGoWork()))
	h.Write([]byte(ws.RenderGoWork()))
	fmt.Fprintf(h, "target:%s\n", plan.Target.String())
	fmt.Fprintf(h, "cgo:%v\n", plan.CgoEnabled)
	return hex.EncodeToString(h.Sum(nil))
}

// sortedWrappers returns plan.Wrappers in deterministic order so the
// cache key is stable across slice permutations.
func sortedWrappers(in []wrapper.Result) []wrapper.Result {
	out := make([]wrapper.Result, len(in))
	copy(out, in)
	sort.Slice(out, func(i, j int) bool { return out[i].ModuleName < out[j].ModuleName })
	return out
}

// cacheArtefactDir returns the cache directory that holds the
// artefact set for a given key. NoCache callers will get an empty
// string back; tryCacheHit treats that as a cache miss.
func (d *Driver) cacheArtefactDir(key string) string {
	if d.opts.NoCache || d.opts.CacheDir == "" || key == "" {
		return ""
	}
	if len(key) < 4 {
		return filepath.Join(d.opts.CacheDir, "artefacts", key)
	}
	return filepath.Join(d.opts.CacheDir, "artefacts", key[:2], key)
}

// tryCacheHit looks for a pre-existing artefact bundle at cachePath
// and, if present, copies it back into the work-dir. Returns a non-
// nil BuildResult on a hit, a nil result on a miss (with err == nil).
func (d *Driver) tryCacheHit(plan BuildPlan, cachePath, root string) (*BuildResult, error) {
	if cachePath == "" {
		return nil, nil
	}
	manifest := filepath.Join(cachePath, "manifest.txt")
	if _, err := os.Stat(manifest); err != nil {
		if errors.Is(err, os.ErrNotExist) {
			return nil, nil
		}
		return nil, fmt.Errorf("build: cache stat: %w", err)
	}
	artefacts := map[string]BuildArtefact{}
	for _, w := range plan.Wrappers {
		srcArchive := filepath.Join(cachePath, w.ModuleName+".a")
		srcHeader := filepath.Join(cachePath, w.ModuleName+".h")
		if _, err := os.Stat(srcArchive); err != nil {
			return nil, nil
		}
		if _, err := os.Stat(srcHeader); err != nil {
			return nil, nil
		}
		modDir := filepath.Join(root, "go_wrap", w.ModuleName)
		if err := os.MkdirAll(modDir, 0o755); err != nil {
			return nil, err
		}
		dstArchive := filepath.Join(modDir, w.ModuleName+".a")
		dstHeader := filepath.Join(modDir, w.ModuleName+".h")
		if err := copyFile(srcArchive, dstArchive); err != nil {
			return nil, fmt.Errorf("build: copy archive from cache: %w", err)
		}
		if err := copyFile(srcHeader, dstHeader); err != nil {
			return nil, fmt.Errorf("build: copy header from cache: %w", err)
		}
		artefacts[w.ModuleName] = BuildArtefact{
			ModuleName:  w.ModuleName,
			ArchivePath: dstArchive,
			HeaderPath:  dstHeader,
		}
	}
	return &BuildResult{Artefacts: artefacts}, nil
}

// populateCache copies a freshly built artefact set into the
// content-addressed cache so a future invocation with the same key
// can short-circuit `go build`.
func (d *Driver) populateCache(cachePath string, artefacts map[string]BuildArtefact) error {
	if cachePath == "" {
		return nil
	}
	if err := os.MkdirAll(cachePath, 0o755); err != nil {
		return fmt.Errorf("build: mkdir cache %s: %w", cachePath, err)
	}
	names := make([]string, 0, len(artefacts))
	for n := range artefacts {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		a := artefacts[n]
		if err := copyFile(a.ArchivePath, filepath.Join(cachePath, n+".a")); err != nil {
			return err
		}
		if err := copyFile(a.HeaderPath, filepath.Join(cachePath, n+".h")); err != nil {
			return err
		}
	}
	manifest := strings.Join(names, "\n") + "\n"
	if err := os.WriteFile(filepath.Join(cachePath, "manifest.txt"), []byte(manifest), 0o644); err != nil {
		return err
	}
	return nil
}

// copyFile copies src to dst with 0o644 permissions. Used by the
// cache populate / hydrate paths.
func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	out, err := os.OpenFile(dst, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0o644)
	if err != nil {
		return err
	}
	if _, err := io.Copy(out, in); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}
