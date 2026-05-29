package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"mochi/package3/go/wrapper"
)

// TestPhase9BuildOrchestration is the MEP-74 phase 9 end-to-end
// sentinel. It exercises the full pipeline against a handwritten
// minimal cgo wrapper (the phase 6 baseline emitter is not yet
// buildable as a standalone module without phase 11's user-module
// scaffolding), proving that:
//
//   - the workspace assembly lays out a buildable go_wrap tree,
//   - `go build -buildmode=c-archive` actually produces a .a + .h
//     pair on the host,
//   - the artefact pair is registered in BuildResult.Artefacts,
//   - a re-run with the same plan hits the cache instead of
//     re-invoking go build (we verify by mutating the cached
//     archive bytes and checking the second build returns the
//     mutated content).
//
// The test is skipped on platforms where cgo c-archive is not
// supported (e.g. js/wasm, ios), and skipped when CGO_ENABLED=0 or
// when the system has no working C toolchain reachable from go.
func TestPhase9BuildOrchestration(t *testing.T) {
	if !cgoArchiveSupported(t) {
		t.Skip("c-archive not supported on this platform / cgo disabled")
	}

	tmp := t.TempDir()
	d := NewDriver(Options{
		CacheDir: filepath.Join(tmp, "cache"),
		WorkDir:  filepath.Join(tmp, "work"),
	})
	defer d.Cleanup()

	plan := BuildPlan{
		Wrappers:   []wrapper.Result{minimalCgoWrapper()},
		CgoEnabled: true,
	}

	res, err := d.Build(plan)
	if err != nil {
		t.Fatalf("Build (first): %v", err)
	}
	if res.CacheHit {
		t.Errorf("first build should miss cache; got hit")
	}
	art, ok := res.Artefacts["mochi_go_phase9"]
	if !ok {
		t.Fatalf("artefact missing for mochi_go_phase9: %#v", res.Artefacts)
	}
	if info, err := os.Stat(art.ArchivePath); err != nil || info.Size() == 0 {
		t.Fatalf("archive missing or empty: path=%s err=%v", art.ArchivePath, err)
	}
	if info, err := os.Stat(art.HeaderPath); err != nil || info.Size() == 0 {
		t.Fatalf("header missing or empty: path=%s err=%v", art.HeaderPath, err)
	}

	// Tamper with the cached archive to prove the second invocation
	// hydrates from cache rather than rebuilding.
	cachePath := d.cacheArtefactDir(res.CacheKey)
	cachedArchive := filepath.Join(cachePath, "mochi_go_phase9.a")
	if err := os.WriteFile(cachedArchive, []byte("MOCHI-CACHE-SENTINEL"), 0o644); err != nil {
		t.Fatalf("tamper cached archive: %v", err)
	}

	// Drop the work-dir so the rebuild starts cleanly.
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	d2 := NewDriver(Options{
		CacheDir: d.CacheDir(),
		WorkDir:  filepath.Join(tmp, "work2"),
	})
	defer d2.Cleanup()

	res2, err := d2.Build(plan)
	if err != nil {
		t.Fatalf("Build (second): %v", err)
	}
	if !res2.CacheHit {
		t.Fatalf("second build should hit cache; got miss (key=%s)", res2.CacheKey)
	}
	if res2.CacheKey != res.CacheKey {
		t.Errorf("CacheKey diverged across runs: %s vs %s", res.CacheKey, res2.CacheKey)
	}
	body, err := os.ReadFile(res2.Artefacts["mochi_go_phase9"].ArchivePath)
	if err != nil {
		t.Fatalf("read hydrated archive: %v", err)
	}
	if string(body) != "MOCHI-CACHE-SENTINEL" {
		t.Errorf("cache-hit archive bytes do not match tampered cache: got %q", body)
	}
}

// minimalCgoWrapper returns a wrapper.Result whose Files contain a
// hand-written, deliberately tiny cgo //export source. It is enough
// to drive `go build -buildmode=c-archive` to success on every cgo-
// capable host and proves the orchestrator works without depending
// on the phase-6 wrapper synthesiser producing buildable output.
func minimalCgoWrapper() wrapper.Result {
	src := `package main

/*
#include <stdint.h>
*/
import "C"

//export Mochi_Phase9_Add
func Mochi_Phase9_Add(a, b C.int64_t) C.int64_t { return a + b }

func main() {}
`
	return wrapper.Result{
		ModuleName: "mochi_go_phase9",
		Files: map[string]string{
			"wrap.go": src,
		},
	}
}

// cgoArchiveSupported reports whether the host can run
// `go build -buildmode=c-archive`. It checks three things:
//   - the platform supports c-archive (darwin / linux / windows /
//     freebsd / etc., but not js/wasm or ios under most builders),
//   - CGO_ENABLED is not explicitly off,
//   - a C compiler is reachable from `go env CC`.
func cgoArchiveSupported(t *testing.T) bool {
	t.Helper()
	switch runtime.GOOS {
	case "js", "ios":
		return false
	}
	if os.Getenv("CGO_ENABLED") == "0" {
		return false
	}
	out, err := exec.Command("go", "env", "CGO_ENABLED").Output()
	if err == nil && strings.TrimSpace(string(out)) == "0" {
		return false
	}
	out, err = exec.Command("go", "env", "CC").Output()
	if err != nil {
		return false
	}
	cc := strings.TrimSpace(string(out))
	if cc == "" {
		return false
	}
	if _, err := exec.LookPath(cc); err != nil {
		return false
	}
	return true
}
