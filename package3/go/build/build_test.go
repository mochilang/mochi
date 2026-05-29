package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/go/wrapper"
)

func TestBuildRejectsEmptyPlan(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	if _, err := d.Build(BuildPlan{}); err == nil {
		t.Errorf("Build(empty plan) returned nil error; expected one")
	}
}

func TestBuildRejectsWrapperWithoutFiles(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{{
			ModuleName: "mochi_go_x",
		}},
		SkipBuild: true,
	}
	if _, err := d.Build(plan); err == nil {
		t.Errorf("Build(wrapper with no files) returned nil error; expected one")
	}
}

func TestBuildRejectsWrapperWithEmptyName(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{{Files: map[string]string{"x.go": "package x"}}},
		SkipBuild: true,
	}
	if _, err := d.Build(plan); err == nil {
		t.Errorf("Build(wrapper with empty ModuleName) returned nil; expected error")
	}
}

func TestBuildWritesWrapperFilesAndGoMod(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache"), WorkDir: filepath.Join(tmp, "work")})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{{
			ModuleName: "mochi_go_example_com_foo",
			Files: map[string]string{
				"wrap.go": "package foo\n",
				"sub/help.go": "package foo\n",
			},
		}},
		SkipBuild: true,
	}
	res, err := d.Build(plan)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	modDir := filepath.Join(res.WorkspaceRoot, "go_wrap", "mochi_go_example_com_foo")
	if _, err := os.Stat(filepath.Join(modDir, "wrap.go")); err != nil {
		t.Errorf("wrap.go not written: %v", err)
	}
	if _, err := os.Stat(filepath.Join(modDir, "sub", "help.go")); err != nil {
		t.Errorf("sub/help.go not written: %v", err)
	}
	gomod, err := os.ReadFile(filepath.Join(modDir, "go.mod"))
	if err != nil {
		t.Fatalf("read go.mod: %v", err)
	}
	if !strings.Contains(string(gomod), "module mochilang.local/mochi_go_example_com_foo") {
		t.Errorf("go.mod wrong module line:\n%s", gomod)
	}
	if !strings.Contains(string(gomod), "go 1.21") {
		t.Errorf("go.mod missing go-version floor:\n%s", gomod)
	}
}

func TestBuildAssemblesWorkspaceGoWork(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{CacheDir: filepath.Join(tmp, "cache"), WorkDir: filepath.Join(tmp, "work")})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{
			{ModuleName: "mochi_go_a", Files: map[string]string{"a.go": "package a"}},
			{ModuleName: "mochi_go_b", Files: map[string]string{"b.go": "package b"}},
		},
		SkipBuild: true,
	}
	res, err := d.Build(plan)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	gowork, err := os.ReadFile(filepath.Join(res.WorkspaceRoot, "go.work"))
	if err != nil {
		t.Fatalf("read go.work: %v", err)
	}
	s := string(gowork)
	if !strings.Contains(s, "./go_wrap/mochi_go_a") {
		t.Errorf("go.work missing wrapper a use directive:\n%s", s)
	}
	if !strings.Contains(s, "./go_wrap/mochi_go_b") {
		t.Errorf("go.work missing wrapper b use directive:\n%s", s)
	}
}

func TestBuildRejectsEscapingFileName(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{{
			ModuleName: "mochi_go_x",
			Files:      map[string]string{"../escape.go": "package x"},
		}},
		SkipBuild: true,
	}
	if _, err := d.Build(plan); err == nil {
		t.Errorf("Build accepted escaping file path; expected error")
	}
}

func TestBuildRejectsAbsoluteFileName(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers: []wrapper.Result{{
			ModuleName: "mochi_go_x",
			Files:      map[string]string{"/etc/passwd": "package x"},
		}},
		SkipBuild: true,
	}
	if _, err := d.Build(plan); err == nil {
		t.Errorf("Build accepted absolute file path; expected error")
	}
}

func TestComputeCacheKeyDeterministic(t *testing.T) {
	plan := BuildPlan{
		Wrappers: []wrapper.Result{
			{ModuleName: "z", Files: map[string]string{"z.go": "package z"}},
			{ModuleName: "a", Files: map[string]string{"a.go": "package a"}},
		},
		Target:     Target{GOOS: "darwin", GOARCH: "arm64"},
		CgoEnabled: true,
	}
	ws := DefaultWorkspace()
	ws.AddModule(WorkspaceModule{ImportPath: "mochilang.local/a", Path: "go_wrap/a", Kind: ModuleWrapper})
	ws.AddModule(WorkspaceModule{ImportPath: "mochilang.local/z", Path: "go_wrap/z", Kind: ModuleWrapper})
	first := computeCacheKey(plan, ws)
	for i := 0; i < 10; i++ {
		got := computeCacheKey(plan, ws)
		if got != first {
			t.Fatalf("cache key non-deterministic on iter %d: %s vs %s", i, first, got)
		}
	}
}

func TestComputeCacheKeyChangesOnFileEdit(t *testing.T) {
	ws := DefaultWorkspace()
	ws.AddModule(WorkspaceModule{ImportPath: "mochilang.local/a", Path: "go_wrap/a", Kind: ModuleWrapper})

	plan1 := BuildPlan{Wrappers: []wrapper.Result{{ModuleName: "a", Files: map[string]string{"a.go": "package a"}}}}
	plan2 := BuildPlan{Wrappers: []wrapper.Result{{ModuleName: "a", Files: map[string]string{"a.go": "package a // edited"}}}}
	if computeCacheKey(plan1, ws) == computeCacheKey(plan2, ws) {
		t.Errorf("cache key did not change when wrapper file content changed")
	}
}

func TestComputeCacheKeyChangesOnTarget(t *testing.T) {
	ws := DefaultWorkspace()
	w := wrapper.Result{ModuleName: "a", Files: map[string]string{"a.go": "package a"}}

	plan1 := BuildPlan{Wrappers: []wrapper.Result{w}, Target: Target{GOOS: "darwin", GOARCH: "arm64"}}
	plan2 := BuildPlan{Wrappers: []wrapper.Result{w}, Target: Target{GOOS: "linux", GOARCH: "amd64"}}
	if computeCacheKey(plan1, ws) == computeCacheKey(plan2, ws) {
		t.Errorf("cache key did not change when target changed")
	}
}

func TestComputeCacheKeyStableAcrossSlicePermutation(t *testing.T) {
	ws := DefaultWorkspace()
	ws.AddModule(WorkspaceModule{ImportPath: "mochilang.local/a", Path: "go_wrap/a", Kind: ModuleWrapper})
	ws.AddModule(WorkspaceModule{ImportPath: "mochilang.local/z", Path: "go_wrap/z", Kind: ModuleWrapper})
	a := wrapper.Result{ModuleName: "a", Files: map[string]string{"a.go": "package a"}}
	z := wrapper.Result{ModuleName: "z", Files: map[string]string{"z.go": "package z"}}
	planAZ := BuildPlan{Wrappers: []wrapper.Result{a, z}}
	planZA := BuildPlan{Wrappers: []wrapper.Result{z, a}}
	if got1, got2 := computeCacheKey(planAZ, ws), computeCacheKey(planZA, ws); got1 != got2 {
		t.Errorf("cache key changed under permutation: %s vs %s", got1, got2)
	}
}

func TestTargetStringHostFallback(t *testing.T) {
	tgt := Target{}
	got := tgt.String()
	if !strings.Contains(got, "/") {
		t.Errorf("Target.String fallback should produce GOOS/GOARCH: %q", got)
	}
}

func TestTargetStringExplicit(t *testing.T) {
	tgt := Target{GOOS: "linux", GOARCH: "arm", GOARM: "7"}
	if got := tgt.String(); got != "linux/arm/v7" {
		t.Errorf("Target.String() = %q; want linux/arm/v7", got)
	}
}

func TestSetEnvOverridesExisting(t *testing.T) {
	env := []string{"FOO=1", "BAR=2"}
	env = setEnv(env, "FOO", "9")
	for _, e := range env {
		if e == "FOO=1" {
			t.Errorf("setEnv left old FOO=1: %v", env)
		}
	}
	found := false
	for _, e := range env {
		if e == "FOO=9" {
			found = true
		}
	}
	if !found {
		t.Errorf("setEnv did not write new FOO=9: %v", env)
	}
}

func TestSetEnvAppendsMissing(t *testing.T) {
	env := []string{"BAR=2"}
	env = setEnv(env, "FOO", "1")
	if len(env) != 2 {
		t.Errorf("setEnv did not append: %v", env)
	}
}

func TestCacheArtefactDirNoCache(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if got := d.cacheArtefactDir("abcd"); got != "" {
		t.Errorf("cacheArtefactDir under NoCache = %q; want empty", got)
	}
}

func TestCacheArtefactDirShardsByPrefix(t *testing.T) {
	d := NewDriver(Options{CacheDir: "/tmp/cache"})
	key := "abcdef1234"
	got := d.cacheArtefactDir(key)
	want := filepath.Join("/tmp/cache", "artefacts", "ab", key)
	if got != want {
		t.Errorf("cacheArtefactDir = %q; want %q", got, want)
	}
}

func TestBuildCacheHitShortCircuits(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{
		CacheDir: filepath.Join(tmp, "cache"),
		WorkDir:  filepath.Join(tmp, "work"),
	})
	defer d.Cleanup()

	plan := BuildPlan{
		Wrappers: []wrapper.Result{{
			ModuleName: "mochi_go_x",
			Files:      map[string]string{"x.go": "package x\n"},
		}},
		SkipBuild: true,
	}

	// Pre-seed cache with stub artefacts at the expected key.
	ws := DefaultWorkspace()
	ws.AddModule(WorkspaceModule{
		ImportPath: synthesisedImportPath("mochi_go_x"),
		Path:       "go_wrap/mochi_go_x",
		Kind:       ModuleWrapper,
	})
	key := computeCacheKey(plan, ws)
	cachePath := d.cacheArtefactDir(key)
	if err := os.MkdirAll(cachePath, 0o755); err != nil {
		t.Fatal(err)
	}
	for _, name := range []string{"mochi_go_x.a", "mochi_go_x.h"} {
		if err := os.WriteFile(filepath.Join(cachePath, name), []byte("stub"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(filepath.Join(cachePath, "manifest.txt"), []byte("mochi_go_x\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	res, err := d.Build(plan)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if !res.CacheHit {
		t.Errorf("expected CacheHit=true; got false (key=%s)", key)
	}
	if res.CacheKey != key {
		t.Errorf("CacheKey = %q; want %q", res.CacheKey, key)
	}
	art, ok := res.Artefacts["mochi_go_x"]
	if !ok {
		t.Fatalf("artefacts missing mochi_go_x: %#v", res.Artefacts)
	}
	body, err := os.ReadFile(art.ArchivePath)
	if err != nil {
		t.Fatalf("read archive: %v", err)
	}
	if string(body) != "stub" {
		t.Errorf("archive body = %q; want stub (cache hit should have copied bytes)", body)
	}
}

func TestBuildCacheMissPopulatesOnSkipBuild(t *testing.T) {
	tmp := t.TempDir()
	d := NewDriver(Options{
		CacheDir: filepath.Join(tmp, "cache"),
		WorkDir:  filepath.Join(tmp, "work"),
	})
	defer d.Cleanup()
	plan := BuildPlan{
		Wrappers:  []wrapper.Result{{ModuleName: "mochi_go_x", Files: map[string]string{"x.go": "package x\n"}}},
		SkipBuild: true,
	}
	res, err := d.Build(plan)
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	if res.CacheHit {
		t.Errorf("expected CacheHit=false on fresh cache")
	}
	if res.CacheKey == "" {
		t.Errorf("CacheKey should be populated even on miss")
	}
	// SkipBuild=true does not populate the cache because there are
	// no real artefacts to copy. Verify the manifest is absent.
	cachePath := d.cacheArtefactDir(res.CacheKey)
	if _, err := os.Stat(filepath.Join(cachePath, "manifest.txt")); err == nil {
		t.Errorf("SkipBuild=true should not populate cache; manifest exists")
	}
}

func TestCopyFileWritesContents(t *testing.T) {
	tmp := t.TempDir()
	src := filepath.Join(tmp, "src")
	dst := filepath.Join(tmp, "subdir", "dst")
	if err := os.WriteFile(src, []byte("payload"), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := copyFile(src, dst); err != nil {
		t.Fatalf("copyFile: %v", err)
	}
	body, err := os.ReadFile(dst)
	if err != nil {
		t.Fatalf("read dst: %v", err)
	}
	if string(body) != "payload" {
		t.Errorf("dst body = %q; want payload", body)
	}
}

func TestBuildEnvCarriesTargetTuple(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	plan := BuildPlan{
		Target:     Target{GOOS: "linux", GOARCH: "arm64"},
		CgoEnabled: true,
	}
	env := d.buildEnv(plan)
	want := map[string]string{
		"GOOS=linux":   "GOOS=linux",
		"GOARCH=arm64": "GOARCH=arm64",
		"CGO_ENABLED=1": "CGO_ENABLED=1",
	}
	for k := range want {
		found := false
		for _, e := range env {
			if e == k {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("buildEnv missing %q", k)
		}
	}
}

func TestBuildEnvCgoDisabled(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	env := d.buildEnv(BuildPlan{CgoEnabled: false})
	found := false
	for _, e := range env {
		if e == "CGO_ENABLED=0" {
			found = true
		}
	}
	if !found {
		t.Errorf("buildEnv with CgoEnabled=false missing CGO_ENABLED=0: %v", env)
	}
}

func TestBuildEnvDeterministicAddsFlags(t *testing.T) {
	d := NewDriver(Options{NoCache: true, Deterministic: true})
	env := d.buildEnv(BuildPlan{CgoEnabled: true})
	hasFlags := false
	hasEpoch := false
	for _, e := range env {
		if strings.HasPrefix(e, "GOFLAGS=") && strings.Contains(e, "-trimpath") {
			hasFlags = true
		}
		if e == "SOURCE_DATE_EPOCH=0" {
			hasEpoch = true
		}
	}
	if !hasFlags {
		t.Errorf("Deterministic driver should set GOFLAGS=-trimpath...:\n%v", env)
	}
	if !hasEpoch {
		t.Errorf("Deterministic driver should set SOURCE_DATE_EPOCH=0:\n%v", env)
	}
}

func TestBuildEnvExtraEnvOverrides(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	env := d.buildEnv(BuildPlan{ExtraEnv: []string{"CGO_ENABLED=42"}})
	// ExtraEnv is appended last so the latest entry wins on lookup;
	// the bridge does not de-dupe before passing to exec.Cmd.
	last := ""
	for _, e := range env {
		if strings.HasPrefix(e, "CGO_ENABLED=") {
			last = e
		}
	}
	if last != "CGO_ENABLED=42" {
		t.Errorf("ExtraEnv override not at the end: last CGO_ENABLED = %q", last)
	}
}

func TestSynthesisedImportPath(t *testing.T) {
	if got, want := synthesisedImportPath("mochi_go_cobra"), "mochilang.local/mochi_go_cobra"; got != want {
		t.Errorf("synthesisedImportPath = %q; want %q", got, want)
	}
}

func TestRenderWrapperGoMod(t *testing.T) {
	got := renderWrapperGoMod("mochi_go_x")
	for _, want := range []string{
		"module mochilang.local/mochi_go_x",
		"go 1.21",
		"Auto-generated by MEP-74",
	} {
		if !strings.Contains(got, want) {
			t.Errorf("renderWrapperGoMod missing %q\n%s", want, got)
		}
	}
}

func TestSortedWrappersStable(t *testing.T) {
	in := []wrapper.Result{
		{ModuleName: "z"},
		{ModuleName: "a"},
		{ModuleName: "m"},
	}
	got := sortedWrappers(in)
	want := []string{"a", "m", "z"}
	for i, w := range got {
		if w.ModuleName != want[i] {
			t.Errorf("sortedWrappers[%d] = %s; want %s", i, w.ModuleName, want[i])
		}
	}
	// Verify caller's slice is not mutated.
	if in[0].ModuleName != "z" {
		t.Errorf("sortedWrappers mutated input slice: %v", in)
	}
}
