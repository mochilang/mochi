package build

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase15Composer is the gate for Phase 15: the PHP runtime ships
// as a Composer-installable package and a representative cross-feature
// fixture set lowers cleanly through the transpiler. Three sub-gates:
//
//  1. composer.json parses as valid JSON and declares the expected
//     package name and PHP version constraint. Always runs.
//  2. Each fixture lowers to PHP source without errors. Always runs.
//  3. When PHP is on PATH, each fixture also runs and stdout matches
//     the recorded .out file. Skipped without PHP.
//
// The fixtures cover the language features that are most prone to
// breakage when packaging changes touch the runtime layout: agents,
// records, loops, lists, scalars, strings. A green Phase 15 means a
// fresh `composer install` against the runtime package will still
// let any of these fixtures run.
func TestPhase15Composer(t *testing.T) {
	t.Run("composer_json_valid", func(t *testing.T) {
		path := filepath.Join(repoRoot(t), "transpiler3", "php", "runtime", "composer.json")
		data, err := os.ReadFile(path)
		if err != nil {
			t.Fatalf("read composer.json: %v", err)
		}
		var pkg struct {
			Name    string                 `json:"name"`
			Type    string                 `json:"type"`
			License string                 `json:"license"`
			Require map[string]string      `json:"require"`
			Auto    map[string]interface{} `json:"autoload"`
		}
		if err := json.Unmarshal(data, &pkg); err != nil {
			t.Fatalf("composer.json not valid JSON: %v", err)
		}
		if pkg.Name != "mochi/runtime" {
			t.Errorf(`composer.json name = %q, want "mochi/runtime"`, pkg.Name)
		}
		if pkg.Type != "library" {
			t.Errorf(`composer.json type = %q, want "library"`, pkg.Type)
		}
		if v, ok := pkg.Require["php"]; !ok || !strings.HasPrefix(v, "^8") {
			t.Errorf(`composer.json php constraint = %q, want ^8.x`, v)
		}
		if _, ok := pkg.Auto["psr-4"]; !ok {
			t.Error("composer.json missing autoload.psr-4 mapping")
		}
	})

	t.Run("runtime_layout", func(t *testing.T) {
		root := filepath.Join(repoRoot(t), "transpiler3", "php", "runtime")
		for _, want := range []string{"composer.json", "src", "phpstan.neon", "psalm.xml"} {
			if _, err := os.Stat(filepath.Join(root, want)); err != nil {
				t.Errorf("runtime layout missing %s: %v", want, err)
			}
		}
	})

	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase15-composer")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPhpFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

// TestPhase15LowersAllFeatures asserts that the cross-feature fixture
// set produces non-empty PHP source for each input. The fragment
// assertions in phases 1-14 already verify the per-feature shape;
// this gate is a structural cross-cut over the broader feature mix.
func TestPhase15LowersAllFeatures(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase15-composer")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			mochiPath := filepath.Join(fixtureDir, e.Name())
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", name, err)
			}
			data, err := os.ReadFile(p)
			if err != nil {
				t.Fatalf("read %s: %v", p, err)
			}
			src := string(data)
			if !strings.HasPrefix(src, "<?php") {
				t.Errorf("%s: missing <?php opener", name)
			}
			if !strings.Contains(src, "declare(strict_types=1);") {
				t.Errorf("%s: missing declare(strict_types=1)", name)
			}
			if !strings.Contains(src, "mochi_main();") {
				t.Errorf("%s: missing mochi_main() invocation", name)
			}
		})
	}
}
