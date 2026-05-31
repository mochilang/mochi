package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase17NpmPackage verifies that BuildNpmPackage emits the expected
// file structure: src/index.ts, package.json, tsconfig.json, .npmignore.
func TestPhase17NpmPackage(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase17-package")
	fixtures := []string{"hello_module.mochi", "simple_greet.mochi", "sum_module.mochi"}
	for _, fix := range fixtures {
		fix := fix
		t.Run(strings.TrimSuffix(fix, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			spec := NpmPackageSpec{Name: "test-pkg", Version: "1.0.0"}
			pkgDir, err := d.BuildNpmPackage(filepath.Join(fixtureDir, fix), outDir, spec)
			if err != nil {
				t.Fatalf("BuildNpmPackage(%s): %v", fix, err)
			}
			mustExist(t, filepath.Join(pkgDir, "src", "index.ts"))
			mustExist(t, filepath.Join(pkgDir, "package.json"))
			mustExist(t, filepath.Join(pkgDir, "tsconfig.json"))
			mustExist(t, filepath.Join(pkgDir, ".npmignore"))
			pkgJSON := readTrim(t, filepath.Join(pkgDir, "package.json"))
			if !strings.Contains(pkgJSON, `"test-pkg"`) {
				t.Errorf("package.json missing package name: %s", pkgJSON)
			}
			if !strings.Contains(pkgJSON, `"1.0.0"`) {
				t.Errorf("package.json missing version: %s", pkgJSON)
			}
		})
	}
}

// TestPhase17DenoModule verifies that BuildDenoModule emits mod.ts and deno.json.
func TestPhase17DenoModule(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase17-package")
	fixtures := []string{"hello_module.mochi", "counter_module.mochi"}
	for _, fix := range fixtures {
		fix := fix
		t.Run(strings.TrimSuffix(fix, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			spec := NpmPackageSpec{Name: "@mochi/test", Version: "0.2.0"}
			pkgDir, err := d.BuildDenoModule(filepath.Join(fixtureDir, fix), outDir, spec)
			if err != nil {
				t.Fatalf("BuildDenoModule(%s): %v", fix, err)
			}
			mustExist(t, filepath.Join(pkgDir, "mod.ts"))
			mustExist(t, filepath.Join(pkgDir, "deno.json"))
			denoJSON := readTrim(t, filepath.Join(pkgDir, "deno.json"))
			if !strings.Contains(denoJSON, `"@mochi/test"`) {
				t.Errorf("deno.json missing package name: %s", denoJSON)
			}
		})
	}
}

// TestPhase17CIWorkflow verifies that EmitCIWorkflow writes the expected
// workflow file and includes provenance / id-token for npm; deno publish for JSR.
func TestPhase17CIWorkflow(t *testing.T) {
	t.Run("npm", func(t *testing.T) {
		outDir := t.TempDir()
		d := &Driver{CacheDir: t.TempDir(), NoCache: true}
		if err := d.EmitCIWorkflow(outDir, false); err != nil {
			t.Fatalf("EmitCIWorkflow npm: %v", err)
		}
		wfPath := filepath.Join(outDir, ".github", "workflows", "npm-publish.yml")
		mustExist(t, wfPath)
		content := readTrim(t, wfPath)
		for _, want := range []string{"npm publish", "provenance", "NPM_TOKEN", "id-token: write"} {
			if !strings.Contains(content, want) {
				t.Errorf("npm workflow missing %q", want)
			}
		}
	})
	t.Run("jsr", func(t *testing.T) {
		outDir := t.TempDir()
		d := &Driver{CacheDir: t.TempDir(), NoCache: true}
		if err := d.EmitCIWorkflow(outDir, true); err != nil {
			t.Fatalf("EmitCIWorkflow jsr: %v", err)
		}
		wfPath := filepath.Join(outDir, ".github", "workflows", "jsr-publish.yml")
		mustExist(t, wfPath)
		content := readTrim(t, wfPath)
		for _, want := range []string{"deno publish", "denoland/setup-deno", "id-token: write"} {
			if !strings.Contains(content, want) {
				t.Errorf("jsr workflow missing %q", want)
			}
		}
	})
}

// TestPhase17FixtureCount verifies the minimum fixture corpus size.
func TestPhase17FixtureCount(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase17-package")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	count := 0
	for _, e := range entries {
		if strings.HasSuffix(e.Name(), ".mochi") {
			count++
		}
	}
	if count < 5 {
		t.Fatalf("Phase 17 fixture corpus has %d .mochi files, expected at least 5", count)
	}
}

func mustExist(t *testing.T, path string) {
	t.Helper()
	if _, err := os.Stat(path); err != nil {
		t.Errorf("expected file to exist: %s: %v", path, err)
	}
}
