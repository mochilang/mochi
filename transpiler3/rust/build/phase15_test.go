package build

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase15Publish is the gate for Phase 15 (publish-ready crate).
//
// Step 1 (always runs): build to TargetRustCrate and assert the generated
// Cargo.toml carries `license`, `description`, and `repository` fields that
// crates.io requires for publishing.
//
// Step 2 (opt-in, gated on MOCHI_RUN_PUBLISH_DRYRUN=1): run `cargo publish
// --dry-run --no-verify --allow-dirty` inside the emitted crate to confirm
// cargo accepts the metadata. Off by default because cargo's dry-run still
// requires registry index access.
func TestPhase15Publish(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "rust", "fixtures", "phase15-publish")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		t.Run(name, func(t *testing.T) {
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			outDir := t.TempDir()
			crateDir, err := d.Build(mochiPath, outDir, TargetRustCrate)
			if err != nil {
				t.Fatalf("emit crate: %v", err)
			}
			cargoTomlBytes, err := os.ReadFile(filepath.Join(crateDir, "Cargo.toml"))
			if err != nil {
				t.Fatalf("read Cargo.toml: %v", err)
			}
			toml := string(cargoTomlBytes)
			for _, want := range []string{
				`license = "Apache-2.0"`,
				`description = "Mochi-generated Rust crate."`,
				`repository = "https://github.com/mochilang/mochi"`,
			} {
				if !strings.Contains(toml, want) {
					t.Errorf("Cargo.toml missing %q:\n%s", want, toml)
				}
			}
			if _, err := os.Stat(filepath.Join(crateDir, "src", "main.rs")); err != nil {
				t.Errorf("src/main.rs missing: %v", err)
			}

			if os.Getenv("MOCHI_RUN_PUBLISH_DRYRUN") != "1" {
				t.Log("skipping cargo publish --dry-run (set MOCHI_RUN_PUBLISH_DRYRUN=1 to enable)")
				return
			}
			cargo, err := resolveCargo()
			if err != nil {
				t.Skipf("cargo not available: %v", err)
			}
			cmd := exec.Command(cargo, "publish", "--dry-run", "--no-verify", "--allow-dirty")
			cmd.Dir = crateDir
			cmd.Stdout = os.Stderr
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("cargo publish --dry-run: %v", err)
			}
		})
	}
}
