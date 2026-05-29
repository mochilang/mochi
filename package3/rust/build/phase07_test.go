package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase07Orchestration is the per-phase sentinel test. The CI
// gate for phase 7 LANDED requires this test to pass on every
// supported host without a rust toolchain.
//
// Sub-tests:
//   - end_to_end: ImportRef --[SurfaceProvider+wrapper+emit]-->
//     on-disk workspace populated with the wrapper crate (Cargo.toml
//     + src/lib.rs + SKIPPED.txt) plus the Mochi-side shim files
//     (<crate>_extern.mochi + <crate>.mochi) plus the workspace root
//     Cargo.toml.
//   - argv_shape: Cargo.ArgsBuild is a pure function of (Cargo,
//     BuildOptions) and produces the canonical `cargo build
//     --profile release --manifest-path <root>/Cargo.toml` argv.
//   - env_shape: Cargo.Env applies the deterministic-build keys on
//     top of the inherited environment with stable lexicographic
//     ordering for cache-key reproducibility.
//   - members_sorted: the resolved Workspace lists members in
//     alphabetical path order regardless of input ref order.
func TestPhase07Orchestration(t *testing.T) {
	t.Run("end_to_end", func(t *testing.T) {
		d := NewDriver(Options{NoCache: true})
		defer d.Cleanup()
		if _, err := d.PrepareWorkspace(); err != nil {
			t.Fatalf("PrepareWorkspace: %v", err)
		}
		p := &Pipeline{Driver: d, Provider: newHexProvider()}
		res, err := p.Resolve([]ImportRef{{Crate: "hex", Version: "0.4.3", Alias: "hex"}})
		if err != nil {
			t.Fatalf("Resolve: %v", err)
		}
		root, err := p.MaterialiseWorkspace(res)
		if err != nil {
			t.Fatalf("MaterialiseWorkspace: %v", err)
		}
		for _, rel := range []string{
			"Cargo.toml",
			".gitignore",
			"rust_wrap/hex/Cargo.toml",
			"rust_wrap/hex/src/lib.rs",
			"rust_wrap/hex/SKIPPED.txt",
			"mochi/hex_extern.mochi",
			"mochi/hex.mochi",
		} {
			if _, err := os.Stat(filepath.Join(root, filepath.FromSlash(rel))); err != nil {
				t.Errorf("missing %s: %v", rel, err)
			}
		}
		// Spot-check the workspace root pins the upstream version.
		root_cargo, err := os.ReadFile(filepath.Join(root, "Cargo.toml"))
		if err != nil {
			t.Fatalf("read root Cargo.toml: %v", err)
		}
		for _, want := range []string{
			`"rust_wrap/hex"`,
			`hex = "=0.4.3"`,
			"[profile.release]",
			`panic = "abort"`,
		} {
			if !strings.Contains(string(root_cargo), want) {
				t.Errorf("root Cargo.toml missing %q\n%s", want, root_cargo)
			}
		}
	})

	t.Run("argv_shape", func(t *testing.T) {
		c := &Cargo{Verbose: true, Offline: true, Locked: true}
		args := c.ArgsBuild(BuildOptions{
			WorkspaceRoot: "/tmp/ws",
			Profile:       "release",
		})
		want := []string{
			"cargo", "build", "--verbose", "--offline", "--locked",
			"--profile", "release",
			"--manifest-path", "/tmp/ws/Cargo.toml",
		}
		if !equalSlices(args, want) {
			t.Errorf("ArgsBuild = %v\n   want %v", args, want)
		}
	})

	t.Run("env_shape", func(t *testing.T) {
		c := &Cargo{
			Deterministic: true,
			CargoHome:     "/cache/cargo",
			ExtraEnv:      map[string]string{"RUSTFLAGS": "-D warnings"},
		}
		env := c.Env([]string{"PATH=/usr/bin", "HOME=/home/u"})
		want := []string{
			"CARGO_HOME=/cache/cargo",
			"CARGO_TERM_COLOR=never",
			"HOME=/home/u",
			"PATH=/usr/bin",
			"RUSTC_BOOTSTRAP=0",
			"RUSTFLAGS=-D warnings",
			"SOURCE_DATE_EPOCH=0",
		}
		if !equalSlices(env, want) {
			t.Errorf("Env = %v\n  want %v", env, want)
		}
	})

	t.Run("members_sorted", func(t *testing.T) {
		d := NewDriver(Options{NoCache: true})
		defer d.Cleanup()
		p := &Pipeline{
			Driver: d,
			Provider: staticProvider{
				"hex@0.4.3":     hexLikeSurface(),
				"anyhow@1.0.86": emptySurface("anyhow", "1.0.86"),
				"serde@1.0.150": emptySurface("serde", "1.0.150"),
			},
		}
		// Refs presented in mixed order; expect the resolved Workspace
		// to list members alphabetically by path.
		res, err := p.Resolve([]ImportRef{
			{Crate: "hex", Version: "0.4.3"},
			{Crate: "anyhow", Version: "1.0.86"},
			{Crate: "serde", Version: "1.0.150"},
		})
		if err != nil {
			t.Fatalf("Resolve: %v", err)
		}
		got := make([]string, len(res.Workspace.Members))
		for i, m := range res.Workspace.Members {
			got[i] = m.Path
		}
		want := []string{"rust_wrap/anyhow", "rust_wrap/hex", "rust_wrap/serde"}
		if !equalSlices(got, want) {
			t.Errorf("Members paths = %v; want %v", got, want)
		}
	})
}
