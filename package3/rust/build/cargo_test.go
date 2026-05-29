package build

import (
	"bytes"
	"context"
	"os"
	"runtime"
	"strings"
	"testing"
)

var osStat = os.Stat

func TestCargoArgsBuildDefaults(t *testing.T) {
	c := &Cargo{}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/tmp/ws"})
	want := []string{"cargo", "build", "--profile", "release", "--manifest-path", "/tmp/ws/Cargo.toml"}
	if !equalSlices(args, want) {
		t.Errorf("ArgsBuild = %v; want %v", args, want)
	}
}

func TestCargoArgsBuildVerboseOffline(t *testing.T) {
	c := &Cargo{Verbose: true, Offline: true}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/tmp/ws", Profile: "dev"})
	want := []string{"cargo", "build", "--verbose", "--offline", "--profile", "dev", "--manifest-path", "/tmp/ws/Cargo.toml"}
	if !equalSlices(args, want) {
		t.Errorf("ArgsBuild = %v; want %v", args, want)
	}
}

func TestCargoArgsBuildFrozenLocked(t *testing.T) {
	c := &Cargo{Frozen: true, Locked: true}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/x", Profile: "release"})
	wantContains := []string{"--frozen", "--locked"}
	for _, w := range wantContains {
		if !containsString(args, w) {
			t.Errorf("ArgsBuild missing %q in %v", w, args)
		}
	}
}

func TestCargoArgsBuildPackage(t *testing.T) {
	c := &Cargo{}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/ws", Package: "mochi_wrap_hex"})
	if !containsString(args, "--package") || !containsString(args, "mochi_wrap_hex") {
		t.Errorf("ArgsBuild missing --package mochi_wrap_hex: %v", args)
	}
}

func TestCargoArgsBuildTarget(t *testing.T) {
	c := &Cargo{}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/ws", Target: "x86_64-unknown-linux-musl"})
	if !containsString(args, "--target") || !containsString(args, "x86_64-unknown-linux-musl") {
		t.Errorf("ArgsBuild missing --target x86_64-unknown-linux-musl: %v", args)
	}
}

func TestCargoArgsBuildJobs(t *testing.T) {
	c := &Cargo{}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/ws", Jobs: 4})
	if !containsString(args, "--jobs") || !containsString(args, "4") {
		t.Errorf("ArgsBuild missing --jobs 4: %v", args)
	}
}

func TestCargoArgsBuildCustomBin(t *testing.T) {
	c := &Cargo{Bin: "/opt/rust/bin/cargo"}
	args := c.ArgsBuild(BuildOptions{WorkspaceRoot: "/ws"})
	if args[0] != "/opt/rust/bin/cargo" {
		t.Errorf("argv[0] = %q; want /opt/rust/bin/cargo", args[0])
	}
}

func TestCargoArgsBuildOmitManifestPathWhenEmpty(t *testing.T) {
	c := &Cargo{}
	args := c.ArgsBuild(BuildOptions{})
	if containsString(args, "--manifest-path") {
		t.Errorf("ArgsBuild emitted --manifest-path with empty WorkspaceRoot: %v", args)
	}
}

func TestCargoEnvDeterministic(t *testing.T) {
	c := &Cargo{Deterministic: true}
	env := c.Env([]string{"PATH=/usr/bin", "HOME=/home/u"})
	want := []string{
		"CARGO_TERM_COLOR=never",
		"HOME=/home/u",
		"PATH=/usr/bin",
		"RUSTC_BOOTSTRAP=0",
		"SOURCE_DATE_EPOCH=0",
	}
	if !equalSlices(env, want) {
		t.Errorf("Env = %v; want %v", env, want)
	}
}

func TestCargoEnvCargoHome(t *testing.T) {
	c := &Cargo{CargoHome: "/cache/cargo"}
	env := c.Env([]string{"PATH=/x"})
	if !containsString(env, "CARGO_HOME=/cache/cargo") {
		t.Errorf("Env missing CARGO_HOME: %v", env)
	}
}

func TestCargoEnvOverridesInherited(t *testing.T) {
	c := &Cargo{ExtraEnv: map[string]string{"PATH": "/override"}}
	env := c.Env([]string{"PATH=/inherited", "OTHER=x"})
	if !containsString(env, "PATH=/override") {
		t.Errorf("ExtraEnv did not override PATH: %v", env)
	}
	if containsString(env, "PATH=/inherited") {
		t.Errorf("inherited PATH survived override: %v", env)
	}
}

func TestCargoEnvSortedDeterministic(t *testing.T) {
	c := &Cargo{ExtraEnv: map[string]string{"Z": "1", "A": "2", "M": "3"}}
	env := c.Env(nil)
	want := []string{"A=2", "M=3", "Z=1"}
	if !equalSlices(env, want) {
		t.Errorf("Env not sorted: %v; want %v", env, want)
	}
}

func TestCargoEnvSkipsMalformed(t *testing.T) {
	c := &Cargo{}
	env := c.Env([]string{"=NOVAR", "BAD"})
	if len(env) != 0 {
		t.Errorf("Env = %v; want empty (malformed entries should be skipped)", env)
	}
}

func TestCargoBuildRejectsEmptyRoot(t *testing.T) {
	c := &Cargo{}
	err := c.Build(context.Background(), BuildOptions{})
	if err == nil || !strings.Contains(err.Error(), "empty WorkspaceRoot") {
		t.Errorf("Build with empty WorkspaceRoot: err = %v; want 'empty WorkspaceRoot'", err)
	}
}

func TestCargoBuildEchoSucceeds(t *testing.T) {
	// Use a portable no-op binary to verify that argv composition,
	// env composition, and exec.CommandContext wiring work end-to-end
	// without depending on cargo or rustc being on PATH.
	if runtime.GOOS == "windows" {
		t.Skip("skipping true test on windows")
	}
	bin := pickBin("true")
	if bin == "" {
		t.Skip("no true binary on PATH")
	}
	out := &bytes.Buffer{}
	c := &Cargo{Bin: bin, Stdout: out, Stderr: out}
	if err := c.Build(context.Background(), BuildOptions{WorkspaceRoot: "/tmp/ws"}); err != nil {
		t.Fatalf("Build via %s: %v", bin, err)
	}
}

func TestCargoBuildPropagatesContextCancellation(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("skipping sleep test on windows")
	}
	bin := pickBin("sleep")
	if bin == "" {
		t.Skip("no sleep binary on PATH")
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	c := &Cargo{Bin: bin}
	err := c.Build(ctx, BuildOptions{WorkspaceRoot: "/tmp/ws"})
	if err == nil {
		t.Errorf("Build with cancelled context did not error")
	}
}

func pickBin(name string) string {
	for _, p := range []string{"/usr/bin/" + name, "/bin/" + name} {
		if _, err := osStat(p); err == nil {
			return p
		}
	}
	return ""
}

func TestLookCargoReturnsString(t *testing.T) {
	// We don't assert presence, only that the function returns
	// something coherent: either a path with no error, or a clear
	// error.
	path, err := LookCargo()
	if err != nil && path != "" {
		t.Errorf("LookCargo: err non-nil but path %q non-empty", path)
	}
	if err == nil && path == "" {
		t.Errorf("LookCargo: nil error but empty path")
	}
}

func TestNewCaptureBufIsResettable(t *testing.T) {
	b := newCaptureBuf()
	if b == nil {
		t.Fatalf("newCaptureBuf returned nil")
	}
	b.WriteString("x")
	if b.Len() != 1 {
		t.Errorf("len = %d; want 1", b.Len())
	}
	b.Reset()
	if b.Len() != 0 {
		t.Errorf("len after reset = %d; want 0", b.Len())
	}
}

func containsString(xs []string, s string) bool {
	for _, x := range xs {
		if x == s {
			return true
		}
	}
	return false
}
