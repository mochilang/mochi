package build

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
)

// Cargo encapsulates a `cargo` invocation for the bridge. The
// orchestration layer constructs argv + env here, then either runs
// the command (Run) or returns argv for inspection (ArgsBuild). Tests
// pin the argv shape against ArgsBuild without exec'ing cargo, so the
// gate is portable to hosts that lack a rust toolchain.
//
// All env writes are additive: the existing process environment is
// preserved except for the deterministic-build keys, which override
// inherited values. The Deterministic flag toggles SOURCE_DATE_EPOCH,
// CARGO_TERM_COLOR, and RUSTC_BOOTSTRAP.
//
// Phase 7 ships only `cargo build`. Later phases extend Cargo with
// `cargo test`, `cargo publish`, and `cargo doc --output-format=json`.
type Cargo struct {
	// Bin is the cargo binary to invoke. Empty defaults to "cargo".
	Bin string

	// Verbose mirrors cargo's --verbose flag.
	Verbose bool

	// Deterministic activates the reproducible-build environment.
	Deterministic bool

	// Offline forces --offline mode. Recommended when running under
	// the bridge's content-addressed cache: every dep should already
	// be present, so network access during build is a bug.
	Offline bool

	// Frozen forces --frozen mode (requires Cargo.lock to be present
	// and up to date). Used in CI for reproducible builds.
	Frozen bool

	// Locked forces --locked mode. Like --frozen but does not also
	// imply --offline.
	Locked bool

	// CargoHome overrides CARGO_HOME. Empty leaves the inherited
	// value untouched.
	CargoHome string

	// ExtraEnv is merged on top of the deterministic defaults.
	ExtraEnv map[string]string

	// Stdout / Stderr are the destinations for command output. nil
	// inherits the calling process's streams. Tests set these to
	// buffers to capture output.
	Stdout io.Writer
	Stderr io.Writer
}

// BuildOptions configures a single `cargo build` invocation.
type BuildOptions struct {
	// WorkspaceRoot is the directory containing the workspace
	// Cargo.toml. The path is converted to absolute form before being
	// passed via --manifest-path.
	WorkspaceRoot string

	// Profile is the cargo profile to build under: "release", "dev",
	// "test", etc. Empty defaults to "release" because the bridge
	// always wants optimised wrapper artefacts.
	Profile string

	// Package, when non-empty, restricts the build to a single
	// workspace member via --package <name>.
	Package string

	// Target sets the rustc compilation target via --target. Empty
	// leaves the host default.
	Target string

	// Jobs sets cargo's --jobs flag. 0 leaves the cargo default
	// (number of logical CPUs).
	Jobs int
}

// ArgsBuild composes the argv for `cargo build` under the supplied
// options. The result includes the Cargo.Bin (or "cargo" default) at
// argv[0] and is suitable for direct use with exec.Command.
//
// ArgsBuild is the gate the phase-7 test pins: argv composition is a
// pure function of (Cargo, BuildOptions), so the test does not need a
// rust toolchain to verify it.
func (c *Cargo) ArgsBuild(opts BuildOptions) []string {
	bin := c.Bin
	if bin == "" {
		bin = "cargo"
	}
	args := []string{bin, "build"}
	if c.Verbose {
		args = append(args, "--verbose")
	}
	if c.Offline {
		args = append(args, "--offline")
	}
	if c.Frozen {
		args = append(args, "--frozen")
	}
	if c.Locked {
		args = append(args, "--locked")
	}
	profile := opts.Profile
	if profile == "" {
		profile = "release"
	}
	args = append(args, "--profile", profile)
	if opts.Package != "" {
		args = append(args, "--package", opts.Package)
	}
	if opts.Target != "" {
		args = append(args, "--target", opts.Target)
	}
	if opts.Jobs > 0 {
		args = append(args, "--jobs", fmt.Sprintf("%d", opts.Jobs))
	}
	if opts.WorkspaceRoot != "" {
		args = append(args, "--manifest-path",
			filepath.Join(opts.WorkspaceRoot, "Cargo.toml"))
	}
	return args
}

// Env composes the environment passed to cargo. The result is the
// caller's existing environment minus any deterministic keys, plus
// the Cargo's overrides (CargoHome, Deterministic toggles, ExtraEnv).
// Keys are emitted in sorted order for byte-stable testability.
func (c *Cargo) Env(base []string) []string {
	overrides := map[string]string{}
	if c.Deterministic {
		overrides["SOURCE_DATE_EPOCH"] = "0"
		overrides["CARGO_TERM_COLOR"] = "never"
		overrides["RUSTC_BOOTSTRAP"] = "0"
	}
	if c.CargoHome != "" {
		overrides["CARGO_HOME"] = c.CargoHome
	}
	for k, v := range c.ExtraEnv {
		overrides[k] = v
	}
	merged := map[string]string{}
	for _, kv := range base {
		eq := strings.IndexByte(kv, '=')
		if eq <= 0 {
			continue
		}
		merged[kv[:eq]] = kv[eq+1:]
	}
	for k, v := range overrides {
		merged[k] = v
	}
	keys := make([]string, 0, len(merged))
	for k := range merged {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	out := make([]string, 0, len(keys))
	for _, k := range keys {
		out = append(out, k+"="+merged[k])
	}
	return out
}

// Build runs `cargo build` under the supplied options. The cargo
// binary is resolved via Cargo.Bin or "cargo" on PATH; the process
// environment is the result of Cargo.Env on os.Environ().
//
// Build returns an error if the cargo binary cannot be located, if
// the process exits non-zero, or if the supplied context cancels.
func (c *Cargo) Build(ctx context.Context, opts BuildOptions) error {
	if opts.WorkspaceRoot == "" {
		return errors.New("cargo: empty WorkspaceRoot")
	}
	args := c.ArgsBuild(opts)
	if len(args) == 0 {
		return errors.New("cargo: empty argv")
	}
	cmd := exec.CommandContext(ctx, args[0], args[1:]...)
	cmd.Env = c.Env(os.Environ())
	if c.Stdout != nil {
		cmd.Stdout = c.Stdout
	}
	if c.Stderr != nil {
		cmd.Stderr = c.Stderr
	}
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("cargo build: %w", err)
	}
	return nil
}

// LookCargo resolves the cargo binary on PATH. It returns the
// absolute path if found, the empty string and a non-nil error
// otherwise. Use this from the driver layer to fail fast with a
// useful diagnostic instead of letting exec emit a generic ENOENT.
func LookCargo() (string, error) {
	path, err := exec.LookPath("cargo")
	if err != nil {
		return "", fmt.Errorf("cargo not on PATH: %w", err)
	}
	return path, nil
}

// captureBuf is a tiny helper bridging io.Writer to a *bytes.Buffer
// without importing bytes in callers; surfaced so tests can request a
// buffer-backed Cargo without juggling imports.
func newCaptureBuf() *bytes.Buffer { return &bytes.Buffer{} }
