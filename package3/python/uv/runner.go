package uv

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// Runner is the abstract surface a build phase calls to invoke uv. The default
// implementation shells out to the uv binary; tests substitute a fake.
type Runner interface {
	// Run executes `uv <args...>` in workDir with the given env. On success it
	// returns the stdout bytes; on failure it returns an error including the
	// captured stderr.
	Run(ctx context.Context, workDir string, env []string, args ...string) ([]byte, error)
	// Version reports the uv version string ("uv 0.5.7"). Empty when uv is not
	// installed or not on PATH.
	Version(ctx context.Context) (string, error)
}

// ExecRunner is the production Runner: it shells out to the uv binary.
type ExecRunner struct {
	// Binary is the path to the uv binary. Empty means "look on PATH".
	Binary string
	// Timeout caps each Run invocation. Zero means no per-call cap (the
	// context still applies).
	Timeout time.Duration
	// ExtraEnv is appended to the env passed to Run. Use this for
	// HTTPS_PROXY, UV_INDEX_URL, UV_KEYRING_PROVIDER overrides set by the
	// bridge.
	ExtraEnv []string
}

// NewExecRunner returns an ExecRunner with sensible defaults.
func NewExecRunner() *ExecRunner {
	return &ExecRunner{
		Timeout: 5 * time.Minute,
	}
}

// Locate searches PATH for the uv binary. Returns ("", error) when not found.
func Locate() (string, error) {
	for _, name := range []string{"uv"} {
		if p, err := exec.LookPath(name); err == nil {
			return p, nil
		}
	}
	return "", fmt.Errorf("uv: not found on PATH; install from https://docs.astral.sh/uv/")
}

// Run implements Runner.
func (r *ExecRunner) Run(ctx context.Context, workDir string, env []string, args ...string) ([]byte, error) {
	bin := r.Binary
	if bin == "" {
		located, err := Locate()
		if err != nil {
			return nil, err
		}
		bin = located
	}
	if r.Timeout > 0 {
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, r.Timeout)
		defer cancel()
	}
	cmd := exec.CommandContext(ctx, bin, args...)
	cmd.Dir = workDir
	full := append([]string{}, env...)
	full = append(full, r.ExtraEnv...)
	cmd.Env = full
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("uv %s: %w (stderr: %s)", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return stdout.Bytes(), nil
}

// Version implements Runner.
func (r *ExecRunner) Version(ctx context.Context) (string, error) {
	out, err := r.Run(ctx, "", os.Environ(), "--version")
	if err != nil {
		return "", err
	}
	return strings.TrimSpace(string(out)), nil
}

// LockOptions tunes a `uv lock` invocation.
type LockOptions struct {
	// PythonVersion forces uv to resolve for a specific interpreter
	// ("3.12.0", "3.13"). Empty uses the project's default.
	PythonVersion string
	// Resolution is "highest" (default), "lowest-direct", or "lowest".
	Resolution string
	// IndexURL overrides the default PyPI index.
	IndexURL string
	// ExtraIndexURLs are additional indexes to search.
	ExtraIndexURLs []string
	// NoBuild, when true, asks uv to refuse to resolve to any sdist that
	// requires a build (only wheels). Maps to `--no-build`.
	NoBuild bool
	// ExtraArgs are passed verbatim after the flags above.
	ExtraArgs []string
}

// BuildLockArgs renders LockOptions into the argv tail for `uv lock`. The
// returned slice does not include "lock" itself.
func (o LockOptions) BuildLockArgs() []string {
	var args []string
	if o.PythonVersion != "" {
		args = append(args, "--python", o.PythonVersion)
	}
	switch o.Resolution {
	case "", "highest":
		// default; no flag
	case "lowest", "lowest-direct":
		args = append(args, "--resolution", o.Resolution)
	}
	if o.IndexURL != "" {
		args = append(args, "--index-url", o.IndexURL)
	}
	for _, u := range o.ExtraIndexURLs {
		args = append(args, "--extra-index-url", u)
	}
	if o.NoBuild {
		args = append(args, "--no-build")
	}
	args = append(args, o.ExtraArgs...)
	return args
}

// Lock runs `uv lock` in projectDir and returns the rendered uv.lock contents
// by reading the file after the command completes. The lockfile path is
// `<projectDir>/uv.lock`.
func Lock(ctx context.Context, r Runner, projectDir string, opts LockOptions) ([]byte, error) {
	args := append([]string{"lock"}, opts.BuildLockArgs()...)
	if _, err := r.Run(ctx, projectDir, os.Environ(), args...); err != nil {
		return nil, err
	}
	path := filepath.Join(projectDir, "uv.lock")
	body, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("uv lock: read %s: %w", path, err)
	}
	return body, nil
}

// Export runs `uv export --format pylock.toml` in projectDir and returns the
// rendered pylock.toml bytes on stdout.
func Export(ctx context.Context, r Runner, projectDir string) ([]byte, error) {
	args := []string{"export", "--format", "pylock.toml"}
	out, err := r.Run(ctx, projectDir, os.Environ(), args...)
	if err != nil {
		return nil, fmt.Errorf("uv export pylock: %w", err)
	}
	return out, nil
}
