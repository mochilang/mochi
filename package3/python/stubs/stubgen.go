package stubs

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

// Stubgen is the abstract surface for the tier-4 stubgen fallback. The default
// implementation shells out to `python -m mypy.stubgen`. Tests substitute a
// fake that writes pre-baked .pyi bytes into a temp directory.
type Stubgen interface {
	// Generate runs stubgen for `name` (PEP 503 distribution name) /
	// `module` (Python import name) under sitePackages and returns a
	// StubSource pointing at the generated output. The output is always
	// marked Partial = true.
	Generate(name, module, sitePackages string) (*StubSource, error)
}

// ExecStubgen shells out to a Python interpreter that has mypy installed.
type ExecStubgen struct {
	// Python is the interpreter to invoke. Empty means "python3 on PATH".
	Python string
	// CacheDir is the directory under which per-package output trees are
	// written. The bridge typically points this at the venv's stubgen-out
	// directory so phase 8's content-addressed cache can recover.
	CacheDir string
	// Timeout caps each stubgen invocation. Zero uses 60 seconds.
	Timeout time.Duration
}

// NewExecStubgen returns an ExecStubgen with sensible defaults.
func NewExecStubgen(cacheDir string) *ExecStubgen {
	return &ExecStubgen{
		Python:   "",
		CacheDir: cacheDir,
		Timeout:  60 * time.Second,
	}
}

// Generate implements Stubgen.
func (s *ExecStubgen) Generate(name, module, sitePackages string) (*StubSource, error) {
	if s.CacheDir == "" {
		return nil, fmt.Errorf("stubgen: CacheDir is empty")
	}
	out := filepath.Join(s.CacheDir, name)
	if err := os.MkdirAll(out, 0o755); err != nil {
		return nil, fmt.Errorf("stubgen: mkdir %q: %w", out, err)
	}
	bin := s.Python
	if bin == "" {
		path, err := exec.LookPath("python3")
		if err != nil {
			return nil, fmt.Errorf("stubgen: python3 not on PATH: %w", err)
		}
		bin = path
	}
	timeout := s.Timeout
	if timeout == 0 {
		timeout = 60 * time.Second
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	args := []string{
		"-m", "mypy.stubgen",
		"--package", module,
		"--output", out,
		"--quiet",
	}
	cmd := exec.CommandContext(ctx, bin, args...)
	// Run inside the venv site-packages so stubgen can find the package.
	if sitePackages != "" {
		cmd.Env = append(os.Environ(), "PYTHONPATH="+sitePackages)
	}
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("stubgen: %s %s: %w (stderr: %s)", bin, strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	files, _ := walkStubFiles(out)
	return &StubSource{
		Package: name,
		Tier:    TierStubgen,
		RootDir: out,
		Files:   files,
		Partial: true,
	}, nil
}

// FakeStubgen is a test double that "generates" stubs by writing a fixed
// per-module .pyi body to disk. The body is set once at construction.
type FakeStubgen struct {
	CacheDir string
	Body     []byte
}

// Generate implements Stubgen.
func (f *FakeStubgen) Generate(name, module, sitePackages string) (*StubSource, error) {
	if f.CacheDir == "" {
		return nil, fmt.Errorf("FakeStubgen: empty CacheDir")
	}
	out := filepath.Join(f.CacheDir, name)
	if err := os.MkdirAll(out, 0o755); err != nil {
		return nil, err
	}
	path := filepath.Join(out, module+".pyi")
	if err := os.WriteFile(path, f.Body, 0o644); err != nil {
		return nil, err
	}
	return &StubSource{
		Package: name,
		Tier:    TierStubgen,
		RootDir: out,
		Files:   []string{module + ".pyi"},
		Partial: true,
	}, nil
}
