// Package reflect implements the PHP Reflection API surface extractor for the
// MEP-75 PHP bridge. It invokes a PHP CLI script (reflect.php) via
// exec.Command and parses the emitted JSON surface document.
//
// See [website/docs/research/0075/03-prior-art-bridges.md] for the design.
package reflect

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// ReflectScriptName is the filename of the embedded PHP reflection script
// written to a temp dir before invocation.
const ReflectScriptName = "reflect.php"

// Run invokes the PHP CLI at phpBin, passing the embedded reflect.php script
// and pkgDir (the path to the extracted Composer package root). It returns the
// parsed ReflectionSurface JSON document.
//
// The reflect.php script is written to a temporary directory alongside the
// invocation. The caller retains ownership of pkgDir.
//
// Returns ErrPHPNotFound when phpBin is not executable, ErrReflectFailed when
// the PHP process exits non-zero.
func Run(ctx context.Context, phpBin, pkgDir string) (*ReflectionSurface, error) {
	if phpBin == "" {
		phpBin = "php"
	}

	// Verify the PHP binary exists and is executable.
	phpPath, err := exec.LookPath(phpBin)
	if err != nil {
		return nil, fmt.Errorf("%w: %s: %v", ErrPHPNotFound, phpBin, err)
	}

	// Write the reflect.php script to a temp dir.
	scriptDir, err := os.MkdirTemp("", "mochi-php-reflect-")
	if err != nil {
		return nil, fmt.Errorf("reflect: create script dir: %w", err)
	}
	defer os.RemoveAll(scriptDir) //nolint:errcheck

	scriptPath := filepath.Join(scriptDir, ReflectScriptName)
	if err := os.WriteFile(scriptPath, []byte(reflectPHPScript), 0o644); err != nil {
		return nil, fmt.Errorf("reflect: write script: %w", err)
	}

	// Invoke: php reflect.php <pkgDir>
	cmd := exec.CommandContext(ctx, phpPath, scriptPath, pkgDir)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		detail := stderr.String()
		if len(detail) > 512 {
			detail = detail[:512]
		}
		return nil, fmt.Errorf("%w: %s: %v\n%s", ErrReflectFailed, pkgDir, err, detail)
	}

	var surface ReflectionSurface
	if err := json.Unmarshal(stdout.Bytes(), &surface); err != nil {
		snippet := stdout.String()
		if len(snippet) > 256 {
			snippet = snippet[:256]
		}
		return nil, fmt.Errorf("reflect: parse JSON from %s: %w\noutput: %s", pkgDir, err, snippet)
	}
	return &surface, nil
}

// RunFromScript is like Run but accepts the PHP script source directly as
// scriptSrc instead of using the embedded reflect.php. Used in tests to inject
// a custom PHP script.
func RunFromScript(ctx context.Context, phpBin, pkgDir, scriptSrc string) (*ReflectionSurface, error) {
	if phpBin == "" {
		phpBin = "php"
	}
	phpPath, err := exec.LookPath(phpBin)
	if err != nil {
		return nil, fmt.Errorf("%w: %s: %v", ErrPHPNotFound, phpBin, err)
	}

	scriptDir, err := os.MkdirTemp("", "mochi-php-reflect-")
	if err != nil {
		return nil, fmt.Errorf("reflect: create script dir: %w", err)
	}
	defer os.RemoveAll(scriptDir) //nolint:errcheck

	scriptPath := filepath.Join(scriptDir, ReflectScriptName)
	if err := os.WriteFile(scriptPath, []byte(scriptSrc), 0o644); err != nil {
		return nil, fmt.Errorf("reflect: write script: %w", err)
	}

	cmd := exec.CommandContext(ctx, phpPath, scriptPath, pkgDir)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		detail := stderr.String()
		if len(detail) > 512 {
			detail = detail[:512]
		}
		return nil, fmt.Errorf("%w: %s: %v\n%s", ErrReflectFailed, pkgDir, err, detail)
	}

	var surface ReflectionSurface
	if err := json.Unmarshal(stdout.Bytes(), &surface); err != nil {
		snippet := stdout.String()
		if len(snippet) > 256 {
			snippet = snippet[:256]
		}
		return nil, fmt.Errorf("reflect: parse JSON from %s: %w\noutput: %s", pkgDir, err, snippet)
	}
	return &surface, nil
}

// ParseSurface parses a ReflectionSurface JSON document from raw bytes.
// Used when the JSON was obtained outside of Run (e.g. from a cache).
func ParseSurface(data []byte) (*ReflectionSurface, error) {
	var surface ReflectionSurface
	if err := json.Unmarshal(data, &surface); err != nil {
		return nil, fmt.Errorf("reflect: parse surface: %w", err)
	}
	return &surface, nil
}

// ErrPHPNotFound is returned when the PHP CLI binary cannot be located.
var ErrPHPNotFound = errors.New("reflect: PHP binary not found")

// ErrReflectFailed is returned when the reflect.php process exits non-zero.
var ErrReflectFailed = errors.New("reflect: PHP reflection failed")
