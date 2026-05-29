package build

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// goBuild invokes `go build` on workDir, writing the binary
// to outBin. Phase 1 ships the reproducibility flag set
// (-trimpath -buildvcs=false -ldflags=-buildid=) so the
// produced binary is byte-stable across CI hosts.
//
// target and profile are accepted for forward compatibility
// with Phase 16 (cross-compile matrix); Phase 1 ignores
// non-empty target values that don't match the host tuple and
// treats profile as informational.
func goBuild(goBin, workDir, outBin string, env []string) error {
	if goBin == "" {
		goBin = resolveGoBin()
	}
	absOut, err := filepath.Abs(outBin)
	if err != nil {
		return fmt.Errorf("transpiler3/go/build: abs %s: %w", outBin, err)
	}
	if err := os.MkdirAll(filepath.Dir(absOut), 0o755); err != nil {
		return fmt.Errorf("transpiler3/go/build: mkdir out: %w", err)
	}

	args := []string{
		"build",
		"-trimpath",
		"-buildvcs=false",
		"-ldflags=-buildid=",
		"-o", absOut,
		".",
	}
	cmd := exec.Command(goBin, args...)
	cmd.Dir = workDir
	cmd.Env = mergeEnv(os.Environ(), env)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("transpiler3/go/build: go build: %w\n%s", err, stderr.String())
	}
	return nil
}

// resolveGoBin returns the `go` binary path: $GOROOT/bin/go
// if GOROOT is set, otherwise plain "go" (PATH lookup).
func resolveGoBin() string {
	if root := os.Getenv("GOROOT"); root != "" {
		candidate := filepath.Join(root, "bin", "go")
		if _, err := os.Stat(candidate); err == nil {
			return candidate
		}
	}
	return "go"
}

// mergeEnv overlays extras on base: later keys win. Both
// inputs are KEY=value strings.
func mergeEnv(base, extras []string) []string {
	if len(extras) == 0 {
		return base
	}
	idx := make(map[string]int, len(base)+len(extras))
	out := append([]string(nil), base...)
	for i, kv := range out {
		if k, _, ok := splitEnv(kv); ok {
			idx[k] = i
		}
	}
	for _, kv := range extras {
		k, _, ok := splitEnv(kv)
		if !ok {
			continue
		}
		if i, found := idx[k]; found {
			out[i] = kv
		} else {
			idx[k] = len(out)
			out = append(out, kv)
		}
	}
	return out
}

func splitEnv(kv string) (string, string, bool) {
	k, v, ok := strings.Cut(kv, "=")
	return k, v, ok
}
