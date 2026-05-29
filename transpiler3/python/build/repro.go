package build

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// Phase 16.0: SOURCE_DATE_EPOCH support.
//
// The reproducible-builds.org spec defines SOURCE_DATE_EPOCH as the
// canonical override for build timestamps. When set, the wheel and
// sdist archive entry mtimes pin to this value instead of the
// hard-coded 1980-01-01 floor. The value is a non-negative integer
// (seconds since the Unix epoch).
//
// Source of truth: https://reproducible-builds.org/specs/source-date-epoch/
//
// The driver does not inject SOURCE_DATE_EPOCH automatically; the
// user / CI workflow is responsible for setting it (e.g.
// `SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)`). When unset, the
// wheel still ships byte-deterministic because zeroTime() falls back
// to the 1980 floor; the env var is the standard way to surface a
// human-readable commit timestamp in the archive metadata without
// breaking determinism.

// sourceDateEpoch returns the mtime that wheel and sdist entries
// should be stamped with. Order of precedence:
//
//  1. $SOURCE_DATE_EPOCH (reproducible-builds.org spec) when set to a
//     non-negative integer.
//  2. 1980-01-01 UTC (the zip format's DOS-encoded mtime floor).
//
// Any malformed SOURCE_DATE_EPOCH value (non-integer, negative, or
// outside int64 range) returns the floor instead of failing the
// build; matching dpkg's behaviour for graceful degradation.
func sourceDateEpoch() time.Time {
	if s := os.Getenv("SOURCE_DATE_EPOCH"); s != "" {
		if n, err := strconv.ParseInt(strings.TrimSpace(s), 10, 64); err == nil && n >= 0 {
			t := time.Unix(n, 0).UTC()
			// The zip DOS-encoded mtime cannot represent dates
			// before 1980; clamp to the floor in that case.
			floor := time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
			if t.Before(floor) {
				return floor
			}
			return t
		}
	}
	return time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
}

// gitCommitEpoch returns the unix-seconds timestamp of HEAD in
// repoDir as a decimal string, suitable for `SOURCE_DATE_EPOCH`.
// Returns an error when repoDir is not a git working tree or `git`
// is not on PATH; callers that want graceful degradation can
// silently ignore the error and fall back to the 1980 floor.
func gitCommitEpoch(repoDir string) (string, error) {
	cmd := exec.Command("git", "log", "-1", "--pretty=%ct")
	cmd.Dir = repoDir
	out, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("git log: %w", err)
	}
	s := strings.TrimSpace(string(out))
	if _, err := strconv.ParseInt(s, 10, 64); err != nil {
		return "", fmt.Errorf("git log: unexpected output %q: %w", s, err)
	}
	return s, nil
}

// sha256File returns the lowercase-hex SHA-256 digest of the file
// contents at path. Used by the Phase 16 gate to verify that two
// wheel builds of the same source produce byte-identical artifacts
// without holding the full archive in memory.
func sha256File(path string) (string, error) {
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	return hex.EncodeToString(h.Sum(nil)), nil
}
