// Phase 16: reproducible build plumbing.
//
// reproBuildEnv returns the env slice for the `npm pack` subprocess
// when the driver's Deterministic flag is true. The two knobs:
//
//   - SOURCE_DATE_EPOCH=<commit unix time>: npm 9.5+ honours this
//     for every tar header `mtime`. Without it, npm stamps each
//     entry with `time.Now()` which guarantees a different tarball
//     SHA256 on every build.
//
//   - TZ=UTC: belt-and-braces. Prevents any sub-tool that reads
//     `mtime` from interpreting it in the host's local timezone
//     (one rebroadcast bug observed in older npm).
//
// The commit unix time defaults to 0 (Unix epoch) when the driver's
// SourceDateEpoch field is unset. That matches the Reproducible
// Builds spec: an absent SOURCE_DATE_EPOCH is equivalent to "the
// epoch", not "today".
//
// Defaulting to 0 over `time.Now()` is intentional: the gate that
// proves reproducibility (build twice on the same host, identical
// SHA256) requires the timestamp source to be fixed across builds.
// A zero default is fixed; `time.Now()` is not.
package build

import (
	"fmt"
	"os"
)

// reproBuildEnv returns the environment slice for an `npm pack`
// invocation that needs reproducible tarball output. The returned
// slice is a copy of os.Environ() with SOURCE_DATE_EPOCH + TZ
// appended (override on duplicate keys is left-to-right; the
// appended entries win because Go's exec.Cmd reads the last value).
func reproBuildEnv(sourceDateEpoch int64) []string {
	return append(
		os.Environ(),
		fmt.Sprintf("SOURCE_DATE_EPOCH=%d", sourceDateEpoch),
		"TZ=UTC",
	)
}
