package runtime

import "embed"

// Files carries the libmochi runtime tree. The build driver
// (transpiler3/c/build) walks it and writes each entry to the
// build directory before invoking cc. Headers go to
// $build/include/mochi/, C sources go to $build/src/.
//
// Phase 1 ships include/mochi/print.h + src/print.c. Each later
// phase that lands a new runtime module appends its files to
// the directive list below.
//
//go:embed include/mochi/print.h src/print.c
var Files embed.FS
