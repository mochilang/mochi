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
//go:embed include/mochi/print.h include/mochi/errors.h include/mochi/except.h include/mochi/arena.h include/mochi/list.h include/mochi/map.h include/mochi/strings.h include/mochi/fileio.h include/mochi/value.h include/mochi/csv.h include/mochi/sched.h include/mochi/chan.h include/mochi/stream.h include/mochi/shutdown.h include/mochi/llm.h include/mochi/go_rpc.h src/print.c src/errors.c src/except.c src/arena.c src/list.c src/map.c src/strings.c src/fileio.c src/value.c src/csv.c src/sched.c src/chan.c src/stream.c src/shutdown.c src/llm.c src/go_rpc.c
var Files embed.FS
