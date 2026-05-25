package build

import (
	"runtime"
	"testing"
)

// TestPhase8Arena gates Phase 8.3: mochi_arena_t bump allocator backing
// query result lists. The arena is stack-allocated at the query boundary,
// result elements are appended via mochi_list_<T>_append_arena, and the
// surviving list is copied to heap via mochi_list_<T>_copy_heap before the
// arena is freed. Exercises int, float, bool, string result types; large
// results that exceed the initial arena chunk; nested queries; inner join;
// and order-by+take combinations.
func TestPhase8Arena(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("C toolchain not available in Windows CI")
	}
	runFixtureSuite(t, "arena_query")
}
