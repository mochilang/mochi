package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase92StreamSource verifies the MEP-54 Phase 9.2 lowering
// emits the expected runtime helper calls and `sync` import for a
// minimal stream / subscribe / emit / recv_sub program. Catches
// regressions where the helper wiring or the generic type pin
// drifts away from the runtime helper signature.
func TestPhase92StreamSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "stream_basic", "stream_basic.mochi")
	outBin := filepath.Join(t.TempDir(), "stream_basic")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	mainPath := filepath.Join(d.WorkDirPath, "main.go")
	srcBytes, err := os.ReadFile(mainPath)
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	got := string(srcBytes)

	for _, marker := range []string{
		`"sync"`,
		"type mochiStream[T any] struct",
		"type mochiSub[T any] struct",
		"mochiStreamMake[int64](",
		"mochiStreamSubscribe[int64](",
		"mochiStreamEmit(",
		"mochiSubRecv(",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}

// TestPhase92SubscribeLimitSource verifies the bounded-subscriber
// path lowers through mochiStreamSubscribeLimit with the limit
// argument cast to int.
func TestPhase92SubscribeLimitSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "stream_subscribe_limit", "stream_subscribe_limit.mochi")
	outBin := filepath.Join(t.TempDir(), "stream_subscribe_limit")
	d := &Driver{CacheDir: t.TempDir(), KeepWorkDir: true}
	if err := d.Build(src, outBin, "", ""); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	defer os.RemoveAll(d.WorkDirPath)

	mainPath := filepath.Join(d.WorkDirPath, "main.go")
	srcBytes, err := os.ReadFile(mainPath)
	if err != nil {
		t.Fatalf("read main.go: %v", err)
	}
	got := string(srcBytes)

	for _, marker := range []string{
		"mochiStreamSubscribeLimit[int64](",
		"drops: true",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
