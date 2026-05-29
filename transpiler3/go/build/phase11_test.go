package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase11AsyncSource verifies the MEP-54 Phase 11.0 lowering
// emits the expected IIFE-with-goroutine shape for `async expr`
// and the channel-receive shape for `await fut`. The async_basic
// fixture is the minimum exercise: a single `async compute()` and
// `await fut`, with int64 element type. The gate checks for:
//
//   - the future-typed local declared as `chan int64`
//   - the IIFE that returns the seeded channel
//   - the goroutine spawn + send pattern
//   - the `<-fut` receive on the await side
func TestPhase11AsyncSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "async_basic", "async_basic.mochi")
	outBin := filepath.Join(t.TempDir(), "async_basic")
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
		"func() chan int64",
		"make(chan int64, 1)",
		"go func()",
		"__fut <-",
		"return __fut",
		"<-fut",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}

// TestPhase11AwaitAllSource verifies the MEP-54 Phase 11.2 lowering
// emits the loop-receive IIFE for `await_all([fs...])`. The async_all
// fixture binds three futures (int64) then drains them via a slice.
// The gate checks for:
//
//   - the `[]chan int64{...}` slice literal passed as the argument
//   - the inner function signature with `__futs []chan int64`
//   - the `make([]int64, len(__futs))` allocation
//   - the `range __futs` loop and `<-__f` receive
func TestPhase11AwaitAllSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "async_all", "async_all.mochi")
	outBin := filepath.Join(t.TempDir(), "async_all")
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
		"[]chan int64{f1, f2, f3}",
		"__futs []chan int64",
		"make([]int64, len(__futs))",
		"range __futs",
		"<-__f",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}

// TestPhase11AsyncStringSource exercises the string-element variant
// of the lowering so the chan element type is `string` (not int64)
// and the IIFE's return type matches.
func TestPhase11AsyncStringSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "async_string", "async_string.mochi")
	outBin := filepath.Join(t.TempDir(), "async_string")
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
		"func() chan string",
		"make(chan string, 1)",
		"go func()",
		"__fut <-",
		"return __fut",
		"<-fut",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
