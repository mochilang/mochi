package build

import (
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase93AgentSource verifies the MEP-54 Phase 9.3 lowering
// emits the expected struct decl, pointer-receiver methods, and
// selector-form field accesses for a minimal counter agent. The
// agent_basic fixture exercises field mutation (`count = count + 1`)
// inside an intent plus a value-returning intent, so both the
// VarRef and AssignStmt arms of the __self->field rewrite get a
// gate.
func TestPhase93AgentSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "agent_basic", "agent_basic.mochi")
	outBin := filepath.Join(t.TempDir(), "agent_basic")
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
		"type Counter struct",
		"Count int64",
		"func (self *Counter) Increment()",
		"func (self *Counter) Value() int64",
		"self.Count = self.Count +",
		"return self.Count",
		"&Counter{Count:",
		".Increment()",
		".Value()",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}

// TestPhase93AgentStringSource verifies the string-typed field
// path: the receiver-method assigns and reads `self.Name` and the
// emitted struct field has Go type `string`.
func TestPhase93AgentStringSource(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1 ships POSIX `go` invocation only; Windows lands in Phase 16")
	}
	root := repoRoot(t)
	src := filepath.Join(root, "tests", "transpiler3", "go", "fixtures", "agent_string", "agent_string.mochi")
	outBin := filepath.Join(t.TempDir(), "agent_string")
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
		"type Greeter struct",
		"Name string",
		"func (self *Greeter) Set_name(",
		"func (self *Greeter) Get_name() string",
		"self.Name = ",
		"return self.Name",
	} {
		if !strings.Contains(got, marker) {
			t.Fatalf("main.go missing %q:\n%s", marker, got)
		}
	}
}
