package extern

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

var zeroSHA [32]byte

func mustEmit(t *testing.T, artifact, version string, sha [32]byte, classes []*metadata.APIObject) string {
	t.Helper()
	dir := t.TempDir()
	out := filepath.Join(dir, "shim.mochi")
	if err := Emit(artifact, version, sha, classes, out); err != nil {
		t.Fatalf("Emit: %v", err)
	}
	data, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	return string(data)
}

func TestEmit_SimpleClass(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Greeter",
			Kind:      metadata.ClassKindClass,
			Functions: []metadata.Function{
				{
					Name:    "greet",
					JVMName: "greet",
					Flags:   metadata.FunctionFlags{IsPublic: true},
					Params: []metadata.Param{
						{Name: "name", Type: metadata.KotlinType{ClassName: "kotlin.String"}},
					},
					ReturnType: metadata.KotlinType{ClassName: "kotlin.String"},
				},
			},
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	if !strings.Contains(content, "extern type Greeter") {
		t.Errorf("missing extern type Greeter\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "extern fn") {
		t.Errorf("missing extern fn\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "greet") {
		t.Errorf("missing greet function\ncontent:\n%s", content)
	}
	if !strings.Contains(content, `from kotlin "com.example.Greeter"`) {
		t.Errorf("missing kotlin FQN\ncontent:\n%s", content)
	}
}

func TestEmit_VoidFunction(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Logger",
			Kind:      metadata.ClassKindClass,
			Functions: []metadata.Function{
				{
					Name:       "log",
					JVMName:    "log",
					Flags:      metadata.FunctionFlags{IsPublic: true},
					Params:     []metadata.Param{{Name: "msg", Type: metadata.KotlinType{ClassName: "kotlin.String"}}},
					ReturnType: metadata.KotlinType{ClassName: "kotlin.Unit"},
				},
			},
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	// Void function should not have ": " return type annotation.
	for _, line := range strings.Split(content, "\n") {
		if strings.Contains(line, "extern fn") && strings.Contains(line, "log") {
			if strings.Contains(line, "): ") {
				t.Errorf("void function should omit return type, got: %q", line)
			}
		}
	}
}

func TestEmit_SuspendFunction(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Fetcher",
			Kind:      metadata.ClassKindClass,
			Functions: []metadata.Function{
				{
					Name:       "fetchData",
					JVMName:    "fetchData",
					Flags:      metadata.FunctionFlags{IsPublic: true, IsSuspend: true},
					ReturnType: metadata.KotlinType{ClassName: "kotlin.String"},
				},
			},
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	if !strings.Contains(content, "extern type Handle") {
		t.Errorf("missing Handle type declaration\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "_async") {
		t.Errorf("missing _async function\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "_poll") {
		t.Errorf("missing _poll function\ncontent:\n%s", content)
	}

	// _async should return Handle
	for _, line := range strings.Split(content, "\n") {
		if strings.Contains(line, "_async") && strings.Contains(line, "extern fn") {
			if !strings.Contains(line, ": Handle") {
				t.Errorf("_async should return Handle, got: %q", line)
			}
		}
	}

	// _poll should return bool
	for _, line := range strings.Split(content, "\n") {
		if strings.Contains(line, "_poll") && strings.Contains(line, "extern fn") {
			if !strings.Contains(line, ": bool") {
				t.Errorf("_poll should return bool, got: %q", line)
			}
		}
	}
}

func TestEmit_SealedClass(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Result",
			Kind:      metadata.ClassKindSealedClass,
			SealedSubs: []*metadata.APIObject{
				{ClassName: "com.example.Result$Success", Kind: metadata.ClassKindDataClass},
				{ClassName: "com.example.Result$Failure", Kind: metadata.ClassKindDataClass},
			},
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	if !strings.Contains(content, "result_variant") {
		t.Errorf("missing variant discriminant fn\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "result_is_") {
		t.Errorf("missing is_ discriminant fn\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "success") && !strings.Contains(content, "failure") {
		t.Errorf("missing sealed subtype names\ncontent:\n%s", content)
	}
}

func TestEmit_EnumClass(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName:   "com.example.Direction",
			Kind:        metadata.ClassKindEnumClass,
			EnumEntries: []string{"NORTH", "SOUTH", "EAST", "WEST"},
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	// Should have per-entry constructor fns.
	for _, entry := range []string{"north", "south", "east", "west"} {
		if !strings.Contains(content, entry) {
			t.Errorf("missing enum entry fn for %q\ncontent:\n%s", entry, content)
		}
	}
}

func TestEmit_CustomOverride(t *testing.T) {
	dir := t.TempDir()
	out := filepath.Join(dir, "shim.mochi")

	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Widget",
			Kind:      metadata.ClassKindClass,
			Functions: []metadata.Function{
				{
					Name:       "render",
					JVMName:    "render",
					Flags:      metadata.FunctionFlags{IsPublic: true},
					ReturnType: metadata.KotlinType{ClassName: "kotlin.Unit"},
				},
			},
		},
	}

	// First emit to create file.
	if err := Emit("mylib", "1.0.0", zeroSHA, classes, out); err != nil {
		t.Fatalf("first Emit: %v", err)
	}

	// Read the file and add a custom line.
	data, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	customLine := `extern fn widget_render_custom(x: int): string from kotlin "com.example.Widget" custom`
	modified := string(data) + customLine + "\n"
	if err := os.WriteFile(out, []byte(modified), 0o644); err != nil {
		t.Fatal(err)
	}

	// Re-emit; the custom line should be preserved.
	if err := Emit("mylib", "1.0.0", zeroSHA, classes, out); err != nil {
		t.Fatalf("second Emit: %v", err)
	}

	result, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}
	content := string(result)
	if !strings.Contains(content, "custom") {
		t.Errorf("custom line was not preserved\ncontent:\n%s", content)
	}
}

func TestEmit_ObjectSingleton(t *testing.T) {
	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Registry",
			Kind:      metadata.ClassKindObject,
		},
	}

	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, classes)

	if !strings.Contains(content, "instance") {
		t.Errorf("missing instance() fn for object singleton\ncontent:\n%s", content)
	}
}

func TestEmit_HeaderComment(t *testing.T) {
	var sha [32]byte
	sha[0] = 0xde
	sha[1] = 0xad
	sha[2] = 0xbe
	sha[3] = 0xef

	content := mustEmit(t, "com.example:mylib", "2.0.0", sha, nil)

	if !strings.Contains(content, "// Code generated by mochi pkg lock. DO NOT EDIT.") {
		t.Errorf("missing header comment\ncontent:\n%s", content)
	}
	if !strings.Contains(content, "// Artifact: com.example:mylib@2.0.0") {
		t.Errorf("missing artifact comment\ncontent:\n%s", content)
	}
	expectedSHA := fmt.Sprintf("%x", sha)
	if !strings.Contains(content, "sha256: "+expectedSHA) {
		t.Errorf("missing sha256 in header\ncontent:\n%s", content)
	}
}

func TestEmit_EmptyClassList(t *testing.T) {
	content := mustEmit(t, "mylib", "1.0.0", zeroSHA, nil)

	if !strings.Contains(content, "// Code generated by mochi pkg lock. DO NOT EDIT.") {
		t.Errorf("missing header comment\ncontent:\n%s", content)
	}
	// No extern type declarations for empty list.
	if strings.Contains(content, "extern type") {
		t.Errorf("unexpected extern type in empty class list\ncontent:\n%s", content)
	}
	// No extern fn declarations.
	if strings.Contains(content, "extern fn") {
		t.Errorf("unexpected extern fn in empty class list\ncontent:\n%s", content)
	}
}

func TestEmit_RoundTrip(t *testing.T) {
	dir := t.TempDir()
	out := filepath.Join(dir, "shim.mochi")

	classes := []*metadata.APIObject{
		{
			ClassName: "com.example.Calculator",
			Kind:      metadata.ClassKindClass,
			Functions: []metadata.Function{
				{
					Name:    "add",
					JVMName: "add",
					Flags:   metadata.FunctionFlags{IsPublic: true},
					Params: []metadata.Param{
						{Name: "a", Type: metadata.KotlinType{ClassName: "kotlin.Int"}},
						{Name: "b", Type: metadata.KotlinType{ClassName: "kotlin.Int"}},
					},
					ReturnType: metadata.KotlinType{ClassName: "kotlin.Int"},
				},
			},
		},
	}

	if err := Emit("mylib", "1.0.0", zeroSHA, classes, out); err != nil {
		t.Fatalf("first Emit: %v", err)
	}
	data1, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}

	// Re-emit to same path.
	if err := Emit("mylib", "1.0.0", zeroSHA, classes, out); err != nil {
		t.Fatalf("second Emit: %v", err)
	}
	data2, err := os.ReadFile(out)
	if err != nil {
		t.Fatal(err)
	}

	lines1 := strings.Split(string(data1), "\n")
	lines2 := strings.Split(string(data2), "\n")
	if len(lines1) != len(lines2) {
		t.Errorf("round-trip produced different line count: %d vs %d", len(lines1), len(lines2))
	}
	for i, l := range lines1 {
		if i >= len(lines2) {
			break
		}
		if l != lines2[i] {
			t.Errorf("line %d differs:\n  first:  %q\n  second: %q", i+1, l, lines2[i])
		}
	}
}
