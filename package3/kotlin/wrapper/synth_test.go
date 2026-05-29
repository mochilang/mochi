package wrapper

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/mochilang/mochi/package3/kotlin/metadata"
)

// simpleClass returns a minimal APIObject for testing.
func simpleClass(name string, fns ...metadata.Function) *metadata.APIObject {
	return &metadata.APIObject{
		ClassName:    "com.example." + name,
		JVMClassName: "com/example/" + name,
		Kind:         metadata.ClassKindClass,
		Functions:    fns,
	}
}

func strFn(name string, params []metadata.Param, ret metadata.KotlinType) metadata.Function {
	return metadata.Function{
		Name:       name,
		JVMName:    name,
		Params:     params,
		ReturnType: ret,
		Flags:      metadata.FunctionFlags{IsPublic: true},
	}
}

func kt(className string) metadata.KotlinType {
	return metadata.KotlinType{ClassName: className}
}

func ktVoid() metadata.KotlinType {
	return metadata.KotlinType{ClassName: "kotlin.Unit"}
}

// ---------------------------------------------------------------------------
// Test 1: simple class with greet(name: String): String
// ---------------------------------------------------------------------------

func TestSynthesize_SimpleClass(t *testing.T) {
	obj := simpleClass("MyClass",
		strFn("greet", []metadata.Param{
			{Name: "name", Type: kt("kotlin.String")},
		}, kt("kotlin.String")),
	)

	dir := t.TempDir()
	if err := Synthesize("mylib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	pkgDir := filepath.Join(dir, "java", "com", "mochi", "bridge", "mylib")

	// MochiBridge.java must exist and contain the @CEntryPoint annotation
	bridgePath := filepath.Join(pkgDir, "MochiBridge.java")
	bridgeContent, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("MochiBridge.java not found: %v", err)
	}
	bridgeStr := string(bridgeContent)
	if !strings.Contains(bridgeStr, `@CEntryPoint(name = "my_class_greet")`) {
		t.Errorf("MochiBridge.java: expected @CEntryPoint(name = \"my_class_greet\"), got:\n%s", bridgeStr)
	}

	// MochiHandleRegistry.java must exist
	regPath := filepath.Join(pkgDir, "MochiHandleRegistry.java")
	if _, err := os.Stat(regPath); err != nil {
		t.Fatalf("MochiHandleRegistry.java not found: %v", err)
	}

	// MochiJNI.java must exist
	jniPath := filepath.Join(pkgDir, "MochiJNI.java")
	if _, err := os.Stat(jniPath); err != nil {
		t.Fatalf("MochiJNI.java not found: %v", err)
	}

	// Verify MochiHandleRegistry.java content
	regContent, err := os.ReadFile(regPath)
	if err != nil {
		t.Fatalf("read MochiHandleRegistry.java: %v", err)
	}
	regStr := string(regContent)
	if !strings.Contains(regStr, "ConcurrentHashMap") {
		t.Error("MochiHandleRegistry.java: missing ConcurrentHashMap")
	}
	if !strings.Contains(regStr, "public static long register") {
		t.Error("MochiHandleRegistry.java: missing register method")
	}

	// Verify MochiJNI.java content
	jniContent, err := os.ReadFile(jniPath)
	if err != nil {
		t.Fatalf("read MochiJNI.java: %v", err)
	}
	jniStr := string(jniContent)
	if !strings.Contains(jniStr, "listToJson") {
		t.Error("MochiJNI.java: missing listToJson")
	}
	if !strings.Contains(jniStr, "mapToJson") {
		t.Error("MochiJNI.java: missing mapToJson")
	}
}

// ---------------------------------------------------------------------------
// Test 2: void return function
// ---------------------------------------------------------------------------

func TestSynthesize_VoidReturn(t *testing.T) {
	obj := simpleClass("Logger",
		strFn("log", []metadata.Param{
			{Name: "message", Type: kt("kotlin.String")},
		}, ktVoid()),
	)

	dir := t.TempDir()
	if err := Synthesize("loglib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	bridgePath := filepath.Join(dir, "java", "com", "mochi", "bridge", "loglib", "MochiBridge.java")
	content, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("MochiBridge.java not found: %v", err)
	}
	src := string(content)
	if !strings.Contains(src, "public static void ") {
		t.Errorf("expected void return type method, got:\n%s", src)
	}
}

// ---------------------------------------------------------------------------
// Test 3: object handle parameter
// ---------------------------------------------------------------------------

func TestSynthesize_HandleParam(t *testing.T) {
	obj := simpleClass("CallFactory",
		strFn("execute", []metadata.Param{
			{Name: "call", Type: kt("com.example.Call")},
		}, kt("kotlin.String")),
	)

	dir := t.TempDir()
	if err := Synthesize("http", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	bridgePath := filepath.Join(dir, "java", "com", "mochi", "bridge", "http", "MochiBridge.java")
	content, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("MochiBridge.java not found: %v", err)
	}
	src := string(content)
	// com.example.Call is an extern type -> long parameter
	if !strings.Contains(src, "long call") {
		t.Errorf("expected long parameter for extern type, got:\n%s", src)
	}
}

// ---------------------------------------------------------------------------
// Test 4: data class constructor entry point
// ---------------------------------------------------------------------------

func TestSynthesize_DataClassConstructor(t *testing.T) {
	obj := &metadata.APIObject{
		ClassName:    "com.example.User",
		JVMClassName: "com/example/User",
		Kind:         metadata.ClassKindDataClass,
		Constructors: []metadata.Constructor{
			{
				IsPrimary: true,
				Params: []metadata.Param{
					{Name: "name", Type: kt("kotlin.String")},
					{Name: "age", Type: kt("kotlin.Int")},
				},
			},
		},
	}

	dir := t.TempDir()
	if err := Synthesize("userlib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	bridgePath := filepath.Join(dir, "java", "com", "mochi", "bridge", "userlib", "MochiBridge.java")
	content, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("MochiBridge.java not found: %v", err)
	}
	src := string(content)
	// Should have a constructor entry point
	if !strings.Contains(src, "@CEntryPoint") {
		t.Errorf("data class constructor: expected @CEntryPoint annotation, got:\n%s", src)
	}
}

// ---------------------------------------------------------------------------
// Test 5: empty class (no public functions)
// ---------------------------------------------------------------------------

func TestSynthesize_EmptyClass(t *testing.T) {
	obj := simpleClass("EmptyClass")

	dir := t.TempDir()
	if err := Synthesize("emptylib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	pkgDir := filepath.Join(dir, "java", "com", "mochi", "bridge", "emptylib")

	// All three files should still be generated
	for _, fname := range []string{"MochiBridge.java", "MochiHandleRegistry.java", "MochiJNI.java"} {
		p := filepath.Join(pkgDir, fname)
		if _, err := os.Stat(p); err != nil {
			t.Errorf("empty class: %s not generated: %v", fname, err)
		}
	}

	// Bridge should have no @CEntryPoint annotations
	bridgeContent, err := os.ReadFile(filepath.Join(pkgDir, "MochiBridge.java"))
	if err != nil {
		t.Fatalf("read MochiBridge.java: %v", err)
	}
	if strings.Contains(string(bridgeContent), "@CEntryPoint") {
		t.Error("empty class: MochiBridge.java should have no @CEntryPoint annotations")
	}
}

// ---------------------------------------------------------------------------
// Test 6: naming collision - two functions with same base name
// ---------------------------------------------------------------------------

func TestSynthesize_NamingCollision(t *testing.T) {
	// Two overloaded get() methods
	obj := &metadata.APIObject{
		ClassName:    "com.example.Store",
		JVMClassName: "com/example/Store",
		Kind:         metadata.ClassKindClass,
		Functions: []metadata.Function{
			{
				Name:       "get",
				JVMName:    "get",
				Params:     []metadata.Param{{Name: "id", Type: kt("kotlin.Int")}},
				ReturnType: kt("kotlin.String"),
				Flags:      metadata.FunctionFlags{IsPublic: true},
			},
			{
				Name:       "get",
				JVMName:    "get",
				Params:     []metadata.Param{{Name: "key", Type: kt("kotlin.String")}},
				ReturnType: kt("kotlin.Int"),
				Flags:      metadata.FunctionFlags{IsPublic: true},
			},
		},
	}

	dir := t.TempDir()
	if err := Synthesize("storelib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	bridgePath := filepath.Join(dir, "java", "com", "mochi", "bridge", "storelib", "MochiBridge.java")
	content, err := os.ReadFile(bridgePath)
	if err != nil {
		t.Fatalf("MochiBridge.java not found: %v", err)
	}
	src := string(content)

	// Should have two distinct @CEntryPoint names
	count := strings.Count(src, "@CEntryPoint")
	if count < 2 {
		t.Errorf("naming collision: expected at least 2 @CEntryPoint annotations, got %d:\n%s", count, src)
	}

	// Verify they have distinct names (one is "store_get", other is "store_get_2")
	if !strings.Contains(src, `"store_get"`) {
		t.Errorf("naming collision: expected store_get entry point:\n%s", src)
	}
	if !strings.Contains(src, `"store_get_2"`) {
		t.Errorf("naming collision: expected store_get_2 entry point:\n%s", src)
	}
}

// ---------------------------------------------------------------------------
// Compilation test (skipped if javac not available)
// ---------------------------------------------------------------------------

func TestSynthesize_JavacCompilation(t *testing.T) {
	javacPath, err := exec.LookPath("javac")
	if err != nil {
		t.Skip("javac not found in PATH; skipping compilation test")
	}
	// Verify javac is actually usable (macOS stubs may print an error and exit non-zero)
	probe := exec.Command(javacPath, "-version")
	if out, err := probe.CombinedOutput(); err != nil {
		t.Skipf("javac not functional (%v): %s", err, strings.TrimSpace(string(out)))
	}

	obj := simpleClass("Greeter",
		strFn("greet", []metadata.Param{
			{Name: "name", Type: kt("kotlin.String")},
		}, kt("kotlin.String")),
	)

	dir := t.TempDir()
	if err := Synthesize("greeterlib", []*metadata.APIObject{obj}, dir); err != nil {
		t.Fatalf("Synthesize error: %v", err)
	}

	pkgDir := filepath.Join(dir, "java", "com", "mochi", "bridge", "greeterlib")

	// Try to compile only MochiHandleRegistry.java (no external deps)
	regPath := filepath.Join(pkgDir, "MochiHandleRegistry.java")
	cmd := exec.Command("javac", "-d", dir, regPath)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Errorf("javac compilation failed: %v\n%s", err, out)
	}
}

// ---------------------------------------------------------------------------
// Helper: mochiToJavaCamel
// ---------------------------------------------------------------------------

func TestMochiToJavaCamel(t *testing.T) {
	cases := []struct {
		input string
		want  string
	}{
		{"my_class_greet", "myClassGreet"},
		{"greet", "greet"},
		{"store_get", "storeGet"},
		{"my_class_get_2", "myClassGet2"},
		{"", ""},
	}
	for _, tc := range cases {
		got := mochiToJavaCamel(tc.input)
		if got != tc.want {
			t.Errorf("mochiToJavaCamel(%q): got %q, want %q", tc.input, got, tc.want)
		}
	}
}
