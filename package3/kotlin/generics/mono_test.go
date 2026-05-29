package generics

import (
	"archive/zip"
	"bytes"
	"strings"
	"testing"
)

// ─── MonomorphiseTable ────────────────────────────────────────────────────────

func TestLookupItem_Found(t *testing.T) {
	table := MonomorphiseTable{
		{Item: "com.example.Repo.find", T: "com.example.User"},
		{Item: "com.example.Repo.find", T: "com.example.Post"},
		{Item: "com.example.Cache.get", T: "com.example.Config"},
	}
	ts := table.LookupItem("com.example.Repo.find")
	if len(ts) != 2 {
		t.Fatalf("want 2 entries, got %d", len(ts))
	}
	if ts[0] != "com.example.User" || ts[1] != "com.example.Post" {
		t.Errorf("unexpected entries: %v", ts)
	}
}

func TestLookupItem_NotFound(t *testing.T) {
	table := MonomorphiseTable{
		{Item: "com.example.Repo.find", T: "com.example.User"},
	}
	ts := table.LookupItem("com.example.Missing.method")
	if len(ts) != 0 {
		t.Errorf("want 0, got %d: %v", len(ts), ts)
	}
}

// ─── Monomorphise ─────────────────────────────────────────────────────────────

func TestMonomorphise_NonGeneric(t *testing.T) {
	items := map[string]bool{"com.example.Api.call": false}
	fns := Monomorphise(items, nil)
	if len(fns) != 1 {
		t.Fatalf("want 1 fn, got %d", len(fns))
	}
	fn := fns[0]
	if fn.MangledName != "call" {
		t.Errorf("mangled name = %q; want call", fn.MangledName)
	}
	if fn.ConcreteT != "" {
		t.Errorf("non-generic should have empty ConcreteT")
	}
}

func TestMonomorphise_Generic_OneT(t *testing.T) {
	items := map[string]bool{"com.example.Repo.find": true}
	table := MonomorphiseTable{
		{Item: "com.example.Repo.find", T: "com.example.User"},
	}
	fns := Monomorphise(items, table)
	if len(fns) != 1 {
		t.Fatalf("want 1 fn, got %d", len(fns))
	}
	fn := fns[0]
	if fn.MangledName != "find_User" {
		t.Errorf("mangled = %q; want find_User", fn.MangledName)
	}
	if fn.SimpleName != "User" {
		t.Errorf("SimpleName = %q; want User", fn.SimpleName)
	}
	if fn.ConcreteT != "com.example.User" {
		t.Errorf("ConcreteT = %q", fn.ConcreteT)
	}
}

func TestMonomorphise_Generic_MultipleT(t *testing.T) {
	items := map[string]bool{"com.example.Repo.find": true}
	table := MonomorphiseTable{
		{Item: "com.example.Repo.find", T: "com.example.User"},
		{Item: "com.example.Repo.find", T: "com.example.Post"},
	}
	fns := Monomorphise(items, table)
	if len(fns) != 2 {
		t.Fatalf("want 2 fns, got %d", len(fns))
	}
	names := map[string]bool{}
	for _, fn := range fns {
		names[fn.MangledName] = true
	}
	if !names["find_User"] || !names["find_Post"] {
		t.Errorf("unexpected mangled names: %v", names)
	}
}

func TestMonomorphise_Generic_NoTableEntry(t *testing.T) {
	items := map[string]bool{"com.example.Repo.find": true}
	// No entry in table — generic is in refusal set; nothing emitted.
	fns := Monomorphise(items, nil)
	if len(fns) != 0 {
		t.Errorf("want 0 fns for unregistered generic, got %d", len(fns))
	}
}

// ─── MonoShimLine ─────────────────────────────────────────────────────────────

func TestMonoShimLine_NonGeneric(t *testing.T) {
	fn := MonoFn{BaseName: "call", MangledName: "call", KotlinFQN: "com.example.Api.call"}
	line := MonoShimLine(fn, "mochi_api_call", "string", "req: string")
	if !strings.Contains(line, `from kotlin "com.example.Api.call"`) {
		t.Errorf("missing kotlin annotation: %q", line)
	}
	if strings.Contains(line, "monomorphise") {
		t.Errorf("non-generic should not have monomorphise: %q", line)
	}
}

func TestMonoShimLine_Generic(t *testing.T) {
	fn := MonoFn{
		BaseName: "find", MangledName: "find_User",
		KotlinFQN: "com.example.Repo.find",
		ConcreteT: "com.example.User", SimpleName: "User",
	}
	line := MonoShimLine(fn, "mochi_repo_find_user", "any", "")
	if !strings.Contains(line, `monomorphise T="com.example.User"`) {
		t.Errorf("missing monomorphise annotation: %q", line)
	}
}

// ─── itemSimpleName / typeSimpleName ──────────────────────────────────────────

func TestItemSimpleName(t *testing.T) {
	cases := []struct{ in, want string }{
		{"com.example.Repo.find", "find"},
		{"find", "find"},
		{"a.b", "b"},
	}
	for _, c := range cases {
		if got := itemSimpleName(c.in); got != c.want {
			t.Errorf("itemSimpleName(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

func TestTypeSimpleName(t *testing.T) {
	cases := []struct{ in, want string }{
		{"com.example.User", "User"},
		{"User", "User"},
		{"com.example.deep.Type", "Type"},
	}
	for _, c := range cases {
		if got := typeSimpleName(c.in); got != c.want {
			t.Errorf("typeSimpleName(%q) = %q; want %q", c.in, got, c.want)
		}
	}
}

// ─── ExtractClassesJAR (AAR) ──────────────────────────────────────────────────

func buildAAR(t *testing.T, classesJARContent []byte) []byte {
	t.Helper()
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	// Add AndroidManifest.xml
	w, _ := zw.Create("AndroidManifest.xml")
	_, _ = w.Write([]byte(`<manifest package="com.example"/>`))
	// Add classes.jar
	w, _ = zw.Create("classes.jar")
	_, _ = w.Write(classesJARContent)
	// Add a res/ entry
	w, _ = zw.Create("res/values/strings.xml")
	_, _ = w.Write([]byte(`<resources/>`))
	_ = zw.Close()
	return buf.Bytes()
}

func TestExtractClassesJAR_Found(t *testing.T) {
	fakeJAR := []byte("PK fake jar bytes")
	aar := buildAAR(t, fakeJAR)

	got, err := ExtractClassesJAR(aar)
	if err != nil {
		t.Fatalf("ExtractClassesJAR: %v", err)
	}
	if !bytes.Equal(got, fakeJAR) {
		t.Errorf("classes.jar content mismatch")
	}
}

func TestExtractClassesJAR_Missing(t *testing.T) {
	var buf bytes.Buffer
	zw := zip.NewWriter(&buf)
	w, _ := zw.Create("AndroidManifest.xml")
	_, _ = w.Write([]byte(`<manifest/>`))
	_ = zw.Close()

	_, err := ExtractClassesJAR(buf.Bytes())
	if err == nil {
		t.Fatal("want error when classes.jar absent")
	}
	if !strings.Contains(err.Error(), "classes.jar not found") {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestExtractClassesJAR_NotZIP(t *testing.T) {
	_, err := ExtractClassesJAR([]byte("not a zip"))
	if err == nil {
		t.Fatal("want error for non-ZIP input")
	}
}

// ─── IsAAR ────────────────────────────────────────────────────────────────────

func TestIsAAR(t *testing.T) {
	cases := []struct {
		in   string
		want bool
	}{
		{"mylib.aar", true},
		{"MyLib.AAR", true},
		{"mylib.jar", false},
		{"mylib.aar.bak", false},
		{"", false},
	}
	for _, c := range cases {
		if got := IsAAR(c.in); got != c.want {
			t.Errorf("IsAAR(%q) = %v; want %v", c.in, got, c.want)
		}
	}
}

// ─── SelectJVMVariant ─────────────────────────────────────────────────────────

func TestSelectJVMVariant_KMP(t *testing.T) {
	moduleJSON := []byte(`{
  "formatVersion": "1.1",
  "component": {"group":"org.example","module":"lib","version":"1.0"},
  "variants": [
    {"name": "jvmApiElements", "attributes": {"org.jetbrains.kotlin.platform.type": "jvm"}},
    {"name": "jsApiElements", "attributes": {"org.jetbrains.kotlin.platform.type": "js"}}
  ]
}`)
	art := SelectJVMVariant(moduleJSON, "https://repo.maven.apache.org/maven2",
		"org.example", "lib", "1.0")
	if !art.IsKMP {
		t.Error("want IsKMP=true for KMP module JSON")
	}
	if art.JVMVariantJARURL == "" {
		t.Error("want non-empty JVMVariantJARURL")
	}
	if art.JVMVariantVersion != "1.0" {
		t.Errorf("version = %q; want 1.0", art.JVMVariantVersion)
	}
}

func TestSelectJVMVariant_NotKMP(t *testing.T) {
	moduleJSON := []byte(`{"formatVersion":"1.1","variants":[{"name":"releaseApiElements"}]}`)
	art := SelectJVMVariant(moduleJSON, "https://repo.maven.apache.org/maven2",
		"com.example", "lib", "2.0")
	if art.IsKMP {
		t.Error("want IsKMP=false for non-KMP module JSON")
	}
}

func TestSelectJVMVariant_Empty(t *testing.T) {
	art := SelectJVMVariant(nil, "", "g", "a", "1")
	if art.IsKMP {
		t.Error("want IsKMP=false for nil module JSON")
	}
}
