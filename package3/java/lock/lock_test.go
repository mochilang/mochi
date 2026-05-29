package lock_test

import (
	"strings"
	"testing"

	"mochi/package3/java/lock"
)

var sample = lock.JavaPackage{
	Group:         "com.google.guava",
	Artifact:      "guava",
	Version:       "33.0.0-jre",
	Source:        lock.Source{Kind: lock.SourceMaven},
	JarSHA256:     "abc123",
	JarSHA1:       "deadbeef",
	SurfaceSHA256: "surface456",
	WrapperSHA256: "wrap789",
	Dependencies:  []string{"com.google.code.findbugs:jsr305@3.0.2"},
}

func TestEncodeDecode_RoundTrip(t *testing.T) {
	encoded := lock.Encode([]lock.JavaPackage{sample})
	if !strings.Contains(encoded, "[[java-package]]") {
		t.Errorf("encoded output missing [[java-package]] header:\n%s", encoded)
	}

	got, err := lock.DecodeString(encoded)
	if err != nil {
		t.Fatalf("DecodeString error: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("want 1 entry; got %d", len(got))
	}
	p := got[0]
	if p.Group != sample.Group {
		t.Errorf("Group = %q; want %q", p.Group, sample.Group)
	}
	if p.Artifact != sample.Artifact {
		t.Errorf("Artifact = %q; want %q", p.Artifact, sample.Artifact)
	}
	if p.Version != sample.Version {
		t.Errorf("Version = %q; want %q", p.Version, sample.Version)
	}
	if p.JarSHA256 != sample.JarSHA256 {
		t.Errorf("JarSHA256 = %q; want %q", p.JarSHA256, sample.JarSHA256)
	}
	if p.JarSHA1 != sample.JarSHA1 {
		t.Errorf("JarSHA1 = %q; want %q", p.JarSHA1, sample.JarSHA1)
	}
	if p.SurfaceSHA256 != sample.SurfaceSHA256 {
		t.Errorf("SurfaceSHA256 = %q; want %q", p.SurfaceSHA256, sample.SurfaceSHA256)
	}
	if p.WrapperSHA256 != sample.WrapperSHA256 {
		t.Errorf("WrapperSHA256 = %q; want %q", p.WrapperSHA256, sample.WrapperSHA256)
	}
	if len(p.Dependencies) != 1 || p.Dependencies[0] != sample.Dependencies[0] {
		t.Errorf("Dependencies = %v; want %v", p.Dependencies, sample.Dependencies)
	}
}

func TestEncode_DeterministicOrder(t *testing.T) {
	a := lock.JavaPackage{Group: "z.group", Artifact: "z-art", Version: "1.0", Source: lock.Source{Kind: lock.SourceMaven}}
	b := lock.JavaPackage{Group: "a.group", Artifact: "a-art", Version: "1.0", Source: lock.Source{Kind: lock.SourceMaven}}
	out1 := lock.Encode([]lock.JavaPackage{a, b})
	out2 := lock.Encode([]lock.JavaPackage{b, a})
	if out1 != out2 {
		t.Errorf("Encode output not deterministic:\n%s\nvs\n%s", out1, out2)
	}
	if !strings.HasPrefix(out1, "[[java-package]]\ngroup = \"a.group\"") {
		t.Errorf("expected a.group first; got:\n%s", out1)
	}
}

func TestCheck_NoDrift(t *testing.T) {
	errs := lock.Check([]lock.JavaPackage{sample}, []lock.JavaPackage{sample})
	if len(errs) != 0 {
		t.Errorf("expected no drift; got %v", errs)
	}
}

func TestCheck_JarSHA256Drift(t *testing.T) {
	fresh := sample
	fresh.JarSHA256 = "different"
	errs := lock.Check([]lock.JavaPackage{sample}, []lock.JavaPackage{fresh})
	if len(errs) == 0 {
		t.Fatal("expected drift on jar-sha256")
	}
	if errs[0].Field != "jar-sha256" {
		t.Errorf("Field = %q; want jar-sha256", errs[0].Field)
	}
}

func TestCheck_MissingPackage(t *testing.T) {
	other := lock.JavaPackage{Group: "other", Artifact: "lib", Version: "1.0", Source: lock.Source{Kind: lock.SourceMaven}}
	errs := lock.Check([]lock.JavaPackage{sample}, []lock.JavaPackage{other})
	if len(errs) == 0 {
		t.Fatal("expected drift for missing package")
	}
	if errs[0].Field != "package" {
		t.Errorf("Field = %q; want package", errs[0].Field)
	}
}

func TestCheckErr_NilOnNoDrift(t *testing.T) {
	if err := lock.CheckErr([]lock.JavaPackage{sample}, []lock.JavaPackage{sample}); err != nil {
		t.Errorf("expected nil; got %v", err)
	}
}

func TestDecodeString_SkipsUnknownKeys(t *testing.T) {
	src := `[[java-package]]
group = "com.example"
artifact = "lib"
version = "1.0"
source = { kind = "maven" }
future-field = "ignored"
`
	pkgs, err := lock.DecodeString(src)
	if err != nil {
		t.Fatalf("DecodeString: %v", err)
	}
	if len(pkgs) != 1 || pkgs[0].Artifact != "lib" {
		t.Errorf("unexpected result: %+v", pkgs)
	}
}

func TestDecode_GitSource(t *testing.T) {
	src := `[[java-package]]
group = "com.example"
artifact = "lib"
version = "main"
source = { kind = "git", url = "https://github.com/example/lib", rev = "abc123" }
`
	pkgs, err := lock.DecodeString(src)
	if err != nil {
		t.Fatalf("DecodeString: %v", err)
	}
	if len(pkgs) != 1 {
		t.Fatalf("want 1; got %d", len(pkgs))
	}
	if pkgs[0].Source.Kind != lock.SourceGit {
		t.Errorf("Kind = %q; want git", pkgs[0].Source.Kind)
	}
	if pkgs[0].Source.Rev != "abc123" {
		t.Errorf("Rev = %q; want abc123", pkgs[0].Source.Rev)
	}
}
