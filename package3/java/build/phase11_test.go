package build_test

import (
	"context"
	"os"
	"strings"
	"testing"

	"mochi/package3/java/build"
	"mochi/package3/java/maven"
)

func TestRenderPOM_Basic(t *testing.T) {
	spec := build.POMSpec{
		GroupID:    "dev.mochi.java-bridge",
		ArtifactID: "com-example-mylib-jni-wrapper",
		Version:    "1.0.0",
		Name:       "mylib JNI wrapper",
		Description: "Auto-generated JNI wrapper.",
		Dependencies: []build.POMDependency{
			{GroupID: "com.example", ArtifactID: "mylib", Version: "1.0.0"},
		},
	}
	out := build.RenderPOM(spec)
	for _, want := range []string{
		"<groupId>dev.mochi.java-bridge</groupId>",
		"<artifactId>com-example-mylib-jni-wrapper</artifactId>",
		"<version>1.0.0</version>",
		"<name>mylib JNI wrapper</name>",
		"<dependency>",
		"<scope>compile</scope>",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("RenderPOM output missing %q:\n%s", want, out)
		}
	}
}

func TestRenderPOM_XMLEscape(t *testing.T) {
	spec := build.POMSpec{
		GroupID:    "dev.mochi",
		ArtifactID: "mylib",
		Version:    "1.0",
		Description: "Has <special> & \"chars\"",
	}
	out := build.RenderPOM(spec)
	if strings.Contains(out, "<special>") {
		t.Errorf("RenderPOM did not escape < in description:\n%s", out)
	}
	if !strings.Contains(out, "&lt;special&gt;") {
		t.Errorf("RenderPOM did not encode < correctly:\n%s", out)
	}
}

func TestWrapperPOMSpec(t *testing.T) {
	coord, _ := maven.Parse("com.google.guava:guava@33.0.0-jre")
	spec := build.WrapperPOMSpec(coord)
	if spec.GroupID != "dev.mochi.java-bridge" {
		t.Errorf("GroupID = %q; want dev.mochi.java-bridge", spec.GroupID)
	}
	if !strings.Contains(spec.ArtifactID, "guava") {
		t.Errorf("ArtifactID %q does not contain guava", spec.ArtifactID)
	}
	if spec.Version != "33.0.0-jre" {
		t.Errorf("Version = %q; want 33.0.0-jre", spec.Version)
	}
	if len(spec.Dependencies) != 1 {
		t.Fatalf("want 1 dependency; got %d", len(spec.Dependencies))
	}
	dep := spec.Dependencies[0]
	if dep.GroupID != "com.google.guava" || dep.ArtifactID != "guava" {
		t.Errorf("dependency = %+v; want com.google.guava:guava", dep)
	}
}

func TestEmitTargetJavaLibrary_WritesFiles(t *testing.T) {
	tmp := t.TempDir()
	coord, _ := maven.Parse("com.example:mylib@1.0.0")

	tgt, err := build.EmitTargetJavaLibrary(context.Background(), build.EmitTargetOptions{
		OutDir: tmp,
		Coord:  coord,
		// ClassDir is empty: JAR step is skipped.
	})
	if err != nil {
		t.Fatalf("EmitTargetJavaLibrary: %v", err)
	}
	if tgt.POMPath == "" {
		t.Error("POMPath is empty")
	}
	if _, err := os.Stat(tgt.POMPath); err != nil {
		t.Errorf("POM file not on disk: %v", err)
	}
	// No ClassDir so JAR is not produced.
	if tgt.JARPath != "" {
		t.Errorf("expected empty JARPath when no ClassDir; got %q", tgt.JARPath)
	}
	// Verify POM content is valid XML with expected fields.
	pom, err := os.ReadFile(tgt.POMPath)
	if err != nil {
		t.Fatalf("read POM: %v", err)
	}
	for _, want := range []string{"<groupId>dev.mochi.java-bridge</groupId>", "<artifactId>", "<version>1.0.0</version>"} {
		if !strings.Contains(string(pom), want) {
			t.Errorf("POM missing %q:\n%s", want, pom)
		}
	}
}
