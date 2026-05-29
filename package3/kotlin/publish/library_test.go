package publish

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// ─── validateSpec ─────────────────────────────────────────────────────────────

func TestValidateSpec_OK(t *testing.T) {
	spec := LibrarySpec{
		Group:         "com.example",
		Artifact:      "mylib",
		Version:       "1.0.0",
		KotlinSources: map[string]string{"Main.kt": "fun hello() = 42"},
	}
	if err := validateSpec(spec); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

func TestValidateSpec_MissingGroup(t *testing.T) {
	spec := LibrarySpec{Artifact: "mylib", Version: "1.0", KotlinSources: map[string]string{"A.kt": "fun a() = 1"}}
	if err := validateSpec(spec); err == nil {
		t.Fatal("want error for missing group")
	}
}

func TestValidateSpec_MissingArtifact(t *testing.T) {
	spec := LibrarySpec{Group: "com.example", Version: "1.0", KotlinSources: map[string]string{"A.kt": "fun a() = 1"}}
	if err := validateSpec(spec); err == nil {
		t.Fatal("want error for missing artifact")
	}
}

func TestValidateSpec_MissingVersion(t *testing.T) {
	spec := LibrarySpec{Group: "com.example", Artifact: "mylib", KotlinSources: map[string]string{"A.kt": "fun a() = 1"}}
	if err := validateSpec(spec); err == nil {
		t.Fatal("want error for missing version")
	}
}

func TestValidateSpec_EmptySources(t *testing.T) {
	spec := LibrarySpec{Group: "com.example", Artifact: "mylib", Version: "1.0"}
	if err := validateSpec(spec); err == nil {
		t.Fatal("want error for empty sources")
	}
}

// ─── POM generation ───────────────────────────────────────────────────────────

func TestWritePOM(t *testing.T) {
	dir := t.TempDir()
	spec := LibrarySpec{Group: "org.example", Artifact: "cool-lib", Version: "2.5.1"}
	pomPath := filepath.Join(dir, "cool-lib-2.5.1.pom")
	if err := writePOM(spec, pomPath); err != nil {
		t.Fatalf("writePOM: %v", err)
	}
	data, err := os.ReadFile(pomPath)
	if err != nil {
		t.Fatalf("read pom: %v", err)
	}
	pom := string(data)
	for _, want := range []string{
		"<groupId>org.example</groupId>",
		"<artifactId>cool-lib</artifactId>",
		"<version>2.5.1</version>",
		"<packaging>jar</packaging>",
		`<?xml version="1.0"`,
	} {
		if !strings.Contains(pom, want) {
			t.Errorf("POM missing %q\nGot:\n%s", want, pom)
		}
	}
}

// ─── BuildLibrary (requires kotlinc) ─────────────────────────────────────────

func TestBuildLibrary_NoKotlinc(t *testing.T) {
	// If kotlinc is NOT present, expect ErrKotlincNotFound.
	// If it IS present, this test is skipped (handled by TestBuildLibrary_WithKotlinc).
	if _, err := resolveKotlinc(); err == nil {
		t.Skip("kotlinc is installed; skipping no-kotlinc error test")
	}

	dir := t.TempDir()
	spec := LibrarySpec{
		Group: "com.example", Artifact: "mylib", Version: "1.0",
		KotlinSources: map[string]string{"Main.kt": "fun greet() = \"hello\""},
	}
	_, err := BuildLibrary(spec, dir)
	if err == nil {
		t.Fatal("want error when kotlinc absent, got nil")
	}
	var notFound *ErrKotlincNotFound
	if !errors.As(err, &notFound) {
		t.Fatalf("want ErrKotlincNotFound, got %T: %v", err, err)
	}
}

func TestBuildLibrary_WithKotlinc(t *testing.T) {
	if _, err := resolveKotlinc(); err != nil {
		t.Skip("kotlinc not installed; skipping library compile test")
	}

	dir := t.TempDir()
	spec := LibrarySpec{
		Group:    "com.example",
		Artifact: "greeting-lib",
		Version:  "0.1.0",
		KotlinSources: map[string]string{
			"Greeting.kt": `package com.example

fun greet(name: String): String = "Hello, $name!"
`,
		},
		IncludeSources: true,
	}

	res, err := BuildLibrary(spec, dir)
	if err != nil {
		t.Fatalf("BuildLibrary: %v", err)
	}

	// JAR must exist.
	if _, err := os.Stat(res.JARPath); err != nil {
		t.Errorf("JAR not found at %s: %v", res.JARPath, err)
	}
	if !strings.HasSuffix(res.JARPath, "greeting-lib-0.1.0.jar") {
		t.Errorf("unexpected JAR name: %s", res.JARPath)
	}

	// POM must exist and have correct content.
	if _, err := os.Stat(res.POMPath); err != nil {
		t.Errorf("POM not found: %v", err)
	}
	pom, _ := os.ReadFile(res.POMPath)
	if !strings.Contains(string(pom), "<artifactId>greeting-lib</artifactId>") {
		t.Errorf("POM missing artifactId: %s", string(pom))
	}

	// The JAR must be non-empty (kotlinc produced real bytecode).
	info, _ := os.Stat(res.JARPath)
	if info.Size() == 0 {
		t.Error("JAR is empty")
	}
}

func TestBuildLibrary_WithKotlinc_MultipleFiles(t *testing.T) {
	if _, err := resolveKotlinc(); err != nil {
		t.Skip("kotlinc not installed")
	}

	dir := t.TempDir()
	spec := LibrarySpec{
		Group:    "org.example",
		Artifact: "multi-lib",
		Version:  "1.2.3",
		KotlinSources: map[string]string{
			"Api.kt": `package org.example
interface Api { fun compute(): Int }
`,
			"Impl.kt": `package org.example
class Impl : Api { override fun compute(): Int = 42 }
`,
		},
	}

	res, err := BuildLibrary(spec, dir)
	if err != nil {
		t.Fatalf("BuildLibrary: %v", err)
	}

	info, err := os.Stat(res.JARPath)
	if err != nil {
		t.Fatalf("JAR not found: %v", err)
	}
	if info.Size() == 0 {
		t.Error("JAR is empty")
	}
}

// ─── ErrKotlincNotFound ───────────────────────────────────────────────────────

func TestErrKotlincNotFound_Message(t *testing.T) {
	err := &ErrKotlincNotFound{}
	if !strings.Contains(err.Error(), "kotlinc") {
		t.Errorf("error message should mention kotlinc: %q", err.Error())
	}
}
