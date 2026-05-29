// Package generics implements the MEP-70 Phase-14 generic monomorphisation
// path and the Android AAR consumer helper.
//
// # Generic monomorphisation
//
// Kotlin generics compile to erased bytecode: `fun <T> foo(x: T)` becomes
// `fun foo(x: Any?)` in the .class file. The bridge cannot automatically
// determine which concrete T the caller wants; the user enumerates explicit
// instantiations in mochi.toml:
//
//	[[kotlin.monomorphise]]
//	item    = "com.example.Repo.find"
//	T       = "com.example.User"
//
// MonomorphiseTable stores these entries. ApplyTable converts the erased
// APIObject.Function list into concrete instantiations: one bridged function
// per (function, T) pair, with the name mangled as <fn>_<TSimpleName>.
//
// # Android AAR consumer
//
// An Android Archive (.aar) is a ZIP containing at minimum:
//
//	classes.jar          (the JVM class bytes)
//	AndroidManifest.xml
//	res/                 (Android resources, ignored by the bridge)
//
// ExtractClassesJAR opens the .aar, locates classes.jar, and returns its
// bytes. The caller can then pass those bytes to metadata.IngestJARBytes
// exactly as for a plain Maven JAR.
//
// # KMP JVM variant selection
//
// Kotlin Multiplatform (KMP) artifacts publish a Gradle Module Metadata
// (.module) file listing per-platform variants. The bridge already uses
// maven.SelectJVMVariant to pick the JVM variant; this package provides the
// KMPArtifact type that wraps that logic and exposes whether an artifact is
// KMP-capable.
package generics

import (
	"archive/zip"
	"bytes"
	"fmt"
	"io"
	"strings"
)

// MonomorphiseEntry is one entry from mochi.toml's [[kotlin.monomorphise]] table.
type MonomorphiseEntry struct {
	// Item is the fully-qualified function/method path
	// (e.g. "com.example.Repo.find").
	Item string
	// T is the concrete type argument
	// (e.g. "com.example.User").
	T string
}

// MonomorphiseTable is the full set of user-declared instantiations.
type MonomorphiseTable []MonomorphiseEntry

// LookupItem returns all T values registered for a given item path.
func (mt MonomorphiseTable) LookupItem(item string) []string {
	var out []string
	for _, e := range mt {
		if e.Item == item {
			out = append(out, e.T)
		}
	}
	return out
}

// MonoFn is a concrete instantiation of a generic function.
type MonoFn struct {
	// BaseName is the original function name (e.g. "find").
	BaseName string
	// MangledName is the bridge-visible name with type suffix
	// (e.g. "find_User").
	MangledName string
	// KotlinFQN is the fully-qualified Kotlin call expression
	// (e.g. "com.example.Repo.find").
	KotlinFQN string
	// ConcreteT is the resolved type argument (e.g. "com.example.User").
	ConcreteT string
	// SimpleName is the simple (non-package) name of ConcreteT
	// (e.g. "User").
	SimpleName string
}

// Monomorphise takes a map of item → isGeneric flag, a MonomorphiseTable, and
// returns all concrete MonoFn instantiations declared in the table.
// Items not in genericItems are returned as-is (no suffix, ConcreteT = "").
// Items in genericItems but NOT in the table are silently skipped (they end
// up in the refusal list in the typemap layer).
func Monomorphise(items map[string]bool, table MonomorphiseTable) []MonoFn {
	var out []MonoFn
	for item, isGeneric := range items {
		if !isGeneric {
			// Non-generic: single passthrough with empty T.
			name := itemSimpleName(item)
			out = append(out, MonoFn{
				BaseName:    name,
				MangledName: name,
				KotlinFQN:   item,
			})
			continue
		}
		// Generic: emit one MonoFn per registered T.
		for _, t := range table.LookupItem(item) {
			simple := typeSimpleName(t)
			name := itemSimpleName(item)
			out = append(out, MonoFn{
				BaseName:    name,
				MangledName: name + "_" + simple,
				KotlinFQN:   item,
				ConcreteT:   t,
				SimpleName:  simple,
			})
		}
	}
	return out
}

// MonoShimLine returns the shim.mochi extern fn declaration for one MonoFn.
func MonoShimLine(fn MonoFn, cname, mochiReturn, paramList string) string {
	if fn.ConcreteT == "" {
		return fmt.Sprintf("extern fn %s(%s): %s from kotlin %q",
			cname, paramList, mochiReturn, fn.KotlinFQN)
	}
	return fmt.Sprintf("extern fn %s(%s): %s from kotlin %q monomorphise T=%q",
		cname, paramList, mochiReturn, fn.KotlinFQN, fn.ConcreteT)
}

// ─── Android AAR consumer ─────────────────────────────────────────────────────

// ExtractClassesJAR opens an .aar archive (as bytes) and returns the bytes of
// classes.jar contained within. Returns an error if no classes.jar is found.
func ExtractClassesJAR(aarBytes []byte) ([]byte, error) {
	zr, err := zip.NewReader(bytes.NewReader(aarBytes), int64(len(aarBytes)))
	if err != nil {
		return nil, fmt.Errorf("aar: open zip: %w", err)
	}
	for _, f := range zr.File {
		if f.Name == "classes.jar" {
			rc, err := f.Open()
			if err != nil {
				return nil, fmt.Errorf("aar: open classes.jar: %w", err)
			}
			data, err := io.ReadAll(rc)
			rc.Close()
			if err != nil {
				return nil, fmt.Errorf("aar: read classes.jar: %w", err)
			}
			return data, nil
		}
	}
	return nil, fmt.Errorf("aar: classes.jar not found in archive")
}

// IsAAR returns true when the filename or content-type indicates an Android
// Archive rather than a plain JAR.
func IsAAR(filename string) bool {
	return strings.HasSuffix(strings.ToLower(filename), ".aar")
}

// ─── KMP JVM variant selection ────────────────────────────────────────────────

// KMPArtifact describes the resolution of a Kotlin Multiplatform artifact.
type KMPArtifact struct {
	// IsKMP is true when the artifact has a Gradle Module Metadata (.module)
	// file advertising multiple platform variants.
	IsKMP bool
	// JVMVariantJARURL is the download URL of the JVM-target JAR when IsKMP
	// is true. Empty when IsKMP is false (use the normal Maven JAR URL).
	JVMVariantJARURL string
	// JVMVariantVersion is the version string of the JVM variant.
	JVMVariantVersion string
}

// SelectJVMVariant inspects a parsed Gradle Module Metadata (.module JSON)
// and returns the KMPArtifact descriptor. moduleJSON is the raw bytes of the
// .module file; baseURL is the Maven repository base URL.
//
// If the .module file does not contain a JVM variant, IsKMP is set to false
// and the caller should fall back to the normal JAR download.
func SelectJVMVariant(moduleJSON []byte, baseURL, group, artifact, version string) KMPArtifact {
	if len(moduleJSON) == 0 {
		return KMPArtifact{}
	}

	// Look for "jvm" or "jvmReleaseVariant" in the module JSON.
	// We use a simple string search rather than a full JSON parse so this
	// package has no external dependencies.
	content := string(moduleJSON)

	jvmIndicators := []string{
		`"jvm"`,
		`"jvmReleaseVariant"`,
		`"jvm-api"`,
		`"jvmApiElements"`,
		`"jvmRuntimeElements"`,
	}
	for _, indicator := range jvmIndicators {
		if strings.Contains(content, indicator) {
			// This is a KMP artifact with a JVM variant.
			jarURL := fmt.Sprintf("%s/%s/%s/%s/%s-%s.jar",
				strings.TrimRight(baseURL, "/"),
				strings.ReplaceAll(group, ".", "/"),
				artifact, version, artifact, version)
			return KMPArtifact{
				IsKMP:             true,
				JVMVariantJARURL:  jarURL,
				JVMVariantVersion: version,
			}
		}
	}
	return KMPArtifact{}
}

// ─── helpers ──────────────────────────────────────────────────────────────────

// itemSimpleName returns the last segment of a dot-separated qualified name.
// "com.example.Repo.find" → "find"
func itemSimpleName(fqn string) string {
	dot := strings.LastIndex(fqn, ".")
	if dot < 0 {
		return fqn
	}
	return fqn[dot+1:]
}

// typeSimpleName returns the simple class name from a FQN.
// "com.example.User" → "User"
func typeSimpleName(fqn string) string {
	dot := strings.LastIndex(fqn, ".")
	if dot < 0 {
		return fqn
	}
	return fqn[dot+1:]
}
