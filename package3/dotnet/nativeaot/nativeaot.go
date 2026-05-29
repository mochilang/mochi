// Package nativeaot provides NativeAOT opt-in support for MEP-68 shim projects.
// It includes a curated compatibility database, .csproj generation helpers,
// and static validation of shim surfaces for AOT safety.
package nativeaot

import (
	"fmt"
	"strings"

	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// Compatibility classifies a NuGet package's NativeAOT compatibility.
type Compatibility int

const (
	// CompatUnknown means AOT compatibility was not declared.
	CompatUnknown Compatibility = iota
	// CompatFull means the package declares IsAotCompatible=true.
	CompatFull
	// CompatPartial means the package works with AOT but has some limitations.
	CompatPartial
	// CompatIncompatible means the package is known to be AOT-incompatible.
	CompatIncompatible
)

// String renders the Compatibility.
func (c Compatibility) String() string {
	switch c {
	case CompatFull:
		return "CompatFull"
	case CompatPartial:
		return "CompatPartial"
	case CompatIncompatible:
		return "CompatIncompatible"
	default:
		return "CompatUnknown"
	}
}

// PackageAOTInfo holds NativeAOT compatibility information for a NuGet package.
type PackageAOTInfo struct {
	PackageID     string
	Version       string
	Compatibility Compatibility
	// Notes explains partial/incompatible status.
	Notes string
}

// knownCompatDB is the curated compatibility database from MEP-68 research note 11.
// Keys are lowercase package IDs for case-insensitive lookup.
var knownCompatDB = map[string]Compatibility{
	"system.text.json":                                CompatFull,
	"microsoft.extensions.dependencyinjection":        CompatFull,
	"microsoft.extensions.http":                       CompatFull,
	"newtonsoft.json":                                 CompatIncompatible,
	"serilog":                                         CompatPartial,
	"dapper":                                          CompatIncompatible,
	"nunit":                                           CompatIncompatible,
	"xunit":                                           CompatIncompatible,
	"fluentassertions":                                CompatPartial,
	"automapper":                                      CompatIncompatible,
	"mediatr":                                         CompatPartial,
	"fluentvalidation":                                CompatPartial,
	"polly":                                           CompatFull,
	"bogus":                                           CompatPartial,
	"moq":                                             CompatIncompatible,
	"restsharp":                                       CompatPartial,
	"stackexchange.redis":                             CompatPartial,
	"npgsql":                                          CompatFull,
	"awssdk.core":                                     CompatPartial,
	"microsoft.entityframeworkcore":                   CompatIncompatible,
}

// KnownCompatibility returns the known AOT compatibility for a package based
// on a curated database of the MEP-68 fixture corpus (20 packages).
// Returns CompatUnknown for packages not in the database.
// Lookup is case-insensitive.
func KnownCompatibility(id, version string) Compatibility {
	key := strings.ToLower(id)
	if c, ok := knownCompatDB[key]; ok {
		return c
	}
	return CompatUnknown
}

// AOTCompatibilityFromNuspec reads the <IsAotCompatible> tag from a .nuspec
// or .csproj string to determine declared AOT compatibility.
// Returns CompatFull if the tag is true, CompatIncompatible if false,
// and CompatUnknown if the tag is absent.
func AOTCompatibilityFromNuspec(xmlContent string) Compatibility {
	lower := strings.ToLower(xmlContent)
	_, afterOpen, found := strings.Cut(lower, "<isaotcompatible>")
	if !found {
		return CompatUnknown
	}
	inner, _, hasClose := strings.Cut(afterOpen, "</")
	if !hasClose {
		return CompatUnknown
	}
	value := strings.TrimSpace(inner)
	switch value {
	case "true":
		return CompatFull
	case "false":
		return CompatIncompatible
	default:
		return CompatUnknown
	}
}

// EmitAOTCsproj renders the .csproj additions required for NativeAOT publishing
// of the shim project: PublishAot=true, TrimmerRootFile, etc.
// It returns the complete .csproj content (not a fragment).
func EmitAOTCsproj(s *shimgen.Shim, runtimeID string) string {
	ns := safeNamespace(s.Package)
	var b strings.Builder
	b.WriteString("<Project Sdk=\"Microsoft.NET.Sdk\">\n")
	b.WriteString("  <PropertyGroup>\n")
	fmt.Fprintf(&b, "    <TargetFramework>%s</TargetFramework>\n", s.TargetFramework)
	b.WriteString("    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>\n")
	b.WriteString("    <PublishAot>true</PublishAot>\n")
	b.WriteString("    <TrimmerRootDescriptor>TrimmerRoots.xml</TrimmerRootDescriptor>\n")
	b.WriteString("    <Nullable>enable</Nullable>\n")
	b.WriteString("    <ImplicitUsings>disable</ImplicitUsings>\n")
	b.WriteString("    <SelfContained>true</SelfContained>\n")
	if runtimeID != "" {
		fmt.Fprintf(&b, "    <RuntimeIdentifier>%s</RuntimeIdentifier>\n", runtimeID)
	}
	fmt.Fprintf(&b, "    <AssemblyName>MochiShim.%s</AssemblyName>\n", ns)
	b.WriteString("  </PropertyGroup>\n")
	b.WriteString("  <ItemGroup>\n")
	fmt.Fprintf(&b, "    <PackageReference Include=%q Version=%q />\n",
		s.Package, "["+s.PackageVersion+"]")
	b.WriteString("  </ItemGroup>\n")
	b.WriteString("</Project>\n")
	return b.String()
}

// AOTPublishArgs returns the `dotnet publish` arguments for NativeAOT.
// e.g. ["publish", "-c", "Release", "-r", "linux-x64", "-p:PublishAot=true"]
func AOTPublishArgs(runtimeID string) []string {
	args := []string{
		"publish",
		"-c", "Release",
	}
	if runtimeID != "" {
		args = append(args, "-r", runtimeID)
	}
	args = append(args, "-p:PublishAot=true")
	return args
}

// AOTWarning records a method in the shim that may not be AOT-safe.
type AOTWarning struct {
	Method string
	Reason string
}

// ValidateForAOT checks whether a shimgen.Shim's type surface is AOT-safe.
// It returns warnings for features that require runtime reflection
// (dynamic dispatch, compound collection types in params/return that
// require type.MakeGenericType, etc.).
func ValidateForAOT(s *shimgen.Shim) []AOTWarning {
	if s == nil {
		return nil
	}
	var warnings []AOTWarning
	for _, m := range s.Methods {
		// Check return type.
		if w, ok := checkMappingAOT(m.CSMethodName, "return", m.Return); ok {
			warnings = append(warnings, w)
		}
		// Check params.
		for _, p := range m.Params {
			pm := p.Mapping
			if w, ok := checkMappingAOT(m.CSMethodName, "param:"+p.Name, &pm); ok {
				warnings = append(warnings, w)
			}
		}
	}
	return warnings
}

// checkMappingAOT returns a warning if the mapping requires reflection that
// is not safe for NativeAOT.
func checkMappingAOT(method, position string, m *typemap.Mapping) (AOTWarning, bool) {
	if m == nil {
		return AOTWarning{}, false
	}
	switch m.Kind {
	case typemap.KindHandle, typemap.KindRecord:
		// GCHandle.Alloc / GCHandle.FromIntPtr requires runtime type information
		// but is generally safe in AOT. However, warn if the handle type is not
		// a sealed/concrete type that can be statically rooted.
		return AOTWarning{
			Method: method,
			Reason: fmt.Sprintf("position %s: handle/record type %q requires GCHandle; ensure TrimmerRoots.xml roots this type", position, m.CLRName),
		}, true
	case typemap.KindList, typemap.KindSet, typemap.KindMap:
		return AOTWarning{
			Method: method,
			Reason: fmt.Sprintf("position %s: collection type %q may require MakeGenericType at runtime; use explicit monomorphise entries", position, m.CLRName),
		}, true
	case typemap.KindTuple:
		return AOTWarning{
			Method: method,
			Reason: fmt.Sprintf("position %s: tuple type may require reflection; verify AOT compatibility", position),
		}, true
	}
	return AOTWarning{}, false
}

// safeNamespace converts a NuGet package id to a C# namespace-safe segment.
func safeNamespace(pkg string) string {
	var b strings.Builder
	for _, r := range pkg {
		switch {
		case r == '.', r == '-':
			// drop separators
		case (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9'):
			b.WriteRune(r)
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}
