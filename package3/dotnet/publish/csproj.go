package publish

import (
	"fmt"
	"strings"
)

// LibraryCsproj holds the parameters for generating a library .csproj.
type LibraryCsproj struct {
	// TargetFramework is the TFM (e.g. "net8.0").
	TargetFramework string
	// AssemblyName is the output assembly name.
	AssemblyName string
	// Version is the package version.
	Version string
	// Authors is the package authors.
	Authors string
	// Description is the package description.
	Description string
	// PackageID is the NuGet package identifier.
	PackageID string
	// License is the SPDX license expression.
	License string
	// ProjectURL is the project homepage.
	ProjectURL string
	// IsNativeAOT enables PublishAot when true.
	IsNativeAOT bool
	// RuntimeID is the runtime identifier (e.g. "linux-x64", "osx-arm64", "win-x64").
	// When non-empty, a <RuntimeIdentifier> element is emitted.
	RuntimeID string
}

// EmitLibraryCsproj renders the .csproj for a Mochi-sourced .NET library package.
func EmitLibraryCsproj(p LibraryCsproj) string {
	var b strings.Builder
	b.WriteString("<Project Sdk=\"Microsoft.NET.Sdk\">\n")
	b.WriteString("  <PropertyGroup>\n")
	if p.TargetFramework != "" {
		fmt.Fprintf(&b, "    <TargetFramework>%s</TargetFramework>\n", p.TargetFramework)
	}
	if p.AssemblyName != "" {
		fmt.Fprintf(&b, "    <AssemblyName>%s</AssemblyName>\n", p.AssemblyName)
	}
	if p.Version != "" {
		fmt.Fprintf(&b, "    <Version>%s</Version>\n", p.Version)
	}
	if p.Authors != "" {
		fmt.Fprintf(&b, "    <Authors>%s</Authors>\n", p.Authors)
	}
	if p.Description != "" {
		fmt.Fprintf(&b, "    <Description>%s</Description>\n", p.Description)
	}
	if p.PackageID != "" {
		fmt.Fprintf(&b, "    <PackageId>%s</PackageId>\n", p.PackageID)
	}
	if p.License != "" {
		fmt.Fprintf(&b, "    <PackageLicenseExpression>%s</PackageLicenseExpression>\n", p.License)
	}
	if p.ProjectURL != "" {
		fmt.Fprintf(&b, "    <PackageProjectUrl>%s</PackageProjectUrl>\n", p.ProjectURL)
	}
	b.WriteString("    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>\n")
	b.WriteString("    <Nullable>enable</Nullable>\n")
	b.WriteString("    <ImplicitUsings>disable</ImplicitUsings>\n")
	if p.IsNativeAOT {
		b.WriteString("    <PublishAot>true</PublishAot>\n")
	}
	if p.RuntimeID != "" {
		fmt.Fprintf(&b, "    <RuntimeIdentifier>%s</RuntimeIdentifier>\n", p.RuntimeID)
	}
	b.WriteString("  </PropertyGroup>\n")
	b.WriteString("</Project>\n")
	return b.String()
}
