// Package publish provides the NuGet package emit path.
package publish

import (
	"fmt"
	"strings"
)

// PackageMeta holds the metadata for a NuGet package.
type PackageMeta struct {
	// ID is the NuGet package identifier.
	ID string
	// Version is the package version.
	Version string
	// Authors is the package authors string.
	Authors string
	// Description is the package description.
	Description string
	// ProjectURL is the project homepage URL.
	ProjectURL string
	// License is the SPDX license expression.
	License string
	// Tags is a space-separated list of tags.
	Tags string
	// ReadmeFile is the relative path to the readme file.
	ReadmeFile string
	// RequireLicenseAcceptance controls whether consumers must accept the license.
	RequireLicenseAcceptance bool
}

// EmitNuspec renders a .nuspec manifest XML file for the given package metadata
// and target framework.
func EmitNuspec(meta PackageMeta, targetFramework string) string {
	var b strings.Builder
	b.WriteString(`<?xml version="1.0" encoding="utf-8"?>`)
	b.WriteString("\n")
	b.WriteString(`<package xmlns="http://schemas.microsoft.com/packaging/2013/05/nuspec.xsd">`)
	b.WriteString("\n")
	b.WriteString("  <metadata>\n")
	fmt.Fprintf(&b, "    <id>%s</id>\n", xmlEscape(meta.ID))
	fmt.Fprintf(&b, "    <version>%s</version>\n", xmlEscape(meta.Version))
	if meta.Authors != "" {
		fmt.Fprintf(&b, "    <authors>%s</authors>\n", xmlEscape(meta.Authors))
	}
	if meta.Description != "" {
		fmt.Fprintf(&b, "    <description>%s</description>\n", xmlEscape(meta.Description))
	}
	if meta.ProjectURL != "" {
		fmt.Fprintf(&b, "    <projectUrl>%s</projectUrl>\n", xmlEscape(meta.ProjectURL))
	}
	if meta.License != "" {
		fmt.Fprintf(&b, "    <license type=\"expression\">%s</license>\n", xmlEscape(meta.License))
	}
	if meta.Tags != "" {
		fmt.Fprintf(&b, "    <tags>%s</tags>\n", xmlEscape(meta.Tags))
	}
	if meta.ReadmeFile != "" {
		fmt.Fprintf(&b, "    <readme>%s</readme>\n", xmlEscape(meta.ReadmeFile))
	}
	if meta.RequireLicenseAcceptance {
		b.WriteString("    <requireLicenseAcceptance>true</requireLicenseAcceptance>\n")
	}
	b.WriteString("    <dependencies>\n")
	if targetFramework != "" {
		fmt.Fprintf(&b, "      <group targetFramework=%q />\n", targetFramework)
	} else {
		b.WriteString("      <group />\n")
	}
	b.WriteString("    </dependencies>\n")
	b.WriteString("  </metadata>\n")
	b.WriteString("</package>\n")
	return b.String()
}

// xmlEscape returns s with XML special characters replaced by their entities.
func xmlEscape(s string) string {
	s = strings.ReplaceAll(s, "&", "&amp;")
	s = strings.ReplaceAll(s, "<", "&lt;")
	s = strings.ReplaceAll(s, ">", "&gt;")
	s = strings.ReplaceAll(s, "\"", "&quot;")
	s = strings.ReplaceAll(s, "'", "&apos;")
	return s
}
