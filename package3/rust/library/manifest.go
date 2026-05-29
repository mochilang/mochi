package library

import (
	"fmt"
	"sort"
	"strings"
)

// RenderManifest renders the Cargo.toml file for a PublicAPI. The
// output is byte-stable and matches the layout MEP-73 §3 promises:
//
//   [package]
//   ... metadata in fixed order ...
//
//   [lib]
//   crate-type = ["rlib", "cdylib"]
//
//   [dependencies]
//   ... sorted by crate name ...
//
// Empty metadata fields are omitted (no `key = ""` rows). String
// arrays are rendered on a single line for short lists, matching
// cargo's default formatting.
func RenderManifest(api PublicAPI) string {
	var b strings.Builder
	b.WriteString("[package]\n")
	fmt.Fprintf(&b, "name = %q\n", api.CrateName)
	fmt.Fprintf(&b, "version = %q\n", api.Version)
	edition := api.Package.Edition
	if edition == "" {
		edition = "2021"
	}
	fmt.Fprintf(&b, "edition = %q\n", edition)
	if api.Package.RustVersion != "" {
		fmt.Fprintf(&b, "rust-version = %q\n", api.Package.RustVersion)
	}
	if api.Package.Description != "" {
		fmt.Fprintf(&b, "description = %q\n", api.Package.Description)
	}
	if api.Package.License != "" {
		fmt.Fprintf(&b, "license = %q\n", api.Package.License)
	}
	if api.Package.Repository != "" {
		fmt.Fprintf(&b, "repository = %q\n", api.Package.Repository)
	}
	if api.Package.Documentation != "" {
		fmt.Fprintf(&b, "documentation = %q\n", api.Package.Documentation)
	}
	if api.Package.Homepage != "" {
		fmt.Fprintf(&b, "homepage = %q\n", api.Package.Homepage)
	}
	if api.Package.Readme != "" {
		fmt.Fprintf(&b, "readme = %q\n", api.Package.Readme)
	}
	if len(api.Package.Authors) > 0 {
		writeStringArrayRow(&b, "authors", api.Package.Authors)
	}
	if len(api.Package.Keywords) > 0 {
		writeStringArrayRow(&b, "keywords", api.Package.Keywords)
	}
	if len(api.Package.Categories) > 0 {
		writeStringArrayRow(&b, "categories", api.Package.Categories)
	}

	b.WriteString("\n[lib]\n")
	b.WriteString("crate-type = [\"rlib\", \"cdylib\"]\n")

	if len(api.Dependencies) > 0 {
		b.WriteString("\n[dependencies]\n")
		names := make([]string, 0, len(api.Dependencies))
		for k := range api.Dependencies {
			names = append(names, k)
		}
		sort.Strings(names)
		for _, n := range names {
			fmt.Fprintf(&b, "%s = %q\n", n, api.Dependencies[n])
		}
	}
	return b.String()
}

func writeStringArrayRow(b *strings.Builder, key string, vs []string) {
	fmt.Fprintf(b, "%s = [", key)
	for i, v := range vs {
		if i > 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(b, "%q", v)
	}
	b.WriteString("]\n")
}
