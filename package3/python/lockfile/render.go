package lockfile

import (
	"fmt"
	"strings"
)

// RenderTOML produces a deterministic TOML document for the manifest. The
// schema:
//
//	# Auto-generated. Do not edit by hand. Regenerate via `mochi pkg lock`.
//	schema-version = 1
//
//	[[python-package]]
//	name = "..."
//	alias = "..."
//	source = "registry"
//	version = "2.6.1"
//	specifier = ">=2.0, <3.0"
//	wrapper-sha256 = "..."
//	capabilities = ["async", "dataclass"]
//
// Optional fields are omitted when empty. The order inside each table is
// fixed; the order across tables matches Manifest.Entries (sorted by
// (Name, Alias)).
func RenderTOML(m Manifest) string {
	out := m
	out.Sort()
	v := out.Version
	if v == 0 {
		v = SchemaVersion
	}
	var b strings.Builder
	b.WriteString("# Auto-generated MEP-71 python lockfile. Do not edit by hand.\n")
	b.WriteString("# Regenerate via `mochi pkg lock`.\n")
	fmt.Fprintf(&b, "schema-version = %d\n", v)
	b.WriteString("\n")
	for _, e := range out.Entries {
		renderEntry(&b, e)
	}
	return b.String()
}

func renderEntry(b *strings.Builder, e Entry) {
	b.WriteString("[[python-package]]\n")
	fmt.Fprintf(b, "name = %q\n", e.Name)
	fmt.Fprintf(b, "alias = %q\n", e.Alias)
	fmt.Fprintf(b, "source = %q\n", string(e.Source))
	if e.Version != "" {
		fmt.Fprintf(b, "version = %q\n", e.Version)
	}
	if e.Specifier != "" {
		fmt.Fprintf(b, "specifier = %q\n", e.Specifier)
	}
	if e.IndexURL != "" {
		fmt.Fprintf(b, "index-url = %q\n", e.IndexURL)
	}
	if e.GitURL != "" {
		fmt.Fprintf(b, "git-url = %q\n", e.GitURL)
	}
	if e.GitRev != "" {
		fmt.Fprintf(b, "git-rev = %q\n", e.GitRev)
	}
	if e.LocalPath != "" {
		fmt.Fprintf(b, "local-path = %q\n", e.LocalPath)
	}
	if e.WrapperSHA256 != "" {
		fmt.Fprintf(b, "wrapper-sha256 = %q\n", e.WrapperSHA256)
	}
	if len(e.Capabilities) > 0 {
		b.WriteString("capabilities = [")
		for i, c := range e.Capabilities {
			if i > 0 {
				b.WriteString(", ")
			}
			fmt.Fprintf(b, "%q", c)
		}
		b.WriteString("]\n")
	}
	b.WriteString("\n")
}
