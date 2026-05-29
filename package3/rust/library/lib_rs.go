package library

import (
	"fmt"
	"strings"
)

// RenderLibRS renders the src/lib.rs file for a PublicAPI. Output is
// byte-stable and includes:
//
//   - An optional `#![no_std]` attribute (when NoStd is true).
//   - An `extern crate alloc;` line when NoStd is true (the embedded
//     subset's universal opt-in to allocation).
//   - One Rust item per Item, in declaration order.
//
// Items rendered:
//
//   - ItemFn  → `pub fn name(params) -> ret { body }` or, when
//     Extern, `#[no_mangle]\npub extern "C" fn ...`.
//   - ItemStruct → `pub struct Name { fields }` with optional
//     `#[repr(C)]` and `#[derive(...)]` attributes.
//   - ItemEnum → `pub enum Name { variants }` with optional repr /
//     derive attributes.
//
// Doc strings render as `///` comments preceding the item.
func RenderLibRS(api PublicAPI) string {
	var b strings.Builder
	if api.NoStd {
		b.WriteString("#![no_std]\n")
		b.WriteString("\n")
		b.WriteString("extern crate alloc;\n")
		b.WriteString("\n")
	}
	first := true
	for _, it := range api.Items {
		if !first {
			b.WriteString("\n")
		}
		first = false
		switch v := it.(type) {
		case ItemFn:
			writeItemFn(&b, v)
		case ItemStruct:
			writeItemStruct(&b, v)
		case ItemEnum:
			writeItemEnum(&b, v)
		}
	}
	return b.String()
}

func writeDoc(b *strings.Builder, doc string) {
	if doc == "" {
		return
	}
	for _, line := range strings.Split(strings.TrimRight(doc, "\n"), "\n") {
		fmt.Fprintf(b, "/// %s\n", line)
	}
}

func writeDerives(b *strings.Builder, derives []string) {
	if len(derives) == 0 {
		return
	}
	b.WriteString("#[derive(")
	for i, d := range derives {
		if i > 0 {
			b.WriteString(", ")
		}
		b.WriteString(d)
	}
	b.WriteString(")]\n")
}

func writeItemFn(b *strings.Builder, f ItemFn) {
	writeDoc(b, f.Doc)
	if f.Extern {
		b.WriteString("#[no_mangle]\npub extern \"C\" fn ")
	} else {
		b.WriteString("pub fn ")
	}
	b.WriteString(f.Name)
	b.WriteString("(")
	for i, p := range f.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(b, "%s: %s", p.Name, p.Type)
	}
	b.WriteString(")")
	if f.Return != "" && f.Return != "()" {
		fmt.Fprintf(b, " -> %s", f.Return)
	}
	if f.Extern && strings.TrimSpace(f.Body) == "" {
		// Extern declarations may omit a body; render as a stub
		// returning unsafe { core::mem::zeroed() } so the crate
		// still links. Callers typically provide a Body though.
		b.WriteString(" {\n    unsafe { core::mem::zeroed() }\n}\n")
		return
	}
	b.WriteString(" {\n")
	body := strings.TrimRight(f.Body, "\n")
	for _, line := range strings.Split(body, "\n") {
		if line == "" {
			b.WriteString("\n")
			continue
		}
		fmt.Fprintf(b, "    %s\n", line)
	}
	b.WriteString("}\n")
}

func writeItemStruct(b *strings.Builder, s ItemStruct) {
	writeDoc(b, s.Doc)
	if s.ReprC {
		b.WriteString("#[repr(C)]\n")
	}
	writeDerives(b, s.Derives)
	fmt.Fprintf(b, "pub struct %s {\n", s.Name)
	for _, f := range s.Fields {
		writeDoc(b, f.Doc)
		vis := "pub "
		if !f.Pub {
			vis = ""
		}
		fmt.Fprintf(b, "    %s%s: %s,\n", vis, f.Name, f.Type)
	}
	b.WriteString("}\n")
}

func writeItemEnum(b *strings.Builder, e ItemEnum) {
	writeDoc(b, e.Doc)
	if e.ReprC {
		b.WriteString("#[repr(C)]\n")
	}
	writeDerives(b, e.Derives)
	fmt.Fprintf(b, "pub enum %s {\n", e.Name)
	for _, v := range e.Variants {
		writeDoc(b, v.Doc)
		if len(v.Fields) == 0 {
			fmt.Fprintf(b, "    %s,\n", v.Name)
			continue
		}
		fmt.Fprintf(b, "    %s {\n", v.Name)
		for _, f := range v.Fields {
			fmt.Fprintf(b, "        %s: %s,\n", f.Name, f.Type)
		}
		b.WriteString("    },\n")
	}
	b.WriteString("}\n")
}
