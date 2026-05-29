package wrapper

import (
	"fmt"
	"sort"
	"strings"

	"mochi/package3/rust/asyncbridge"
	"mochi/package3/rust/embedded"
	"mochi/package3/rust/errors"
	"mochi/package3/rust/typemap"
)

// EmitLibRS renders src/lib.rs for the Crate. The output is
// deterministic: the runtime prologue is verbatim, functions follow
// in walker order (the order Synth produced), and identifiers come
// straight from SynthFn fields.
//
// The bodies marshal scalar inputs and scalar/string returns
// directly. Other surface kinds (list, map, option, result, tuple,
// struct, enum, handle, bytes) are emitted with a `todo!()` body
// plus a `// TODO: marshal` comment. Phase 4.1 will fill those in;
// the gate here is structural correctness.
func EmitLibRS(c *Crate) string {
	var b strings.Builder
	b.WriteString(embedded.LibRSHeader(c.Profile))
	b.WriteString(runtimePrologue)
	b.WriteString("\n")
	b.WriteString(fmt.Sprintf("// upstream crate: %s %s\n", c.Upstream, c.UpstreamVersion))
	b.WriteString("\n")
	if c.HasAsync() {
		b.WriteString("pub mod mochi_rt;\n")
		b.WriteString("\n")
	}
	for _, fn := range c.Functions {
		emitFn(&b, &fn)
		b.WriteString("\n")
	}
	return b.String()
}

// EmitMochiRT renders src/mochi_rt.rs when the crate has at least one
// async fn. Callers write the returned text to
// `<wrapper>/src/mochi_rt.rs`. Returns empty when no async fns are
// present.
func EmitMochiRT(c *Crate) string {
	if !c.HasAsync() {
		return ""
	}
	return asyncbridge.RuntimeModule(c.AsyncFlavor)
}

func emitFn(b *strings.Builder, fn *SynthFn) {
	b.WriteString("#[no_mangle]\n")
	b.WriteString("pub unsafe extern \"C\" fn ")
	b.WriteString(fn.ExternName)
	b.WriteString("(")
	parts := make([]string, 0, len(fn.Params))
	for _, p := range fn.Params {
		parts = append(parts, emitParamDecl(p))
	}
	b.WriteString(strings.Join(parts, ", "))
	b.WriteString(")")
	if fn.Return != nil {
		b.WriteString(" -> ")
		b.WriteString(fn.Return.FFIRepr())
	}
	b.WriteString(" {\n")
	emitBody(b, fn)
	b.WriteString("}\n")
}

func emitParamDecl(p SynthParam) string {
	switch p.Mapping.Kind {
	case typemap.KindString, typemap.KindBytes:
		// Strings and byte slices are passed as a MochiString /
		// MochiSlice by value to keep ABI noise low.
		return fmt.Sprintf("%s: %s", p.Name, p.Mapping.FFIRepr())
	default:
		return fmt.Sprintf("%s: %s", p.Name, p.Mapping.FFIRepr())
	}
}

func emitBody(b *strings.Builder, fn *SynthFn) {
	allScalar := true
	for _, p := range fn.Params {
		if !p.Mapping.IsScalar() && p.Mapping.Kind != typemap.KindString {
			allScalar = false
			break
		}
	}
	retSimple := fn.Return == nil || fn.Return.IsScalar() || fn.Return.Kind == typemap.KindString
	if !allScalar || !retSimple {
		b.WriteString("    // TODO: marshal compound input/output (phase 4.1).\n")
		b.WriteString("    todo!()\n")
		return
	}
	// Marshal each string input into a &str binding.
	args := make([]string, 0, len(fn.Params))
	for _, p := range fn.Params {
		switch p.Mapping.Kind {
		case typemap.KindString:
			b.WriteString(fmt.Sprintf("    let %s: &str = unsafe { mochi_string_to_str(%s) };\n", p.Name, p.Name))
			args = append(args, p.Name)
		default:
			args = append(args, p.Name)
		}
	}
	var call string
	if fn.IsAsync {
		call = asyncbridge.FnBody(fn.UpstreamPath, args)
	} else {
		call = fmt.Sprintf("%s(%s)", fn.UpstreamPath, strings.Join(args, ", "))
	}
	if fn.Return == nil {
		b.WriteString(fmt.Sprintf("    %s;\n", call))
		return
	}
	switch fn.Return.Kind {
	case typemap.KindString:
		b.WriteString(fmt.Sprintf("    let __ret = %s;\n", call))
		b.WriteString("    mochi_string_from_owned(__ret)\n")
	default:
		b.WriteString(fmt.Sprintf("    %s\n", call))
	}
}

// EmitCargoTOML renders the wrapper crate's Cargo.toml. The crate
// is built as cdylib + rlib so it can be linked into the Mochi
// runtime (cdylib) and rebuilt against other Rust callers (rlib).
//
// When the crate has any `async fn` (HasAsync() returns true), the
// `[dependencies]` block additionally carries tokio (pinned to
// asyncbridge.MinTokioVersion with the flavor's feature set) and
// once_cell (for the runtime singleton).
func EmitCargoTOML(c *Crate) string {
	var b strings.Builder
	b.WriteString("[package]\n")
	b.WriteString(fmt.Sprintf("name = %q\n", c.Name))
	b.WriteString("version = \"0.1.0\"\n")
	b.WriteString("edition = \"2021\"\n")
	b.WriteString("publish = false\n")
	b.WriteString("\n")
	b.WriteString("[lib]\n")
	b.WriteString(fmt.Sprintf("name = %q\n", c.Name))
	b.WriteString("crate-type = [\"cdylib\", \"rlib\"]\n")
	b.WriteString("\n")
	b.WriteString("[dependencies]\n")
	b.WriteString(embedded.CargoUpstreamDepRow(c.Profile, c.Upstream, c.UpstreamVersion))
	if c.HasAsync() {
		b.WriteString(asyncbridge.CargoDepRow(c.AsyncFlavor))
		b.WriteString("\n")
		b.WriteString(asyncbridge.OnceCellDepRow())
		b.WriteString("\n")
	}
	return b.String()
}

// EmitSkippedTXT renders SKIPPED.txt. The output groups SkipReports
// by Reason for readability, with each group sorted by ItemPath.
// Within each group, every report is rendered via SkipReport.String.
func EmitSkippedTXT(c *Crate) string {
	if len(c.Skipped) == 0 {
		return "# No skipped items.\n"
	}
	groups := map[errors.SkipReason][]errors.SkipReport{}
	for _, s := range c.Skipped {
		groups[s.Reason] = append(groups[s.Reason], s)
	}
	keys := make([]errors.SkipReason, 0, len(groups))
	for k := range groups {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool { return keys[i].String() < keys[j].String() })
	var b strings.Builder
	for _, k := range keys {
		entries := groups[k]
		sort.Slice(entries, func(i, j int) bool { return entries[i].ItemPath < entries[j].ItemPath })
		b.WriteString("# ")
		b.WriteString(k.String())
		b.WriteString("\n")
		for _, e := range entries {
			b.WriteString(e.String())
		}
		b.WriteString("\n")
	}
	return b.String()
}
