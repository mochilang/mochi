package library

import (
	"fmt"
	"strings"
)

// RenderCHeader renders a cbindgen-compatible C header for the
// `extern "C"` and repr(C) surface of a PublicAPI. The output is a
// freestanding C89 / C99 header that downstream non-Rust consumers
// can #include to link against the rendered crate's cdylib.
//
// The header follows the cbindgen default layout:
//
//   #ifndef <CRATE>_H
//   #define <CRATE>_H
//
//   #include <stdint.h>
//   #include <stddef.h>
//
//   #ifdef __cplusplus
//   extern "C" {
//   #endif
//
//   ... repr(C) typedefs and extern fn prototypes ...
//
//   #ifdef __cplusplus
//   } // extern "C"
//   #endif
//
//   #endif // <CRATE>_H
//
// Non-extern functions and non-repr-C types are omitted. The header
// is a strict subset of the Rust surface, not a 1:1 mirror.
func RenderCHeader(api PublicAPI) string {
	var b strings.Builder
	guard := strings.ToUpper(strings.ReplaceAll(headerName(api.CrateName), "_", "_"))
	guard = strings.ToUpper(guard) + "_H"
	fmt.Fprintf(&b, "#ifndef %s\n", guard)
	fmt.Fprintf(&b, "#define %s\n\n", guard)
	b.WriteString("#include <stdint.h>\n")
	b.WriteString("#include <stddef.h>\n\n")
	b.WriteString("#ifdef __cplusplus\n")
	b.WriteString("extern \"C\" {\n")
	b.WriteString("#endif\n\n")

	// Emit repr(C) types first so function signatures can reference
	// them (forward declarations are not required because cbindgen
	// orders types-before-fns by convention).
	for _, it := range api.Items {
		switch v := it.(type) {
		case ItemStruct:
			if v.ReprC {
				writeCStruct(&b, v)
				b.WriteString("\n")
			}
		case ItemEnum:
			if v.ReprC {
				writeCEnum(&b, v)
				b.WriteString("\n")
			}
		}
	}
	for _, it := range api.Items {
		if fn, ok := it.(ItemFn); ok && fn.Extern {
			writeCFnDecl(&b, fn)
			b.WriteString("\n")
		}
	}

	b.WriteString("#ifdef __cplusplus\n")
	b.WriteString("} // extern \"C\"\n")
	b.WriteString("#endif\n\n")
	fmt.Fprintf(&b, "#endif // %s\n", guard)
	return b.String()
}

func writeCStruct(b *strings.Builder, s ItemStruct) {
	if s.Doc != "" {
		writeCDoc(b, s.Doc)
	}
	fmt.Fprintf(b, "typedef struct %s {\n", s.Name)
	for _, f := range s.Fields {
		fmt.Fprintf(b, "    %s %s;\n", rustTypeToC(f.Type), f.Name)
	}
	fmt.Fprintf(b, "} %s;\n", s.Name)
}

func writeCEnum(b *strings.Builder, e ItemEnum) {
	if e.Doc != "" {
		writeCDoc(b, e.Doc)
	}
	// repr(C) enums in cbindgen render as plain C enums when all
	// variants are unit-typed, and as tagged unions otherwise. For
	// MEP-73 phase 9 we only emit the simple form; the tagged-union
	// form is a phase 12 (monomorphisation) concern.
	if !enumIsUnitOnly(e) {
		fmt.Fprintf(b, "/* %s: tagged-union enum; render deferred to phase 12 */\n", e.Name)
		return
	}
	fmt.Fprintf(b, "typedef enum %s {\n", e.Name)
	for i, v := range e.Variants {
		comma := ","
		if i == len(e.Variants)-1 {
			comma = ""
		}
		fmt.Fprintf(b, "    %s_%s%s\n", e.Name, v.Name, comma)
	}
	fmt.Fprintf(b, "} %s;\n", e.Name)
}

func writeCFnDecl(b *strings.Builder, f ItemFn) {
	if f.Doc != "" {
		writeCDoc(b, f.Doc)
	}
	ret := rustTypeToC(f.Return)
	if ret == "" {
		ret = "void"
	}
	fmt.Fprintf(b, "%s %s(", ret, f.Name)
	if len(f.Params) == 0 {
		b.WriteString("void")
	}
	for i, p := range f.Params {
		if i > 0 {
			b.WriteString(", ")
		}
		fmt.Fprintf(b, "%s %s", rustTypeToC(p.Type), p.Name)
	}
	b.WriteString(");\n")
}

func writeCDoc(b *strings.Builder, doc string) {
	b.WriteString("/**\n")
	for _, line := range strings.Split(strings.TrimRight(doc, "\n"), "\n") {
		fmt.Fprintf(b, " * %s\n", line)
	}
	b.WriteString(" */\n")
}

func enumIsUnitOnly(e ItemEnum) bool {
	for _, v := range e.Variants {
		if len(v.Fields) > 0 {
			return false
		}
	}
	return true
}

// rustTypeToC projects the small closed set of repr(C)-safe Rust
// types into their canonical C spellings. Unknown types render
// verbatim (the caller is expected to use the exact C type when the
// Rust type is unusual, e.g. a typedef'd repr(C) struct name).
func rustTypeToC(rt string) string {
	switch strings.TrimSpace(rt) {
	case "()", "":
		return ""
	case "i8":
		return "int8_t"
	case "u8":
		return "uint8_t"
	case "i16":
		return "int16_t"
	case "u16":
		return "uint16_t"
	case "i32":
		return "int32_t"
	case "u32":
		return "uint32_t"
	case "i64":
		return "int64_t"
	case "u64":
		return "uint64_t"
	case "isize":
		return "intptr_t"
	case "usize":
		return "size_t"
	case "f32":
		return "float"
	case "f64":
		return "double"
	case "bool":
		return "bool"
	case "c_char":
		return "char"
	case "*const c_char":
		return "const char*"
	case "*mut c_char":
		return "char*"
	}
	// Pointer types: *const T → const T*, *mut T → T*.
	if strings.HasPrefix(rt, "*const ") {
		inner := rustTypeToC(strings.TrimPrefix(rt, "*const "))
		return "const " + inner + "*"
	}
	if strings.HasPrefix(rt, "*mut ") {
		inner := rustTypeToC(strings.TrimPrefix(rt, "*mut "))
		return inner + "*"
	}
	return rt
}
