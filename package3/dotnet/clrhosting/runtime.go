// Package clrhosting generates Go source code for the CLR hosting API bridge.
// At runtime, the generated code uses the hostfxr library to embed .NET in the
// Mochi process and load the synthesised C# shim assembly.
//
// The .NET Runtime Hosting API (stable since .NET 5 SDK, GA since .NET 6)
// provides three key functions:
//
//	hostfxr_initialize_for_runtime_config -- load the runtime from a .runtimeconfig.json
//	hostfxr_get_runtime_delegate -- get a function-pointer factory
//	load_assembly_and_get_function_pointer -- load an assembly and get a delegate
//
// The bridge generates a Go file per imported package with cgo declarations
// that call through the synthesised C# shim's [UnmanagedCallersOnly] exports.
package clrhosting

import (
	"fmt"
	"strings"
	"unicode"

	"mochi/package3/dotnet/shimgen"
	"mochi/package3/dotnet/typemap"
)

// Config holds the parameters needed to generate the CLR hosting bridge code.
type Config struct {
	// Package is the NuGet package id (e.g. "Newtonsoft.Json").
	Package string
	// PackageVersion is the resolved version string.
	PackageVersion string
	// TargetFramework is the TFM (e.g. "net8.0").
	TargetFramework string
	// ShimAssemblyName is the C# assembly name (e.g. "MochiShim.NewtonsoftJson").
	ShimAssemblyName string
	// ShimDir is the relative path to the shim project directory.
	ShimDir string
	// EntryPoints lists the C symbol entry points to bind.
	EntryPoints []string
	// MarshalFreeEntry is the entry point for the free function (always "mochi_marshal_free").
	MarshalFreeEntry string
}

// EmittedBridge is the bundle of generated files for one CLR hosting bridge.
type EmittedBridge struct {
	// GoFile is the Go source with cgo bindings.
	GoFile string
	// RuntimeConfigJSON is the .runtimeconfig.json content.
	RuntimeConfigJSON string
}

// MethodBinding describes one C# shim method to bind via cgo.
type MethodBinding struct {
	// EntryPoint is the C symbol name (e.g. "mochi_newtonsoftjson_JsonConvert_SerializeObject").
	EntryPoint string
	// CReturnType is the C return type (e.g. "MochiString", "long long", "void").
	CReturnType string
	// CParams is the C parameter declaration list (e.g. "MochiString s, long long n").
	CParams string
	// GoFuncName is the Go function name for the binding (e.g. "CallJsonConvert_SerializeObject").
	GoFuncName string
	// GoParams is the Go parameter list mirroring CParams.
	GoParams string
	// GoReturnType is the Go return type (e.g. "MochiString", "int64").
	GoReturnType string
}

// RuntimeConfigJSON generates the .runtimeconfig.json content for the shim assembly.
// This tells the CLR which .NET runtime version to load.
func RuntimeConfigJSON(cfg Config) string {
	version := parseTFMVersion(cfg.TargetFramework)
	return fmt.Sprintf(`{
  "runtimeOptions": {
    "tfm": %q,
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": %q
    }
  }
}`, cfg.TargetFramework, version)
}

// parseTFMVersion extracts the dotted version from a TFM string.
// "net8.0" -> "8.0.0", "net6.0" -> "6.0.0", etc.
func parseTFMVersion(tfm string) string {
	s := tfm
	// Strip leading non-digits.
	for len(s) > 0 && !isDigit(s[0]) {
		s = s[1:]
	}
	if s == "" {
		return "8.0.0"
	}
	// s is now e.g. "8.0" or "9.0"
	// Ensure three parts.
	parts := strings.Split(s, ".")
	for len(parts) < 3 {
		parts = append(parts, "0")
	}
	return strings.Join(parts[:3], ".")
}

func isDigit(b byte) bool { return b >= '0' && b <= '9' }

// CgoHeader generates the cgo comment block for the hosting bridge Go file.
// It emits the #cgo directives and C declarations for each entry point.
func CgoHeader(cfg Config, methods []MethodBinding) string {
	var b strings.Builder
	b.WriteString("/*\n")
	b.WriteString("#cgo LDFLAGS: -ldl\n")
	b.WriteString("#include <stdint.h>\n")
	b.WriteString("#include <stdlib.h>\n")
	b.WriteString("#include <string.h>\n")
	b.WriteString("\n")
	b.WriteString("typedef struct { unsigned char* Ptr; int Len; } MochiString;\n")
	b.WriteString("\n")
	// Marshal free is always present.
	freeEntry := cfg.MarshalFreeEntry
	if freeEntry == "" {
		freeEntry = "mochi_marshal_free"
	}
	fmt.Fprintf(&b, "extern void %s(intptr_t ptr);\n", freeEntry)
	for _, m := range methods {
		if m.CParams == "" {
			fmt.Fprintf(&b, "extern %s %s(void);\n", m.CReturnType, m.EntryPoint)
		} else {
			fmt.Fprintf(&b, "extern %s %s(%s);\n", m.CReturnType, m.EntryPoint, m.CParams)
		}
	}
	b.WriteString("*/\n")
	b.WriteString("import \"C\"")
	return b.String()
}

// EmitGoFile generates the Go source file that provides the CLR hosting bridge
// for one shim assembly. The file includes:
//   - cgo import with the hostfxr initialization stubs
//   - A package-level init() that loads the shim assembly
//   - One exported Go function per entry point that delegates to the C symbol
func EmitGoFile(pkg string, cfg Config, methods []MethodBinding) string {
	pkgIdent := safeGoIdent(cfg.Package)
	packageName := "dotnet_bridge_" + strings.ToLower(pkgIdent)

	var b strings.Builder
	fmt.Fprintf(&b, "// Code generated by mochi/package3/dotnet/clrhosting; do not edit.\n")
	fmt.Fprintf(&b, "// CLR hosting bridge for %s %s\n", cfg.Package, cfg.PackageVersion)
	fmt.Fprintf(&b, "package %s\n", packageName)
	b.WriteString("\n")
	b.WriteString(CgoHeader(cfg, methods))
	b.WriteString("\n")
	b.WriteString("import \"unsafe\"\n")
	b.WriteString("\n")
	b.WriteString("// MochiString is the UTF-8 string type shared between Go and C#.\n")
	b.WriteString("type MochiString = C.MochiString\n")
	b.WriteString("\n")
	b.WriteString("// Ensure unsafe is used.\n")
	b.WriteString("var _ = unsafe.Pointer(nil)\n")
	b.WriteString("\n")

	shimDLL := cfg.ShimDir + "/" + cfg.ShimAssemblyName + ".dll"
	shimCfg := cfg.ShimDir + "/" + cfg.ShimAssemblyName + ".runtimeconfig.json"

	b.WriteString("func init() {\n")
	fmt.Fprintf(&b, "\tloadShim(%q,\n\t         %q)\n", shimDLL, shimCfg)
	b.WriteString("}\n")
	b.WriteString("\n")
	b.WriteString("// loadShim is provided by the mochi runtime via a build tag.\n")
	b.WriteString("func loadShim(assemblyPath, runtimeConfigPath string)\n")
	b.WriteString("\n")

	for _, m := range methods {
		fmt.Fprintf(&b, "// %s wraps the C# shim entry point.\n", m.GoFuncName)
		if m.GoParams == "" {
			fmt.Fprintf(&b, "func %s() %s {\n", m.GoFuncName, m.GoReturnType)
		} else {
			fmt.Fprintf(&b, "func %s(%s) %s {\n", m.GoFuncName, m.GoParams, m.GoReturnType)
		}
		args := buildCCallArgs(m)
		if m.GoReturnType == "" || m.GoReturnType == "void" {
			if args == "" {
				fmt.Fprintf(&b, "\tC.%s()\n", m.EntryPoint)
			} else {
				fmt.Fprintf(&b, "\tC.%s(%s)\n", m.EntryPoint, args)
			}
		} else {
			if args == "" {
				fmt.Fprintf(&b, "\treturn C.%s()\n", m.EntryPoint)
			} else {
				fmt.Fprintf(&b, "\treturn C.%s(%s)\n", m.EntryPoint, args)
			}
		}
		b.WriteString("}\n")
		b.WriteString("\n")
	}

	return b.String()
}

// buildCCallArgs constructs the argument list for a C function call from a MethodBinding.
func buildCCallArgs(m MethodBinding) string {
	if m.GoParams == "" {
		return ""
	}
	// Parse the Go params "name type, name type" -> extract just "name, name".
	parts := strings.Split(m.GoParams, ",")
	args := make([]string, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		fields := strings.Fields(p)
		if len(fields) >= 1 {
			args = append(args, fields[0])
		}
	}
	return strings.Join(args, ", ")
}

// BindingsFromShim derives a []MethodBinding from a shimgen.Shim.
// It maps each ShimMethod to a MethodBinding by translating typemap.Mapping
// to C types.
func BindingsFromShim(s *shimgen.Shim) []MethodBinding {
	if s == nil {
		return nil
	}
	out := make([]MethodBinding, 0, len(s.Methods))
	for i := range s.Methods {
		m := &s.Methods[i]
		b := bindingFromMethod(m)
		out = append(out, b)
	}
	return out
}

// bindingFromMethod converts a single ShimMethod to a MethodBinding.
func bindingFromMethod(m *shimgen.ShimMethod) MethodBinding {
	// Build C params.
	cParamParts := make([]string, 0, len(m.Params))
	goParamParts := make([]string, 0, len(m.Params))
	for _, p := range m.Params {
		ct := CTypeFor(&p.Mapping)
		cParamParts = append(cParamParts, ct+" "+p.Name)
		gt := cTypeToGoType(ct)
		goParamParts = append(goParamParts, p.Name+" "+gt)
	}

	cRet := "void"
	goRet := ""
	if m.Return != nil {
		cRet = CTypeFor(m.Return)
		goRet = cTypeToGoType(cRet)
	}

	goFuncName := "Call" + m.CSMethodName

	return MethodBinding{
		EntryPoint:   m.EntryPoint,
		CReturnType:  cRet,
		CParams:      strings.Join(cParamParts, ", "),
		GoFuncName:   goFuncName,
		GoParams:     strings.Join(goParamParts, ", "),
		GoReturnType: goRet,
	}
}

// cTypeToGoType maps a C type string to the corresponding Go/cgo type.
func cTypeToGoType(ct string) string {
	switch ct {
	case "void":
		return ""
	case "uint8_t":
		return "C.uint8_t"
	case "int32_t":
		return "C.int32_t"
	case "int64_t":
		return "C.int64_t"
	case "uint32_t":
		return "C.uint32_t"
	case "uint64_t":
		return "C.uint64_t"
	case "float":
		return "C.float"
	case "double":
		return "C.double"
	case "uint16_t":
		return "C.uint16_t"
	case "MochiString":
		return "MochiString"
	case "intptr_t":
		return "C.intptr_t"
	default:
		return "C." + ct
	}
}

// CTypeFor returns the C type string for a typemap.Mapping, used in cgo declarations.
func CTypeFor(m *typemap.Mapping) string {
	if m == nil {
		return "void"
	}
	switch m.Kind {
	case typemap.KindBool:
		return "uint8_t"
	case typemap.KindByte:
		return "uint8_t"
	case typemap.KindInt:
		return "int32_t"
	case typemap.KindInt64:
		return "int64_t"
	case typemap.KindUInt:
		return "uint32_t"
	case typemap.KindUInt64:
		return "uint64_t"
	case typemap.KindFloat:
		return "float"
	case typemap.KindFloat64:
		return "double"
	case typemap.KindChar:
		return "uint16_t"
	case typemap.KindString:
		return "MochiString"
	case typemap.KindBytes:
		return "MochiString"
	case typemap.KindUnit:
		return "void"
	case typemap.KindHandle:
		return "intptr_t"
	case typemap.KindRecord:
		return "intptr_t"
	case typemap.KindList:
		return "intptr_t"
	case typemap.KindMap:
		return "intptr_t"
	case typemap.KindSet:
		return "intptr_t"
	case typemap.KindEnum:
		return "int32_t"
	case typemap.KindOption:
		return "intptr_t"
	case typemap.KindTuple:
		return "intptr_t"
	case typemap.KindTask:
		// Already unwrapped by shimgen; use the inner type.
		if m.Inner != nil {
			return CTypeFor(m.Inner)
		}
		return "void"
	default:
		return "intptr_t"
	}
}

// safeGoIdent converts a NuGet package id to a Go-safe identifier segment.
// "Newtonsoft.Json" -> "newtonsoftjson"
func safeGoIdent(pkg string) string {
	var b strings.Builder
	for _, r := range pkg {
		switch {
		case r == '.', r == '-':
			// drop separators
		case unicode.IsLetter(r) || unicode.IsDigit(r):
			b.WriteRune(unicode.ToLower(r))
		default:
			b.WriteRune('_')
		}
	}
	return b.String()
}
