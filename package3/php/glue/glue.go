// Package glue emits PHP-side use statements and forwarding stubs that bridge
// the Mochi extern shims to the real Composer autoloaded classes.
//
// Each PHP package processed by the bridge gets a corresponding set of glue
// stubs under vendor/mochi-glue/<vendor>/<package>/. The stubs live in the
// MochiGlue\<PascalVendor>\<PascalPackage>\ namespace prefix (reserved; no
// upstream Packagist package uses MochiGlue\ as a prefix).
//
// The glue stubs wrap real Composer classes and provide stable PHP entry points
// that Mochi-generated PHP code can call without knowing the original FQCN.
// This decouples the generated PHP from the upstream package's namespace.
//
// Usage:
//
//	result, err := glue.Emit(surface, "guzzlehttp", "guzzle")
//	if err != nil { ... }
//	// result.Files maps relative file path -> PHP source text
package glue

import (
	"fmt"
	"strings"

	"mochi/package3/php/reflect"
	"mochi/package3/php/typemap"
)

// EmitResult holds the PHP glue files produced by Emit.
type EmitResult struct {
	// Files maps relative file path to PHP source content.
	// Paths are relative to the glue output directory (e.g. "GuzzleHttpClient.php").
	Files map[string]string
	// Namespace is the PHP namespace for all glue stubs in this result.
	Namespace string
}

// Emit generates PHP glue stubs for a Composer package.
// vendor and pkg are the Composer vendor and package names (e.g. "guzzlehttp", "guzzle").
func Emit(surface *reflect.ReflectionSurface, vendor, pkg string) (*EmitResult, error) {
	ns := mochiGlueNS(vendor, pkg)
	result := &EmitResult{
		Files:     make(map[string]string),
		Namespace: ns,
	}

	for _, cls := range surface.Classes {
		fname, src := emitClassStub(ns, cls)
		result.Files[fname] = src
	}
	for _, iface := range surface.Interfaces {
		fname, src := emitInterfaceStub(ns, iface)
		result.Files[fname] = src
	}
	for _, enum := range surface.Enums {
		fname, src := emitEnumStub(ns, enum)
		result.Files[fname] = src
	}

	return result, nil
}

// mochiGlueNS constructs the MochiGlue namespace for a given vendor/package.
// "guzzlehttp"/"guzzle" -> "MochiGlue\\GuzzleHttp\\Guzzle"
func mochiGlueNS(vendor, pkg string) string {
	return "MochiGlue\\" + pascalCase(vendor) + "\\" + pascalCase(pkg)
}

// emitClassStub generates a PHP forwarding wrapper for a public class.
func emitClassStub(ns string, cls reflect.ClassSurface) (string, string) {
	handle := classHandle(cls.FQCN)
	alias := "_" + handle
	escapedFQCN := escapePHP(cls.FQCN)

	var sb strings.Builder
	writeHeader(&sb, ns)
	fmt.Fprintf(&sb, "use %s as %s;\n\n", escapedFQCN, alias)
	fmt.Fprintf(&sb, "class %s {\n", handle)
	fmt.Fprintf(&sb, "    private %s $_inner;\n\n", alias)
	fmt.Fprintf(&sb, "    public function __construct(%s $inner) {\n", alias)
	fmt.Fprintf(&sb, "        $this->_inner = $inner;\n")
	fmt.Fprintf(&sb, "    }\n\n")

	for _, m := range cls.Methods {
		if strings.HasPrefix(m.Name, "__") {
			continue
		}
		if m.Static {
			writeStaticForwarder(&sb, m, alias)
		} else {
			writeInstanceForwarder(&sb, m)
		}
	}

	fmt.Fprintf(&sb, "}\n")
	return handle + ".php", sb.String()
}

// emitInterfaceStub generates a PHP comment/alias stub for an interface.
func emitInterfaceStub(ns string, iface reflect.InterfaceSurface) (string, string) {
	handle := classHandle(iface.FQCN)
	escapedFQCN := escapePHP(iface.FQCN)

	var sb strings.Builder
	writeHeader(&sb, ns)
	fmt.Fprintf(&sb, "// Interface binding: %s\n", escapedFQCN)
	fmt.Fprintf(&sb, "// Concrete implementations are wrapped by their own stubs.\n\n")
	fmt.Fprintf(&sb, "use %s as _%s;\n", escapedFQCN, handle)
	return handle + ".php", sb.String()
}

// emitEnumStub generates a PHP helper class for an enum.
func emitEnumStub(ns string, enum reflect.EnumSurface) (string, string) {
	handle := classHandle(enum.FQCN)
	escapedFQCN := escapePHP(enum.FQCN)

	var sb strings.Builder
	writeHeader(&sb, ns)
	fmt.Fprintf(&sb, "use %s as _%s;\n\n", escapedFQCN, handle)
	fmt.Fprintf(&sb, "class %s {\n", handle)

	if enum.BackingType != "" {
		_, skip, _ := typemap.Map(enum.BackingType, false, typemap.DirectionIn)
		if skip == 0 {
			fmt.Fprintf(&sb, "    public static function fromValue(mixed $value): _%s {\n", handle)
			fmt.Fprintf(&sb, "        return _%s::from($value);\n", handle)
			fmt.Fprintf(&sb, "    }\n\n")
		}
	}

	for _, c := range enum.Cases {
		fmt.Fprintf(&sb, "    public static function case%s(): _%s {\n", c.Name, handle)
		fmt.Fprintf(&sb, "        return _%s::%s;\n", handle, c.Name)
		fmt.Fprintf(&sb, "    }\n\n")
	}

	fmt.Fprintf(&sb, "}\n")
	return handle + ".php", sb.String()
}

// writeHeader writes the PHP file header with namespace declaration.
func writeHeader(sb *strings.Builder, ns string) {
	fmt.Fprintf(sb, "<?php\n")
	fmt.Fprintf(sb, "declare(strict_types=1);\n")
	fmt.Fprintf(sb, "namespace %s;\n\n", ns)
}

// writeInstanceForwarder writes a PHP instance method delegating to $_inner.
func writeInstanceForwarder(sb *strings.Builder, m reflect.MethodSurface) {
	params := phpParamList(m.Parameters)
	args := phpArgList(m.Parameters)
	retHint := phpReturnHint(m.ReturnType, m.Nullable)

	if retHint != "" {
		fmt.Fprintf(sb, "    public function %s(%s): %s {\n", m.Name, params, retHint)
	} else {
		fmt.Fprintf(sb, "    public function %s(%s) {\n", m.Name, params)
	}
	if m.ReturnType == "void" || m.ReturnType == "" {
		fmt.Fprintf(sb, "        $this->_inner->%s(%s);\n", m.Name, args)
	} else {
		fmt.Fprintf(sb, "        return $this->_inner->%s(%s);\n", m.Name, args)
	}
	fmt.Fprintf(sb, "    }\n\n")
}

// writeStaticForwarder writes a PHP static method delegating to the original class.
func writeStaticForwarder(sb *strings.Builder, m reflect.MethodSurface, alias string) {
	params := phpParamList(m.Parameters)
	args := phpArgList(m.Parameters)
	retHint := phpReturnHint(m.ReturnType, m.Nullable)

	if retHint != "" {
		fmt.Fprintf(sb, "    public static function %s(%s): %s {\n", m.Name, params, retHint)
	} else {
		fmt.Fprintf(sb, "    public static function %s(%s) {\n", m.Name, params)
	}
	if m.ReturnType == "void" || m.ReturnType == "" {
		fmt.Fprintf(sb, "        %s::%s(%s);\n", alias, m.Name, args)
	} else {
		fmt.Fprintf(sb, "        return %s::%s(%s);\n", alias, m.Name, args)
	}
	fmt.Fprintf(sb, "    }\n\n")
}

// phpParamList builds a PHP parameter list string.
func phpParamList(params []reflect.ParameterSurface) string {
	var parts []string
	for _, p := range params {
		var part string
		switch {
		case p.Variadic && p.Type != "":
			part = fmt.Sprintf("%s ...$%s", p.Type, p.Name)
		case p.Variadic:
			part = fmt.Sprintf("...$%s", p.Name)
		case p.Nullable && p.Type != "":
			part = fmt.Sprintf("?%s $%s", p.Type, p.Name)
		case p.Type != "":
			part = fmt.Sprintf("%s $%s", p.Type, p.Name)
		default:
			part = fmt.Sprintf("$%s", p.Name)
		}
		parts = append(parts, part)
	}
	return strings.Join(parts, ", ")
}

// phpArgList builds a PHP argument list (just $paramName references).
func phpArgList(params []reflect.ParameterSurface) string {
	var parts []string
	for _, p := range params {
		if p.Variadic {
			parts = append(parts, "...$"+p.Name)
		} else {
			parts = append(parts, "$"+p.Name)
		}
	}
	return strings.Join(parts, ", ")
}

// phpReturnHint returns the PHP return type hint string for a method.
func phpReturnHint(phpType string, nullable bool) string {
	if phpType == "" {
		return ""
	}
	if nullable {
		return "?" + phpType
	}
	return phpType
}

// escapePHP ensures a FQCN uses single backslash separators (as PHP requires).
func escapePHP(fqcn string) string {
	fqcn = strings.TrimPrefix(fqcn, "\\")
	return fqcn
}

// classHandle converts a PHP FQCN to a PascalCase handle name.
func classHandle(fqcn string) string {
	fqcn = strings.TrimPrefix(fqcn, "\\")
	parts := strings.Split(fqcn, "\\")
	var sb strings.Builder
	for _, p := range parts {
		if p == "" {
			continue
		}
		sb.WriteString(strings.ToUpper(p[:1]))
		sb.WriteString(p[1:])
	}
	name := sb.String()
	if name == "" {
		return "OpaqueHandle"
	}
	return name
}

// pascalCase converts a lowercase package name to PascalCase.
// "guzzlehttp" -> "Guzzlehttp", "my-package" -> "MyPackage"
func pascalCase(s string) string {
	parts := strings.FieldsFunc(s, func(r rune) bool {
		return r == '-' || r == '_'
	})
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		sb.WriteString(strings.ToUpper(p[:1]))
		sb.WriteString(p[1:])
	}
	return sb.String()
}
