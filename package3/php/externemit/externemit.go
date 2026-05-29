// Package externemit synthesises Mochi extern fn / extern type declarations
// from a translated PHP surface document (package3/php/reflect.ReflectionSurface).
//
// For each public class, interface, enum, and top-level function in the surface,
// the emitter produces:
//
//   - An `extern type` declaration for every class/interface/enum handle.
//   - An `extern fn` declaration for every public method and top-level function
//     whose parameter and return types are all in the closed type table.
//
// Items that cannot be translated (mixed, untyped array, magic methods, variadic
// parameters, self/static/parent return types, etc.) are collected as SkipReport
// entries. The caller may write these to a SKIPPED.txt file alongside the shim.
//
// Usage:
//
//	result, err := externemit.Emit(surface)
//	if err != nil { ... }
//	// result.MochiSource is the .mochi shim file text
//	// result.Skips contains the untranslatable items
package externemit

import (
	"fmt"
	"strings"
	"unicode"

	bridgeerrors "mochi/package3/php/errors"
	"mochi/package3/php/reflect"
	"mochi/package3/php/typemap"
)

// EmitResult holds the output of an Emit call.
type EmitResult struct {
	// MochiSource is the complete .mochi shim file content, ready to write to disk.
	MochiSource string
	// Skips is the list of items that could not be translated.
	Skips []bridgeerrors.SkipReport
}

// Emit translates a PHP reflection surface into Mochi extern declarations.
func Emit(surface *reflect.ReflectionSurface) (*EmitResult, error) {
	e := &emitter{pkg: surface.PackageName}

	for _, cls := range surface.Classes {
		e.emitClass(cls)
	}
	for _, iface := range surface.Interfaces {
		e.emitInterface(iface)
	}
	for _, enum := range surface.Enums {
		e.emitEnum(enum)
	}
	for _, fn := range surface.Functions {
		e.emitFunction(fn)
	}

	return &EmitResult{
		MochiSource: e.render(),
		Skips:       e.skips,
	}, nil
}

// FormatSKIPPED returns the text of a SKIPPED.txt file for the given skip list.
func FormatSKIPPED(pkg string, skips []bridgeerrors.SkipReport) string {
	if len(skips) == 0 {
		return ""
	}
	var sb strings.Builder
	fmt.Fprintf(&sb, "# SKIPPED items for package %q\n", pkg)
	fmt.Fprintf(&sb, "# These items could not be automatically translated to Mochi extern declarations.\n")
	fmt.Fprintf(&sb, "# Add hand-written extern fn overrides in your Mochi source to use them.\n\n")
	for _, s := range skips {
		fmt.Fprintf(&sb, "%s", s.String())
	}
	return sb.String()
}

// emitter accumulates extern declarations and skip reports.
type emitter struct {
	pkg   string
	lines []string
	skips []bridgeerrors.SkipReport
}

func (e *emitter) addLine(s string) {
	e.lines = append(e.lines, s)
}

func (e *emitter) addSkip(path string, reason bridgeerrors.SkipReason, detail string) {
	e.skips = append(e.skips, bridgeerrors.SkipReport{
		ItemPath: path,
		Reason:   reason,
		Detail:   detail,
	})
}

func (e *emitter) render() string {
	if len(e.lines) == 0 {
		return ""
	}
	return strings.Join(e.lines, "\n") + "\n"
}

// emitClass emits extern type + extern fn declarations for a public class.
func (e *emitter) emitClass(cls reflect.ClassSurface) {
	handle := classHandle(cls.FQCN)
	e.addLine(fmt.Sprintf("extern type %s", handle))
	e.addLine("")

	for _, m := range cls.Methods {
		path := fmt.Sprintf("%s::%s::%s", e.pkg, cls.FQCN, m.Name)
		e.emitMethod(path, handle, m)
	}
}

// emitInterface emits extern type + extern fn declarations for an interface.
func (e *emitter) emitInterface(iface reflect.InterfaceSurface) {
	handle := classHandle(iface.FQCN)
	e.addLine(fmt.Sprintf("extern type %s", handle))
	e.addLine("")

	for _, m := range iface.Methods {
		path := fmt.Sprintf("%s::%s::%s", e.pkg, iface.FQCN, m.Name)
		e.emitMethod(path, handle, m)
	}
}

// emitEnum emits extern type for a PHP enum, plus a fromValue constructor for
// backed enums and any public methods.
func (e *emitter) emitEnum(enum reflect.EnumSurface) {
	handle := classHandle(enum.FQCN)
	e.addLine(fmt.Sprintf("extern type %s", handle))
	e.addLine("")

	if enum.BackingType != "" {
		backM, skip, detail := typemap.Map(enum.BackingType, false, typemap.DirectionIn)
		if skip != 0 {
			e.addSkip(e.pkg+"::"+enum.FQCN+"::fromValue", skip, detail)
		} else {
			fnName := toSnakeCase(handle) + "_from_value"
			e.addLine(fmt.Sprintf("extern fn %s(value: %s) -> %s", fnName, backM.MochiType, handle))
			e.addLine("")
		}
	}

	for _, m := range enum.Methods {
		path := fmt.Sprintf("%s::%s::%s", e.pkg, enum.FQCN, m.Name)
		e.emitMethod(path, handle, m)
	}
}

// emitFunction emits an extern fn for a top-level PHP function.
func (e *emitter) emitFunction(fn reflect.FunctionSurface) {
	path := e.pkg + "::" + fn.Name

	retType, skip, detail := mapReturnType(fn.ReturnType, fn.Nullable)
	if skip != 0 {
		e.addSkip(path+" (return)", skip, detail)
		return
	}

	params, paramSkip, paramDetail := mapParamList(path, fn.Parameters)
	if paramSkip != 0 {
		e.addSkip(path, paramSkip, paramDetail)
		return
	}

	fnName := toSnakeCase(strings.ReplaceAll(fn.Name, "\\", "_"))
	e.addLine(fmt.Sprintf("extern fn %s(%s) -> %s", fnName, params, retType))
	e.addLine("")
}

// emitMethod emits an extern fn for a class/interface/enum method.
func (e *emitter) emitMethod(path, handle string, m reflect.MethodSurface) {
	if strings.HasPrefix(m.Name, "__") {
		e.addSkip(path, bridgeerrors.SkipMagicMethod, "magic method not bridgeable via extern fn")
		return
	}

	retType, skip, detail := mapReturnType(m.ReturnType, m.Nullable)
	if skip != 0 {
		e.addSkip(path+" (return)", skip, detail)
		return
	}

	var paramParts []string
	if !m.Static {
		paramParts = append(paramParts, fmt.Sprintf("self: %s", handle))
	}

	for _, p := range m.Parameters {
		if p.Variadic {
			e.addSkip(path+"::$"+p.Name, bridgeerrors.SkipVararg, "variadic parameter")
			return
		}
		if p.Type == "" {
			e.addSkip(path+"::$"+p.Name, bridgeerrors.SkipMixed, "untyped parameter (implicit mixed)")
			return
		}
		pm, pSkip, pDetail := typemap.Map(p.Type, p.Nullable, typemap.DirectionIn)
		if pSkip != 0 {
			e.addSkip(path+"::$"+p.Name, pSkip, pDetail)
			return
		}
		paramParts = append(paramParts, fmt.Sprintf("%s: %s", safeIdent(p.Name), pm.MochiType))
	}

	params := strings.Join(paramParts, ", ")
	fnName := toSnakeCase(handle) + "_" + toSnakeCase(m.Name)
	e.addLine(fmt.Sprintf("extern fn %s(%s) -> %s", fnName, params, retType))
	e.addLine("")
}

// mapReturnType maps a PHP return type string to a Mochi return type string.
// An empty return type (no annotation) is treated as "unit".
func mapReturnType(phpType string, nullable bool) (string, bridgeerrors.SkipReason, string) {
	if phpType == "" {
		return "unit", 0, ""
	}
	m, skip, detail := typemap.Map(phpType, nullable, typemap.DirectionOut)
	if skip != 0 {
		return "", skip, detail
	}
	return m.MochiType, 0, ""
}

// mapParamList maps a slice of ParameterSurface to a Mochi param string.
func mapParamList(_ string, params []reflect.ParameterSurface) (string, bridgeerrors.SkipReason, string) {
	var parts []string
	for _, p := range params {
		if p.Variadic {
			return "", bridgeerrors.SkipVararg, "variadic parameter $" + p.Name
		}
		if p.Type == "" {
			return "", bridgeerrors.SkipMixed, fmt.Sprintf("untyped parameter $%s (implicit mixed)", p.Name)
		}
		pm, skip, detail := typemap.Map(p.Type, p.Nullable, typemap.DirectionIn)
		if skip != 0 {
			return "", skip, fmt.Sprintf("$%s: %s", p.Name, detail)
		}
		parts = append(parts, fmt.Sprintf("%s: %s", safeIdent(p.Name), pm.MochiType))
	}
	return strings.Join(parts, ", "), 0, ""
}

// classHandle converts a PHP FQCN to a Mochi extern type handle name.
// "GuzzleHttp\\Client" -> "GuzzleHttpClient"
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

// ToSnakeCase converts a PascalCase or camelCase identifier to snake_case.
// "GuzzleHttpClient" -> "guzzle_http_client"
// "send" -> "send"
func ToSnakeCase(s string) string {
	return toSnakeCase(s)
}

func toSnakeCase(s string) string {
	var sb strings.Builder
	for i, r := range s {
		if unicode.IsUpper(r) {
			if i > 0 {
				sb.WriteByte('_')
			}
			sb.WriteRune(unicode.ToLower(r))
		} else {
			sb.WriteRune(r)
		}
	}
	return sb.String()
}

// safeIdent returns a safe Mochi identifier for a PHP parameter name.
func safeIdent(name string) string {
	return name
}
