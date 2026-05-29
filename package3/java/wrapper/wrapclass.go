// Package wrapper synthesises JNI wrapper Java classes from a reflection surface.
package wrapper

import (
	"fmt"
	"strings"

	"mochi/package3/java/typemap"
)

// WrapClass holds the synthesised wrapper information for a single Java class.
type WrapClass struct {
	// ClassName is the synthesised wrapper class name, e.g. "MochiWrap_com_google_guava_Optional".
	ClassName string
	// OrigClass is the name of the original Java class being wrapped.
	OrigClass string
	// Methods is the list of synthesised wrapper methods.
	Methods []WrapMethod
	// Fields is the list of accessible fields.
	Fields []WrapField
}

// WrapMethod describes a single synthesised wrapper method.
type WrapMethod struct {
	// WrapName is the name of the wrapper method (mangled).
	WrapName string
	// OrigName is the original Java method name.
	OrigName string
	// IsStatic is true if the original method is static.
	IsStatic bool
	// ReturnMapping is the type mapping for the return type.
	ReturnMapping *typemap.Mapping
	// ParamMappings is the type mapping for each parameter.
	ParamMappings []*typemap.Mapping
	// ParamNames is the name of each parameter.
	ParamNames []string
}

// WrapField describes a single accessible field in the wrapper.
type WrapField struct {
	// WrapName is the wrapper field accessor name.
	WrapName string
	// OrigName is the original Java field name.
	OrigName string
	// IsStatic is true if the field is static.
	IsStatic bool
	// Mapping is the type mapping for the field type.
	Mapping *typemap.Mapping
}

// classNameToIdent converts "com.google.guava.Optional" to "com_google_guava_Optional".
func classNameToIdent(name string) string {
	s := strings.ReplaceAll(name, ".", "_")
	s = strings.ReplaceAll(s, "$", "_")
	return s
}

// wrapMethodName returns the mangled wrapper method name.
func wrapMethodName(className, methodName string, isStatic bool) string {
	ident := classNameToIdent(className)
	if isStatic {
		return fmt.Sprintf("MochiWrap_%s_static_%s", ident, methodName)
	}
	return fmt.Sprintf("MochiWrap_%s_%s", ident, methodName)
}

// isStaticMethod returns true if the modifiers contain "static".
func isStaticMethod(modifiers []string) bool {
	for _, m := range modifiers {
		if m == "static" {
			return true
		}
	}
	return false
}

// isPublicMethod returns true if the modifiers contain "public".
func isPublicMethod(modifiers []string) bool {
	for _, m := range modifiers {
		if m == "public" {
			return true
		}
	}
	return false
}

// isFinalField returns true if the modifiers contain "final".
func isFinalField(modifiers []string) bool {
	for _, m := range modifiers {
		if m == "final" {
			return true
		}
	}
	return false
}

// SynthResult is the result of synthesising a single class.
type SynthResult struct {
	WrapClass *WrapClass
	Skips     []SkipEntry
}

// SkipEntry records a skipped method or field.
type SkipEntry struct {
	Name   string
	Reason string
}
