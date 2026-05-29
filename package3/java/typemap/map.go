package typemap

import (
	"fmt"
	"strings"

	javaerrors "mochi/package3/java/errors"
)

// Map maps a Java erased type name to a Mapping or a SkipReason explaining
// why the type cannot be mapped.
//
// The return convention: when SkipReason == SkipUnknown AND detail == "",
// the Mapping is valid. Any non-zero SkipReason indicates failure.
func Map(javaTypeName string) (*Mapping, javaerrors.SkipReason, string) {
	// Refusal checks.
	if sr, detail := checkRefusal(javaTypeName); sr != javaerrors.SkipUnknown {
		return nil, sr, detail
	}

	// Direct table lookups.
	if m, ok := primitiveTable[javaTypeName]; ok {
		return m, javaerrors.SkipUnknown, ""
	}
	if m, ok := boxedTable[javaTypeName]; ok {
		return m, javaerrors.SkipUnknown, ""
	}

	// Parametric types: look for simple suffixes. We only handle unparameterised
	// names at this level (the wrapper synthesiser handles generic instantiations).
	switch javaTypeName {
	case "java.lang.String":
		return &Mapping{Kind: KindString, JavaName: javaTypeName, MochiType: "string", FFIRepr: "String"}, javaerrors.SkipUnknown, ""
	case "void":
		return &Mapping{Kind: KindVoid, JavaName: javaTypeName, MochiType: "void", FFIRepr: "void"}, javaerrors.SkipUnknown, ""
	case "byte[]":
		return &Mapping{Kind: KindBytes, JavaName: javaTypeName, MochiType: "bytes", FFIRepr: "byte[]"}, javaerrors.SkipUnknown, ""
	case "int[]", "long[]", "double[]", "boolean[]", "float[]":
		return &Mapping{Kind: KindList, JavaName: javaTypeName, MochiType: "list[" + primitiveElemType(javaTypeName) + "]", FFIRepr: "String"}, javaerrors.SkipUnknown, ""
	case "java.lang.Object":
		return nil, javaerrors.SkipRawObject, "java.lang.Object without generic context is not mappable"
	case "java.util.List", "java.util.ArrayList", "java.util.Collection":
		return nil, javaerrors.SkipGeneric, fmt.Sprintf("raw (unparameterised) %s; declare monomorphisation", javaTypeName)
	case "java.util.Map", "java.util.HashMap", "java.util.LinkedHashMap":
		return nil, javaerrors.SkipGeneric, fmt.Sprintf("raw (unparameterised) %s; declare monomorphisation", javaTypeName)
	case "java.util.Optional":
		return nil, javaerrors.SkipGeneric, "raw java.util.Optional; declare monomorphisation"
	case "java.util.concurrent.CompletableFuture":
		return nil, javaerrors.SkipGeneric, "raw CompletableFuture; declare monomorphisation"
	}

	// Handle generic forms with parameters, e.g. "java.util.List<java.lang.String>".
	if m, ok := mapParametric(javaTypeName); ok {
		return m, javaerrors.SkipUnknown, ""
	}

	// Fall-through: treat unknown reference types as opaque JNI handles.
	return &Mapping{Kind: KindHandle, JavaName: javaTypeName, MochiType: "handle<" + javaTypeName + ">", FFIRepr: "long"}, javaerrors.SkipUnknown, ""
}

// checkRefusal returns a SkipReason if the type name should be refused.
func checkRefusal(name string) (javaerrors.SkipReason, string) {
	// Wildcard.
	if name == "?" || strings.HasPrefix(name, "? extends ") || strings.HasPrefix(name, "? super ") {
		return javaerrors.SkipWildcard, "wildcard type cannot be mapped"
	}
	// Anonymous/inner class (name contains $).
	if strings.Contains(name, "$") {
		return javaerrors.SkipInnerClass, "anonymous or inner class (name contains $)"
	}
	// Reflective APIs.
	if strings.HasPrefix(name, "java.lang.reflect.") || strings.HasPrefix(name, "sun.") || strings.HasPrefix(name, "com.sun.") {
		return javaerrors.SkipReflective, "reflective API not safe to expose"
	}
	return javaerrors.SkipUnknown, ""
}

// mapParametric handles generic forms like "java.util.List<java.lang.String>".
func mapParametric(name string) (*Mapping, bool) {
	inner := func(generic, bracket string) (string, bool) {
		if strings.HasPrefix(name, generic+"<") && strings.HasSuffix(name, ">") {
			return name[len(generic)+1 : len(name)-1], true
		}
		return "", false
	}

	for _, prefix := range []string{"java.util.List", "java.util.ArrayList", "java.util.Collection"} {
		if elem, ok := inner(prefix, "<"); ok {
			elemM, sr, _ := Map(elem)
			if sr != javaerrors.SkipUnknown {
				return nil, false
			}
			m := &Mapping{Kind: KindList, JavaName: name, MochiType: "list[" + elemM.MochiType + "]", FFIRepr: "String", Elem: elemM}
			return m, true
		}
	}

	for _, prefix := range []string{"java.util.Optional"} {
		if elem, ok := inner(prefix, "<"); ok {
			elemM, sr, _ := Map(elem)
			if sr != javaerrors.SkipUnknown {
				return nil, false
			}
			m := &Mapping{Kind: KindOptional, JavaName: name, MochiType: elemM.MochiType + "?", FFIRepr: "String", Elem: elemM}
			return m, true
		}
	}

	if strings.HasPrefix(name, "java.util.concurrent.CompletableFuture<") && strings.HasSuffix(name, ">") {
		elem := name[len("java.util.concurrent.CompletableFuture<") : len(name)-1]
		elemM, sr, _ := Map(elem)
		if sr != javaerrors.SkipUnknown {
			return nil, false
		}
		m := &Mapping{Kind: KindFuture, JavaName: name, MochiType: "async " + elemM.MochiType, FFIRepr: "String", Elem: elemM}
		return m, true
	}

	for _, prefix := range []string{"java.util.Map", "java.util.HashMap"} {
		if strings.HasPrefix(name, prefix+"<") && strings.HasSuffix(name, ">") {
			kv := name[len(prefix)+1 : len(name)-1]
			// Split on first comma that's not inside angle brackets.
			keyStr, valStr := splitMapKV(kv)
			if keyStr == "" {
				return nil, false
			}
			keyM, sr1, _ := Map(keyStr)
			if sr1 != javaerrors.SkipUnknown {
				return nil, false
			}
			valM, sr2, _ := Map(valStr)
			if sr2 != javaerrors.SkipUnknown {
				return nil, false
			}
			m := &Mapping{Kind: KindMap, JavaName: name, MochiType: "map[" + keyM.MochiType + "," + valM.MochiType + "]", FFIRepr: "String", Key: keyM, Value: valM}
			return m, true
		}
	}

	return nil, false
}

// splitMapKV splits "K,V" respecting nested angle brackets.
func splitMapKV(s string) (string, string) {
	depth := 0
	for i, ch := range s {
		switch ch {
		case '<':
			depth++
		case '>':
			depth--
		case ',':
			if depth == 0 {
				return strings.TrimSpace(s[:i]), strings.TrimSpace(s[i+1:])
			}
		}
	}
	return "", ""
}

// primitiveElemType returns the Mochi element type for a primitive array.
func primitiveElemType(name string) string {
	switch name {
	case "int[]":
		return "int"
	case "long[]":
		return "long"
	case "double[]":
		return "double"
	case "float[]":
		return "float"
	case "boolean[]":
		return "bool"
	default:
		return "int"
	}
}

// primitiveTable covers the Java primitive types.
var primitiveTable = map[string]*Mapping{
	"int":     {Kind: KindInt, JavaName: "int", MochiType: "int", FFIRepr: "int"},
	"short":   {Kind: KindInt, JavaName: "short", MochiType: "int", FFIRepr: "int"},
	"byte":    {Kind: KindInt, JavaName: "byte", MochiType: "int", FFIRepr: "int"},
	"char":    {Kind: KindInt, JavaName: "char", MochiType: "int", FFIRepr: "int"},
	"long":    {Kind: KindLong, JavaName: "long", MochiType: "long", FFIRepr: "long"},
	"float":   {Kind: KindFloat, JavaName: "float", MochiType: "float", FFIRepr: "float"},
	"double":  {Kind: KindDouble, JavaName: "double", MochiType: "double", FFIRepr: "double"},
	"boolean": {Kind: KindBool, JavaName: "boolean", MochiType: "bool", FFIRepr: "boolean"},
}

// boxedTable covers the boxed equivalents of Java primitives.
var boxedTable = map[string]*Mapping{
	"java.lang.Integer":   {Kind: KindInt, JavaName: "java.lang.Integer", MochiType: "int", FFIRepr: "int"},
	"java.lang.Short":     {Kind: KindInt, JavaName: "java.lang.Short", MochiType: "int", FFIRepr: "int"},
	"java.lang.Byte":      {Kind: KindInt, JavaName: "java.lang.Byte", MochiType: "int", FFIRepr: "int"},
	"java.lang.Character": {Kind: KindInt, JavaName: "java.lang.Character", MochiType: "int", FFIRepr: "int"},
	"java.lang.Long":      {Kind: KindLong, JavaName: "java.lang.Long", MochiType: "long", FFIRepr: "long"},
	"java.lang.Float":     {Kind: KindFloat, JavaName: "java.lang.Float", MochiType: "float", FFIRepr: "float"},
	"java.lang.Double":    {Kind: KindDouble, JavaName: "java.lang.Double", MochiType: "double", FFIRepr: "double"},
	"java.lang.Boolean":   {Kind: KindBool, JavaName: "java.lang.Boolean", MochiType: "bool", FFIRepr: "boolean"},
}
