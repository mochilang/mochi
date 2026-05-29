// Package typemap implements the closed RBS-to-Mochi type translation table
// described in [website/docs/research/0076/05-type-mapping.md]. Items whose
// entire RBS signature falls inside the table are translated; items with any
// out-of-table type produce a SkipReport.
//
// Phase 5 of MEP-76 fills in the Translate function.
package typemap

import (
	"fmt"
	"strings"

	pkgerr "mochi/package3/ruby/errors"
	"mochi/package3/ruby/rbs"
)

// MochiType is a simplified Mochi type string as it appears in an extern fn
// declaration, e.g. "int", "string", "list<string>", "map<string, int>",
// "bool", "int?".
type MochiType = string

// knownHandles is the closed set of named Ruby types that map to Mochi
// handle<T> rather than producing a SkipUnknown report.
var knownHandles = map[string]bool{
	"IO": true, "File": true, "StringIO": true, "Tempfile": true,
	"Socket": true, "TCPSocket": true, "UDPSocket": true,
	"OpenSSL::SSL::SSLSocket":   true,
	"Nokogiri::HTML::Document":  true,
	"Nokogiri::XML::Document":   true,
	"Nokogiri::NodeSet":         true,
}

// Translate converts an RBS Type to a Mochi type string. It returns a
// SkipReport if the type is outside the closed translation table.
//
// The gem and item parameters are used for the SkipReport message only.
func Translate(t *rbs.Type, gem, item string) (MochiType, *pkgerr.SkipReport) {
	if t == nil {
		return "unit", nil
	}
	switch t.Kind {
	case rbs.TypeInteger:
		return "int", nil
	case rbs.TypeFloat:
		return "float", nil
	case rbs.TypeString:
		return "string", nil
	case rbs.TypeSymbol:
		return "string", nil
	case rbs.TypeBool:
		return "bool", nil
	case rbs.TypeNil:
		return "nil", nil
	case rbs.TypeVoid:
		return "unit", nil
	case rbs.TypeOptional:
		if t.Elem == nil {
			return skip(gem, item, pkgerr.SkipComplexUnion, "T? with nil elem")
		}
		inner, s := Translate(t.Elem, gem, item)
		if s != nil {
			return skip(gem, item, pkgerr.SkipComplexUnion, "optional with untranslatable elem")
		}
		return inner + "?", nil
	case rbs.TypeArray:
		if t.Elem == nil {
			return skip(gem, item, pkgerr.SkipUntyped, "Array[] with no element type")
		}
		elem, s := Translate(t.Elem, gem, item)
		if s != nil {
			return skip(gem, item, s.Reason, "Array element: "+s.RBSType)
		}
		return "list<" + elem + ">", nil
	case rbs.TypeHash:
		if t.Key == nil || t.Value == nil {
			return skip(gem, item, pkgerr.SkipUntyped, "Hash with nil key or value")
		}
		key, ks := Translate(t.Key, gem, item)
		if ks != nil {
			return skip(gem, item, ks.Reason, "Hash key: "+ks.RBSType)
		}
		val, vs := Translate(t.Value, gem, item)
		if vs != nil {
			return skip(gem, item, vs.Reason, "Hash value: "+vs.RBSType)
		}
		return "map<" + key + ", " + val + ">", nil
	case rbs.TypeTuple:
		if len(t.Members) == 0 {
			return skip(gem, item, pkgerr.SkipUntyped, "empty tuple")
		}
		parts := make([]string, 0, len(t.Members))
		for _, m := range t.Members {
			mt, s := Translate(m, gem, item)
			if s != nil {
				return skip(gem, item, s.Reason, "tuple member: "+s.RBSType)
			}
			parts = append(parts, mt)
		}
		return "tuple<" + joinTypes(parts) + ">", nil
	case rbs.TypeProc:
		if len(t.Params) > 5 {
			return skip(gem, item, pkgerr.SkipProc, "proc arity > 5")
		}
		paramTypes := make([]string, 0, len(t.Params))
		for _, p := range t.Params {
			pt, s := Translate(p, gem, item)
			if s != nil {
				return skip(gem, item, pkgerr.SkipProc, "proc param: "+s.RBSType)
			}
			paramTypes = append(paramTypes, pt)
		}
		ret := "unit"
		if t.Return != nil {
			var s *pkgerr.SkipReport
			ret, s = Translate(t.Return, gem, item)
			if s != nil {
				return skip(gem, item, pkgerr.SkipProc, "proc return: "+s.RBSType)
			}
		}
		return "fun(" + joinTypes(paramTypes) + "): " + ret, nil
	case rbs.TypeUntyped:
		return skip(gem, item, pkgerr.SkipUntyped, "untyped")
	case rbs.TypeTop, rbs.TypeBot:
		return skip(gem, item, pkgerr.SkipTopBot, t.Name)
	case rbs.TypeSelf, rbs.TypeInstance, rbs.TypeClass:
		return skip(gem, item, pkgerr.SkipSelfType, t.Name)
	case rbs.TypeNamed:
		if knownHandles[t.Name] {
			return "handle<" + t.Name + ">", nil
		}
		return skip(gem, item, pkgerr.SkipUnknown, "named type "+t.Name)
	default:
		return skip(gem, item, pkgerr.SkipUnknown, "unknown RBS type kind")
	}
}

// TranslateMethod converts one RBS MethodDecl to a Mochi extern fn line.
// The className is the RBS class the method belongs to (e.g. "Redis").
// Returns ("", SkipReport) if the method has no overloads or any type is out of table.
func TranslateMethod(gem string, method rbs.MethodDecl, className string) (string, *pkgerr.SkipReport) {
	if len(method.Types) == 0 {
		return "", &pkgerr.SkipReport{
			Gem:     gem,
			Item:    qualifiedItem(className, method),
			Reason:  pkgerr.SkipNativeExtension,
			RBSType: "no overloads",
		}
	}
	sig := method.Types[0]
	item := qualifiedItem(className, method)

	var params []string
	for _, p := range sig.Params {
		if p.Type != nil && p.Type.Kind == rbs.TypeVoid {
			return "", &pkgerr.SkipReport{
				Gem:     gem,
				Item:    item,
				Reason:  pkgerr.SkipVoidParam,
				RBSType: "void",
			}
		}
		mt, sr := Translate(p.Type, gem, item)
		if sr != nil {
			return "", sr
		}
		params = append(params, p.Name+": "+mt)
	}

	ret, sr := Translate(sig.Return, gem, item)
	if sr != nil {
		return "", sr
	}

	fnName := mangledName(gem, className, method.Name)
	paramStr := strings.Join(params, ", ")
	return fmt.Sprintf("extern fun %s(%s): %s from ruby %q", fnName, paramStr, ret, item), nil
}

// MochiTypeString renders a type as a Mochi type string, returning "" for
// types that cannot be translated.
func MochiTypeString(t *rbs.Type) string {
	mt, _ := Translate(t, "", "")
	return mt
}

// qualifiedItem returns the RBS-style qualified name for the method.
// Instance methods use "#", singleton methods use ".".
func qualifiedItem(className string, method rbs.MethodDecl) string {
	sep := "#"
	if method.Kind == rbs.MethodSingleton {
		sep = "."
	}
	return className + sep + method.Name
}

// mangledName produces the Mochi-side function name from gem, class, and method.
// Example: gem="redis", class="Redis", method="get" => "redis_get"
// Example: gem="nokogiri", class="Nokogiri::HTML::Document", method="parse" => "nokogiri_html_document_parse"
func mangledName(gem, className, methodName string) string {
	classPart := strings.ToLower(className)
	classPart = strings.ReplaceAll(classPart, "::", "_")
	classPart = strings.ReplaceAll(classPart, "-", "_")
	classPart = strings.ReplaceAll(classPart, ".", "_")

	gemPrefix := strings.ReplaceAll(strings.ToLower(gem), "-", "_")
	stripped := strings.TrimPrefix(classPart, gemPrefix+"_")
	if stripped == "" {
		return gemPrefix + "_" + methodName
	}
	if stripped == classPart {
		return classPart + "_" + methodName
	}
	return gemPrefix + "_" + stripped + "_" + methodName
}

func skip(gem, item string, reason pkgerr.SkipReason, rbsType string) (MochiType, *pkgerr.SkipReport) {
	return "", &pkgerr.SkipReport{
		Gem:     gem,
		Item:    item,
		Reason:  reason,
		RBSType: rbsType,
	}
}

func joinTypes(ss []string) string {
	var sb strings.Builder
	for i, s := range ss {
		if i > 0 {
			sb.WriteString(", ")
		}
		sb.WriteString(s)
	}
	return sb.String()
}
