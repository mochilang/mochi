// Package monomorphise handles Swift generic function monomorphisation.
// For generic Swift functions like `func parse<T: Decodable>(...) -> T`,
// the bridge generates one concrete @_cdecl wrapper per type argument
// observed at Mochi call sites.
package monomorphise

import (
	"fmt"
	"strings"
)

// CallSite records one generic function call site.
type CallSite struct {
	// FuncName is the unqualified Swift function name.
	FuncName string
	// Module is the Swift module name.
	Module string
	// TypeArgs is the list of concrete Mochi type names.
	TypeArgs []string
}

// ConcreteFunc is a concrete monomorphised function.
type ConcreteFunc struct {
	// MochiName is the @_cdecl symbol name (mangled with type args).
	MochiName string
	// SwiftName is the original Swift function name.
	SwiftName string
	// Module is the Swift module.
	Module string
	// TypeArgs are the concrete type arguments.
	TypeArgs []string
	// IsSkipped indicates this site was skipped.
	IsSkipped bool
	// SkipReason describes why.
	SkipReason string
}

// MonomorphiseAll converts a set of CallSites to concrete ConcreteFunc values.
// Unsupported type arguments produce IsSkipped=true entries.
func MonomorphiseAll(sites []CallSite) ([]ConcreteFunc, error) {
	out := make([]ConcreteFunc, 0, len(sites))
	for _, site := range sites {
		f, skip := monomorphiseOne(site)
		out = append(out, f)
		_ = skip
	}
	return out, nil
}

func monomorphiseOne(site CallSite) (ConcreteFunc, string) {
	if len(site.TypeArgs) == 0 {
		return ConcreteFunc{
			MochiName:  site.FuncName,
			SwiftName:  site.FuncName,
			Module:     site.Module,
			IsSkipped:  true,
			SkipReason: fmt.Sprintf("no type args for %s", site.FuncName),
		}, "no type args"
	}

	mangled := make([]string, len(site.TypeArgs))
	for i, ta := range site.TypeArgs {
		m, ok := mochiTypeToMangle(ta)
		if !ok {
			return ConcreteFunc{
				MochiName:  site.FuncName,
				SwiftName:  site.FuncName,
				Module:     site.Module,
				TypeArgs:   site.TypeArgs,
				IsSkipped:  true,
				SkipReason: fmt.Sprintf("unsupported type arg %q", ta),
			}, fmt.Sprintf("unsupported type arg %q", ta)
		}
		mangled[i] = m
	}

	mochiName := fmt.Sprintf("swift_%s_%s_%s",
		sanitize(site.Module),
		sanitize(site.FuncName),
		strings.Join(mangled, "_"))

	return ConcreteFunc{
		MochiName: mochiName,
		SwiftName: site.FuncName,
		Module:    site.Module,
		TypeArgs:  site.TypeArgs,
	}, ""
}

func mochiTypeToMangle(t string) (string, bool) {
	switch t {
	case "int", "int64":
		return "Int64", true
	case "float", "float64":
		return "Float64", true
	case "string":
		return "String", true
	case "bool":
		return "Bool", true
	default:
		return "", false
	}
}

func sanitize(s string) string {
	var b strings.Builder
	for _, r := range s {
		if (r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z') || (r >= '0' && r <= '9') {
			b.WriteRune(r)
		} else {
			b.WriteRune('_')
		}
	}
	return b.String()
}
