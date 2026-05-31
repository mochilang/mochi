package generics

import (
	"fmt"
	"strings"
)

// GenericCallSite records one generic method call site observed during type checking.
type GenericCallSite struct {
	// Method is the unqualified method name (e.g. "Parse").
	Method string
	// TypeArgs is the list of concrete Mochi type names (e.g. "int64", "string").
	TypeArgs []string
}

// ConcreteMethodSpec is a concrete monomorphised method produced by MonomorphiseAll.
type ConcreteMethodSpec struct {
	// Name is the mangled method name (e.g. "Parse_Int64").
	Name string
	// TypeArgs are the concrete type arguments used for this instantiation.
	TypeArgs []string
	// IsSkipped indicates this call site was skipped.
	IsSkipped bool
	// SkipReason describes why the call site was skipped.
	SkipReason string
}

// MonomorphiseAll takes the full set of GenericCallSites recorded during surface
// extraction and returns a set of concrete ConcreteMethodSpec values.
//
// Only type arguments that map to Mochi primitive types (int, float, string, bool,
// int64, float64) are supported; others produce a skipped entry.
func MonomorphiseAll(sites []GenericCallSite) ([]ConcreteMethodSpec, error) {
	out := make([]ConcreteMethodSpec, 0, len(sites))
	for _, site := range sites {
		spec, skip := monomorphiseOne(site)
		if skip != "" {
			out = append(out, ConcreteMethodSpec{
				Name:       site.Method,
				IsSkipped:  true,
				SkipReason: skip,
			})
			continue
		}
		out = append(out, spec)
	}
	return out, nil
}

// monomorphiseOne converts one GenericCallSite to a ConcreteMethodSpec.
// Returns ("", skip reason) if the site cannot be monomorphised.
func monomorphiseOne(site GenericCallSite) (ConcreteMethodSpec, string) {
	if len(site.TypeArgs) == 0 {
		return ConcreteMethodSpec{}, fmt.Sprintf("no type args for %s", site.Method)
	}
	mangled := make([]string, len(site.TypeArgs))
	for i, ta := range site.TypeArgs {
		m, ok := mochiTypeToMangle(ta)
		if !ok {
			return ConcreteMethodSpec{}, fmt.Sprintf("unsupported type arg %q for %s", ta, site.Method)
		}
		mangled[i] = m
	}
	name := site.Method + "_" + strings.Join(mangled, "_")
	return ConcreteMethodSpec{
		Name:     name,
		TypeArgs: site.TypeArgs,
	}, ""
}

// mochiTypeToMangle maps a Mochi primitive type name to its mangled suffix.
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
	case "uint", "uint64":
		return "UInt64", true
	default:
		return "", false
	}
}
