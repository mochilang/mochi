// hierarchy.go provides PHP class/interface hierarchy analysis for Phase 11.
//
// The Hierarchy type indexes a ReflectionSurface by class/interface
// relationships: which classes implement which interfaces, which classes
// extend which parents, and which concrete classes exist for each abstract
// base or interface.
package externemit

import (
	"strings"

	"mochi/package3/php/reflect"
)

// Hierarchy indexes the class/interface relationships in a ReflectionSurface.
type Hierarchy struct {
	// ClassByFQCN maps FQCN to ClassSurface.
	ClassByFQCN map[string]reflect.ClassSurface
	// InterfaceByFQCN maps FQCN to InterfaceSurface.
	InterfaceByFQCN map[string]reflect.InterfaceSurface
	// ImplementorsOf maps interface FQCN to the list of concrete class FQCNs
	// that implement it (directly or transitively via interface extension).
	ImplementorsOf map[string][]string
	// SubclassesOf maps parent FQCN to list of direct subclass FQCNs.
	SubclassesOf map[string][]string
	// AbstractClasses is the set of abstract class FQCNs.
	AbstractClasses map[string]bool
}

// BuildHierarchy constructs a Hierarchy from a ReflectionSurface.
func BuildHierarchy(surface *reflect.ReflectionSurface) *Hierarchy {
	h := &Hierarchy{
		ClassByFQCN:     make(map[string]reflect.ClassSurface),
		InterfaceByFQCN: make(map[string]reflect.InterfaceSurface),
		ImplementorsOf:  make(map[string][]string),
		SubclassesOf:    make(map[string][]string),
		AbstractClasses: make(map[string]bool),
	}

	// Index all classes and interfaces.
	for _, cls := range surface.Classes {
		h.ClassByFQCN[cls.FQCN] = cls
		if cls.Abstract {
			h.AbstractClasses[cls.FQCN] = true
		}
	}
	for _, iface := range surface.Interfaces {
		h.InterfaceByFQCN[iface.FQCN] = iface
	}

	// Build parent -> child and interface -> implementor maps.
	for _, cls := range surface.Classes {
		if cls.ParentFQCN != "" {
			h.SubclassesOf[cls.ParentFQCN] = append(h.SubclassesOf[cls.ParentFQCN], cls.FQCN)
		}
		for _, ifaceFQCN := range cls.InterfaceFQCNs {
			h.ImplementorsOf[ifaceFQCN] = append(h.ImplementorsOf[ifaceFQCN], cls.FQCN)
		}
	}

	return h
}

// ConcreteImplementors returns all non-abstract classes that implement
// the given interface FQCN, searching the full hierarchy.
func (h *Hierarchy) ConcreteImplementors(ifaceFQCN string) []string {
	var result []string
	seen := make(map[string]bool)
	for _, fqcn := range h.ImplementorsOf[ifaceFQCN] {
		if !h.AbstractClasses[fqcn] && !seen[fqcn] {
			seen[fqcn] = true
			result = append(result, fqcn)
		}
	}
	return result
}

// AllSubclasses returns all direct and transitive subclasses of a class.
func (h *Hierarchy) AllSubclasses(fqcn string) []string {
	var result []string
	seen := make(map[string]bool)
	var walk func(string)
	walk = func(parent string) {
		for _, child := range h.SubclassesOf[parent] {
			if !seen[child] {
				seen[child] = true
				result = append(result, child)
				walk(child)
			}
		}
	}
	walk(fqcn)
	return result
}

// InterfacesOf returns the list of interface FQCNs a class directly implements.
func (h *Hierarchy) InterfacesOf(classFQCN string) []string {
	cls, ok := h.ClassByFQCN[classFQCN]
	if !ok {
		return nil
	}
	return cls.InterfaceFQCNs
}

// EmitAbstractBridge emits extern type declarations for abstract classes and
// interfaces, adding a comment noting which concrete classes implement them.
// This supplements Emit() for surfaces that have abstract/interface hierarchies.
func EmitAbstractBridge(surface *reflect.ReflectionSurface) *EmitResult {
	h := BuildHierarchy(surface)
	e := &emitter{pkg: surface.PackageName}

	// Emit extern types for abstract classes with concrete subclass annotations.
	for _, cls := range surface.Classes {
		if !cls.Abstract {
			continue
		}
		handle := classHandle(cls.FQCN)
		concretes := h.AllSubclasses(cls.FQCN)
		var concreteParts []string
		for _, c := range concretes {
			if !h.AbstractClasses[c] {
				concreteParts = append(concreteParts, classHandle(c))
			}
		}
		if len(concreteParts) > 0 {
			e.addLine("// Abstract base; concrete subclasses: " + strings.Join(concreteParts, ", "))
		}
		e.addLine("extern type " + handle)
		e.addLine("")
	}

	// Emit extern types for interfaces with concrete implementor annotations.
	for _, iface := range surface.Interfaces {
		handle := classHandle(iface.FQCN)
		implementors := h.ConcreteImplementors(iface.FQCN)
		var implParts []string
		for _, c := range implementors {
			implParts = append(implParts, classHandle(c))
		}
		if len(implParts) > 0 {
			e.addLine("// Interface; concrete implementors: " + strings.Join(implParts, ", "))
		}
		e.addLine("extern type " + handle)
		e.addLine("")
	}

	return &EmitResult{
		MochiSource: e.render(),
		Skips:       e.skips,
	}
}
