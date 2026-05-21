package typebridge

import "go/types"

// Constraint is a Mochi-side identifier for a recognised Go generic
// constraint. The bridge captures the constraint identity here; the
// Phase 2 resolver compares the Mochi-side argument type's traits
// against the constraint when instantiating a generic call site.
type Constraint uint8

const (
	ConstraintUnknown Constraint = iota
	ConstraintAny                // any / interface{} with no methods
	ConstraintComparable         // builtin comparable
	ConstraintOrdered            // cmp.Ordered or constraints.Ordered
	ConstraintInteger            // constraints.Integer
	ConstraintSigned             // constraints.Signed
	ConstraintUnsigned           // constraints.Unsigned
	ConstraintFloat              // constraints.Float
)

// String returns the Mochi-side identifier for diagnostics.
func (c Constraint) String() string {
	switch c {
	case ConstraintAny:
		return "any"
	case ConstraintComparable:
		return "comparable"
	case ConstraintOrdered:
		return "Ordered"
	case ConstraintInteger:
		return "Integer"
	case ConstraintSigned:
		return "Signed"
	case ConstraintUnsigned:
		return "Unsigned"
	case ConstraintFloat:
		return "Float"
	}
	return "unknown"
}

// LookupConstraint returns the Mochi-side Constraint identifier for
// a Go type that appears as a type-parameter constraint. The bool
// is false if the type is not a recognised constraint; the caller
// surfaces an ErrGenericConstraintUnsupported diagnostic in that
// case.
//
// The table is small by design (~7 entries), and is the only
// hand-maintained piece of the bridge beyond the type-shape switch
// in GoToMochi. Adding a constraint is a one-line PR with one test.
func LookupConstraint(t types.Type) (Constraint, bool) {
	if t == nil {
		return ConstraintUnknown, false
	}
	t = types.Unalias(t)

	if iface, ok := t.(*types.Interface); ok && iface.Empty() {
		return ConstraintAny, true
	}

	if basic, ok := t.(*types.Basic); ok {
		// `comparable` is a predeclared identifier whose type is a Basic.
		if basic.Name() == "comparable" {
			return ConstraintComparable, true
		}
		if basic.Name() == "any" {
			return ConstraintAny, true
		}
	}

	if named, ok := t.(*types.Named); ok {
		obj := named.Obj()
		if obj == nil {
			return ConstraintUnknown, false
		}
		name := obj.Name()
		if obj.Pkg() == nil {
			// predeclared (comparable etc.)
			switch name {
			case "comparable":
				return ConstraintComparable, true
			case "any":
				return ConstraintAny, true
			}
			return ConstraintUnknown, false
		}
		switch obj.Pkg().Path() {
		case "cmp":
			if name == "Ordered" {
				return ConstraintOrdered, true
			}
		case "golang.org/x/exp/constraints":
			switch name {
			case "Ordered":
				return ConstraintOrdered, true
			case "Integer":
				return ConstraintInteger, true
			case "Signed":
				return ConstraintSigned, true
			case "Unsigned":
				return ConstraintUnsigned, true
			case "Float":
				return ConstraintFloat, true
			}
		}
	}

	return ConstraintUnknown, false
}
