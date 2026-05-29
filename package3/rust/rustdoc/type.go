package rustdoc

import (
	"encoding/json"
	"fmt"
	"sort"
)

// Type is the rustdoc-types `Type` discriminated union. Like ItemEnum,
// the wire shape is a tagged object with exactly one populated key. The
// bridge models every variant that needs different SkipReport
// classification; everything else lands in Unknown.
type Type struct {
	ResolvedPath    *PathType         `json:"resolved_path,omitempty"`
	DynTrait        *DynTraitType     `json:"dyn_trait,omitempty"`
	Generic         string            `json:"generic,omitempty"`
	Primitive       string            `json:"primitive,omitempty"`
	FunctionPointer *FunctionPointer  `json:"function_pointer,omitempty"`
	Tuple           []Type            `json:"tuple,omitempty"`
	Slice           *Type             `json:"slice,omitempty"`
	Array           *ArrayType        `json:"array,omitempty"`
	Pat             *PatType          `json:"pat,omitempty"`
	ImplTrait       []GenericBound    `json:"impl_trait,omitempty"`
	Infer           bool              `json:"infer,omitempty"`
	RawPointer      *RawPointerType   `json:"raw_pointer,omitempty"`
	BorrowedRef     *BorrowedRefType  `json:"borrowed_ref,omitempty"`
	QualifiedPath   *QualifiedPathType `json:"qualified_path,omitempty"`

	// Unknown captures any variant tag the bridge does not model.
	Unknown string `json:"-"`
}

// Kind returns a stable tag describing the populated variant. Empty
// when no variant is set.
func (t Type) Kind() string {
	switch {
	case t.ResolvedPath != nil:
		return "resolved_path"
	case t.DynTrait != nil:
		return "dyn_trait"
	case t.Generic != "":
		return "generic"
	case t.Primitive != "":
		return "primitive"
	case t.FunctionPointer != nil:
		return "function_pointer"
	case len(t.Tuple) > 0:
		return "tuple"
	case t.Slice != nil:
		return "slice"
	case t.Array != nil:
		return "array"
	case t.Pat != nil:
		return "pat"
	case len(t.ImplTrait) > 0:
		return "impl_trait"
	case t.Infer:
		return "infer"
	case t.RawPointer != nil:
		return "raw_pointer"
	case t.BorrowedRef != nil:
		return "borrowed_ref"
	case t.QualifiedPath != nil:
		return "qualified_path"
	}
	if t.Unknown != "" {
		return "unknown:" + t.Unknown
	}
	return "empty"
}

// UnmarshalJSON dispatches on the single tag key.
func (t *Type) UnmarshalJSON(data []byte) error {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		// rustdoc sometimes emits a bare string for variants that have
		// no payload (e.g. "infer"); handle that here.
		var s string
		if err2 := json.Unmarshal(data, &s); err2 == nil {
			switch s {
			case "infer":
				t.Infer = true
				return nil
			}
			t.Unknown = s
			return nil
		}
		return fmt.Errorf("rustdoc: type: %w", err)
	}
	if len(obj) == 0 {
		return nil
	}
	keys := make([]string, 0, len(obj))
	for k := range obj {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	tag := keys[0]
	raw := obj[tag]
	switch tag {
	case "resolved_path":
		var x PathType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.resolved_path: %w", err)
		}
		t.ResolvedPath = &x
	case "dyn_trait":
		var x DynTraitType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.dyn_trait: %w", err)
		}
		t.DynTrait = &x
	case "generic":
		var s string
		if err := json.Unmarshal(raw, &s); err != nil {
			return fmt.Errorf("rustdoc: type.generic: %w", err)
		}
		t.Generic = s
	case "primitive":
		var s string
		if err := json.Unmarshal(raw, &s); err != nil {
			return fmt.Errorf("rustdoc: type.primitive: %w", err)
		}
		t.Primitive = s
	case "function_pointer":
		var x FunctionPointer
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.function_pointer: %w", err)
		}
		t.FunctionPointer = &x
	case "tuple":
		var ts []Type
		if err := json.Unmarshal(raw, &ts); err != nil {
			return fmt.Errorf("rustdoc: type.tuple: %w", err)
		}
		t.Tuple = ts
	case "slice":
		var x Type
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.slice: %w", err)
		}
		t.Slice = &x
	case "array":
		var x ArrayType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.array: %w", err)
		}
		t.Array = &x
	case "pat":
		var x PatType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.pat: %w", err)
		}
		t.Pat = &x
	case "impl_trait":
		var bs []GenericBound
		if err := json.Unmarshal(raw, &bs); err != nil {
			return fmt.Errorf("rustdoc: type.impl_trait: %w", err)
		}
		t.ImplTrait = bs
	case "raw_pointer":
		var x RawPointerType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.raw_pointer: %w", err)
		}
		t.RawPointer = &x
	case "borrowed_ref":
		var x BorrowedRefType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.borrowed_ref: %w", err)
		}
		t.BorrowedRef = &x
	case "qualified_path":
		var x QualifiedPathType
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type.qualified_path: %w", err)
		}
		t.QualifiedPath = &x
	default:
		t.Unknown = tag
	}
	return nil
}

// PathType is the payload of a `resolved_path` Type. It points at an
// Item via ID and may carry generic arguments.
type PathType struct {
	ID   string      `json:"id"`
	Path string      `json:"path"`
	Args *GenericArgs `json:"args,omitempty"`
}

// DynTraitType captures `dyn Trait1 + Trait2 + 'a`.
type DynTraitType struct {
	Traits   []PolyTrait `json:"traits"`
	Lifetime string      `json:"lifetime,omitempty"`
}

// PolyTrait is the higher-ranked-trait-bound form `for<'a> Trait<...>`.
type PolyTrait struct {
	Trait         PathType         `json:"trait"`
	GenericParams []GenericParamDef `json:"generic_params,omitempty"`
}

// FunctionPointer captures `fn(...) -> ...` types.
type FunctionPointer struct {
	Sig           FunctionSig      `json:"sig"`
	GenericParams []GenericParamDef `json:"generic_params,omitempty"`
	Header        FunctionHeader   `json:"header"`
}

// ArrayType is `[T; N]`.
type ArrayType struct {
	Type Type   `json:"type"`
	Len  string `json:"len"`
}

// PatType captures unstable pattern types.
type PatType struct {
	Base    Type   `json:"base"`
	Pattern string `json:"pattern,omitempty"`
}

// RawPointerType is `*const T` / `*mut T`.
type RawPointerType struct {
	IsMutable bool `json:"is_mutable"`
	Type      Type `json:"type"`
}

// BorrowedRefType is `&'a T` / `&'a mut T`.
type BorrowedRefType struct {
	Lifetime  string `json:"lifetime,omitempty"`
	IsMutable bool   `json:"is_mutable"`
	Type      Type   `json:"type"`
}

// QualifiedPathType is `<T as Trait>::Item`.
type QualifiedPathType struct {
	Name     string    `json:"name"`
	Args     GenericArgs `json:"args"`
	SelfType Type      `json:"self_type"`
	Trait    *PathType `json:"trait,omitempty"`
}

// Generics is the full generics declaration on an item: params + where.
type Generics struct {
	Params       []GenericParamDef    `json:"params,omitempty"`
	WherePredicates []WherePredicate `json:"where_predicates,omitempty"`
}

// GenericParamDef is one of `<T: Bound>`, `<'a>`, `<const N: usize>`.
type GenericParamDef struct {
	Name string             `json:"name"`
	Kind GenericParamDefKind `json:"kind"`
}

// GenericParamDefKind is the discriminator over lifetime / type / const.
type GenericParamDefKind struct {
	Lifetime *struct {
		Outlives []string `json:"outlives,omitempty"`
	} `json:"lifetime,omitempty"`
	Type *struct {
		Bounds  []GenericBound `json:"bounds,omitempty"`
		Default *Type          `json:"default,omitempty"`
		Synthetic bool         `json:"synthetic"`
	} `json:"type,omitempty"`
	Const *struct {
		Type    Type   `json:"type"`
		Default string `json:"default,omitempty"`
	} `json:"const,omitempty"`
}

// GenericArgs is the args portion of a path / trait reference. Two
// shapes: AngleBracketed `<A, B = C>` and Parenthesized `(A, B) -> C`.
type GenericArgs struct {
	AngleBracketed *AngleBracketedArgs `json:"angle_bracketed,omitempty"`
	Parenthesized  *ParenthesizedArgs  `json:"parenthesized,omitempty"`
}

// AngleBracketedArgs is `<A, B = C, 'x>`.
type AngleBracketedArgs struct {
	Args       []GenericArg     `json:"args,omitempty"`
	Constraints []AssocItemConstraint `json:"constraints,omitempty"`
}

// ParenthesizedArgs is `(A, B) -> C` (used for Fn traits).
type ParenthesizedArgs struct {
	Inputs []Type `json:"inputs"`
	Output *Type  `json:"output,omitempty"`
}

// GenericArg is one positional generic argument.
type GenericArg struct {
	Lifetime string `json:"lifetime,omitempty"`
	Type     *Type  `json:"type,omitempty"`
	Const    *ConstValue `json:"const,omitempty"`
	Infer    bool `json:"infer,omitempty"`
}

// UnmarshalJSON handles the tagged-object form.
func (a *GenericArg) UnmarshalJSON(data []byte) error {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		return fmt.Errorf("rustdoc: generic arg: %w", err)
	}
	for k, raw := range obj {
		switch k {
		case "lifetime":
			_ = json.Unmarshal(raw, &a.Lifetime)
		case "type":
			var t Type
			if err := json.Unmarshal(raw, &t); err != nil {
				return fmt.Errorf("rustdoc: generic arg.type: %w", err)
			}
			a.Type = &t
		case "const":
			var c ConstValue
			if err := json.Unmarshal(raw, &c); err != nil {
				return fmt.Errorf("rustdoc: generic arg.const: %w", err)
			}
			a.Const = &c
		case "infer":
			a.Infer = true
		}
	}
	return nil
}

// AssocItemConstraint is `Item = Ty` or `Item: Bound`.
type AssocItemConstraint struct {
	Name    string `json:"name"`
	Args    GenericArgs `json:"args"`
	Binding AssocItemBinding `json:"binding"`
}

// AssocItemBinding is `= Ty` or `: Bound`.
type AssocItemBinding struct {
	Equality *Term         `json:"equality,omitempty"`
	Constraint []GenericBound `json:"constraint,omitempty"`
}

// Term is `Ty` or a const-value.
type Term struct {
	Type  *Type      `json:"type,omitempty"`
	Const *ConstValue `json:"const,omitempty"`
}

// GenericBound is `T: Bound` (TraitBound), `T: 'a` (Outlives), or
// `use<...>` (the captured-generics opt-out).
type GenericBound struct {
	TraitBound *TraitBound `json:"trait_bound,omitempty"`
	Outlives   string      `json:"outlives,omitempty"`
	Use        []string    `json:"use,omitempty"`
}

// TraitBound is the `Trait<...>` body of a TraitBound generic bound.
type TraitBound struct {
	Trait         PathType         `json:"trait"`
	GenericParams []GenericParamDef `json:"generic_params,omitempty"`
	Modifier      string           `json:"modifier,omitempty"` // "none", "maybe", "maybe_const"
}

// WherePredicate models the entries in a `where` clause.
type WherePredicate struct {
	BoundPredicate    *BoundPredicate    `json:"bound_predicate,omitempty"`
	LifetimePredicate *LifetimePredicate `json:"lifetime_predicate,omitempty"`
	EqPredicate       *EqPredicate       `json:"eq_predicate,omitempty"`
}

// BoundPredicate is `T: Bound + 'a`.
type BoundPredicate struct {
	Type          Type             `json:"type"`
	Bounds        []GenericBound   `json:"bounds"`
	GenericParams []GenericParamDef `json:"generic_params,omitempty"`
}

// LifetimePredicate is `'a: 'b + 'c`.
type LifetimePredicate struct {
	Lifetime string   `json:"lifetime"`
	Outlives []string `json:"outlives"`
}

// EqPredicate is `Item = Ty` inside a where clause.
type EqPredicate struct {
	LHS Type `json:"lhs"`
	RHS Term `json:"rhs"`
}
