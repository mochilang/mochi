package rustdoc

import (
	"encoding/json"
	"fmt"
	"sort"
)

// ItemEnum is the rustdoc-types `ItemEnum` discriminated union. The
// wire shape is `{"variant_tag": variant_payload}` and we keep
// exactly one of the embedded pointer fields populated per Item. A
// pointer field that remains nil means the item is not of that kind.
//
// The set of variants below covers the ones MEP-73 phase 2 needs to
// recognise. Unknown variants land in Unknown and become a SkipReport
// during the walk so that rust-lang/rust adding new variants does not
// break the parse.
type ItemEnum struct {
	Module      *ModuleItem      `json:"module,omitempty"`
	ExternCrate *ExternCrateItem `json:"extern_crate,omitempty"`
	Use         *UseItem         `json:"use,omitempty"`
	Struct      *StructItem      `json:"struct,omitempty"`
	StructField *Type            `json:"struct_field,omitempty"`
	Enum        *EnumItem        `json:"enum,omitempty"`
	Variant     *VariantItem     `json:"variant,omitempty"`
	Function    *FunctionItem    `json:"function,omitempty"`
	Trait       *TraitItem       `json:"trait,omitempty"`
	Impl        *ImplItem        `json:"impl,omitempty"`
	TypeAlias   *TypeAliasItem   `json:"type_alias,omitempty"`
	Constant    *ConstantItem    `json:"constant,omitempty"`
	Static      *StaticItem      `json:"static,omitempty"`
	Macro       *MacroItem       `json:"macro,omitempty"`
	ProcMacro   *ProcMacroItem   `json:"proc_macro,omitempty"`
	Primitive   *PrimitiveItem   `json:"primitive,omitempty"`
	AssocConst  *AssocConstItem  `json:"assoc_const,omitempty"`
	AssocType   *AssocTypeItem   `json:"assoc_type,omitempty"`

	// Unknown captures the variant tag of any unrecognised entry so
	// the walker can produce a precise SkipReport. The payload is
	// discarded because the type-mapping phase does not look at it.
	Unknown string `json:"-"`
}

// Kind returns a stable token describing which variant is populated.
// Returns "unknown" with the offending tag included when Unknown is set.
func (e ItemEnum) Kind() string {
	switch {
	case e.Module != nil:
		return "module"
	case e.ExternCrate != nil:
		return "extern_crate"
	case e.Use != nil:
		return "use"
	case e.Struct != nil:
		return "struct"
	case e.StructField != nil:
		return "struct_field"
	case e.Enum != nil:
		return "enum"
	case e.Variant != nil:
		return "variant"
	case e.Function != nil:
		return "function"
	case e.Trait != nil:
		return "trait"
	case e.Impl != nil:
		return "impl"
	case e.TypeAlias != nil:
		return "type_alias"
	case e.Constant != nil:
		return "constant"
	case e.Static != nil:
		return "static"
	case e.Macro != nil:
		return "macro"
	case e.ProcMacro != nil:
		return "proc_macro"
	case e.Primitive != nil:
		return "primitive"
	case e.AssocConst != nil:
		return "assoc_const"
	case e.AssocType != nil:
		return "assoc_type"
	}
	if e.Unknown != "" {
		return "unknown:" + e.Unknown
	}
	return "empty"
}

// UnmarshalJSON dispatches on the tagged-object key. Falls back to
// Unknown when the tag is not in the recognised set.
func (e *ItemEnum) UnmarshalJSON(data []byte) error {
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		return fmt.Errorf("rustdoc: item enum: %w", err)
	}
	if len(obj) == 0 {
		return nil
	}
	// Pick the key deterministically so error messages are stable.
	keys := make([]string, 0, len(obj))
	for k := range obj {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	tag := keys[0]
	raw := obj[tag]
	switch tag {
	case "module":
		var x ModuleItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: module: %w", err)
		}
		e.Module = &x
	case "extern_crate":
		var x ExternCrateItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: extern_crate: %w", err)
		}
		e.ExternCrate = &x
	case "use":
		var x UseItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: use: %w", err)
		}
		e.Use = &x
	case "struct":
		var x StructItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: struct: %w", err)
		}
		e.Struct = &x
	case "struct_field":
		var x Type
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: struct_field: %w", err)
		}
		e.StructField = &x
	case "enum":
		var x EnumItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: enum: %w", err)
		}
		e.Enum = &x
	case "variant":
		var x VariantItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: variant: %w", err)
		}
		e.Variant = &x
	case "function":
		var x FunctionItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: function: %w", err)
		}
		e.Function = &x
	case "trait":
		var x TraitItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: trait: %w", err)
		}
		e.Trait = &x
	case "impl":
		var x ImplItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: impl: %w", err)
		}
		e.Impl = &x
	case "type_alias":
		var x TypeAliasItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: type_alias: %w", err)
		}
		e.TypeAlias = &x
	case "constant":
		var x ConstantItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: constant: %w", err)
		}
		e.Constant = &x
	case "static":
		var x StaticItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: static: %w", err)
		}
		e.Static = &x
	case "macro":
		var x MacroItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: macro: %w", err)
		}
		e.Macro = &x
	case "proc_macro":
		var x ProcMacroItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: proc_macro: %w", err)
		}
		e.ProcMacro = &x
	case "primitive":
		var x PrimitiveItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: primitive: %w", err)
		}
		e.Primitive = &x
	case "assoc_const":
		var x AssocConstItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: assoc_const: %w", err)
		}
		e.AssocConst = &x
	case "assoc_type":
		var x AssocTypeItem
		if err := json.Unmarshal(raw, &x); err != nil {
			return fmt.Errorf("rustdoc: assoc_type: %w", err)
		}
		e.AssocType = &x
	default:
		e.Unknown = tag
	}
	return nil
}

// ModuleItem holds a module's child Item IDs.
type ModuleItem struct {
	IsCrate    bool     `json:"is_crate"`
	Items      []string `json:"items"`
	IsStripped bool     `json:"is_stripped"`
}

// ExternCrateItem records an `extern crate` declaration.
type ExternCrateItem struct {
	Name   string `json:"name"`
	Rename string `json:"rename,omitempty"`
}

// UseItem records a `use ...;` re-export.
type UseItem struct {
	Source string `json:"source"`
	Name   string `json:"name"`
	ID     string `json:"id,omitempty"`
	IsGlob bool   `json:"is_glob"`
}

// StructItem is the payload of a `struct` declaration.
type StructItem struct {
	Kind         StructKind `json:"kind"`
	Generics     Generics   `json:"generics"`
	Impls        []string   `json:"impls,omitempty"`
}

// StructKind discriminates plain / tuple / unit structs.
type StructKind struct {
	Plain   *StructPlain `json:"plain,omitempty"`
	Tuple   []*string    `json:"tuple,omitempty"` // nil = unit struct?
	Unit    *struct{}    `json:"unit,omitempty"`
}

// StructPlain holds plain-struct field IDs.
type StructPlain struct {
	Fields            []string `json:"fields"`
	HasStrippedFields bool     `json:"has_stripped_fields"`
}

// EnumItem is the payload of an `enum` declaration.
type EnumItem struct {
	Generics       Generics `json:"generics"`
	Variants       []string `json:"variants"`
	HasStrippedVariants bool `json:"has_stripped_variants"`
	Impls          []string `json:"impls,omitempty"`
}

// VariantItem is the payload of an enum-variant Item.
type VariantItem struct {
	Kind          VariantKind     `json:"kind"`
	Discriminant  *Discriminant   `json:"discriminant,omitempty"`
}

// VariantKind discriminates plain / tuple / struct variants.
type VariantKind struct {
	Plain  *struct{}     `json:"plain,omitempty"`
	Tuple  []*string     `json:"tuple,omitempty"`
	Struct *VariantStruct `json:"struct,omitempty"`
}

// VariantStruct holds field IDs for a struct-shaped variant.
type VariantStruct struct {
	Fields            []string `json:"fields"`
	HasStrippedFields bool     `json:"has_stripped_fields"`
}

// Discriminant carries the user-declared discriminant for an enum variant.
type Discriminant struct {
	Expr  string `json:"expr"`
	Value string `json:"value"`
}

// FunctionItem holds a fn signature plus declarative metadata.
type FunctionItem struct {
	Sig         FunctionSig   `json:"sig"`
	Generics    Generics      `json:"generics"`
	Header      FunctionHeader `json:"header"`
	HasBody     bool          `json:"has_body"`
}

// FunctionSig is the input/output shape.
type FunctionSig struct {
	Inputs    []FunctionInput `json:"inputs"`
	Output    *Type           `json:"output,omitempty"`
	IsCVariadic bool          `json:"is_c_variadic"`
}

// FunctionInput is one positional parameter.
type FunctionInput struct {
	Name string `json:"-"`
	Type Type   `json:"-"`
}

// UnmarshalJSON expects the wire shape ["name", Type] (rustdoc-json
// represents fn inputs as a 2-tuple).
func (fi *FunctionInput) UnmarshalJSON(data []byte) error {
	var arr []json.RawMessage
	if err := json.Unmarshal(data, &arr); err != nil {
		return fmt.Errorf("rustdoc: function input: %w", err)
	}
	if len(arr) != 2 {
		return fmt.Errorf("rustdoc: function input: want 2-tuple, got %d", len(arr))
	}
	if err := json.Unmarshal(arr[0], &fi.Name); err != nil {
		return fmt.Errorf("rustdoc: function input name: %w", err)
	}
	if err := json.Unmarshal(arr[1], &fi.Type); err != nil {
		return fmt.Errorf("rustdoc: function input type: %w", err)
	}
	return nil
}

// FunctionHeader is the per-fn modifier set (unsafe, const, async, abi).
type FunctionHeader struct {
	IsConst   bool   `json:"is_const"`
	IsUnsafe  bool   `json:"is_unsafe"`
	IsAsync   bool   `json:"is_async"`
	ABI       ABI    `json:"abi"`
}

// ABI is the FFI calling convention. The wire form is either the
// literal string "Rust" / "C" / "system" / ... or a tagged-object form
// like {"C": {"unwind": true}}.
type ABI struct {
	Name   string
	Unwind bool
}

// UnmarshalJSON handles both the bare-string and object forms.
func (a *ABI) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err == nil {
		a.Name = s
		return nil
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		return fmt.Errorf("rustdoc: abi: %w", err)
	}
	for k, raw := range obj {
		a.Name = k
		var inner struct {
			Unwind bool `json:"unwind"`
		}
		_ = json.Unmarshal(raw, &inner)
		a.Unwind = inner.Unwind
		return nil
	}
	return nil
}

// TraitItem is the payload of a `trait` declaration.
type TraitItem struct {
	IsAuto      bool     `json:"is_auto"`
	IsUnsafe    bool     `json:"is_unsafe"`
	Items       []string `json:"items"`
	Generics    Generics `json:"generics"`
	Bounds      []GenericBound `json:"bounds,omitempty"`
	Implementations []string `json:"implementations,omitempty"`
}

// ImplItem is the payload of an `impl` block.
type ImplItem struct {
	IsUnsafe   bool       `json:"is_unsafe"`
	Generics   Generics   `json:"generics"`
	ProvidedTraitMethods []string `json:"provided_trait_methods,omitempty"`
	Trait      *PathType  `json:"trait,omitempty"`
	For        Type       `json:"for"`
	Items      []string   `json:"items"`
	Negative   bool       `json:"negative"`
	Synthetic  bool       `json:"synthetic"`
	BlanketImpl *Type     `json:"blanket_impl,omitempty"`
}

// TypeAliasItem is the payload of `type Foo = Bar;`.
type TypeAliasItem struct {
	Type     Type     `json:"type"`
	Generics Generics `json:"generics"`
}

// ConstantItem is the payload of `const FOO: Ty = ...`.
type ConstantItem struct {
	Type  Type     `json:"type"`
	Const ConstValue `json:"const"`
}

// StaticItem is the payload of `static FOO: Ty = ...`.
type StaticItem struct {
	Type     Type   `json:"type"`
	Mutable  bool   `json:"mutable"`
	Expr     string `json:"expr,omitempty"`
	IsUnsafe bool   `json:"is_unsafe"`
}

// ConstValue captures `expr = ...` / `value = ...` / `is_literal`.
type ConstValue struct {
	Expr      string `json:"expr"`
	Value     string `json:"value,omitempty"`
	IsLiteral bool   `json:"is_literal"`
}

// MacroItem is the source text of a declarative macro.
type MacroItem string

// UnmarshalJSON accepts both a bare string and a wrapper object.
func (m *MacroItem) UnmarshalJSON(data []byte) error {
	var s string
	if err := json.Unmarshal(data, &s); err == nil {
		*m = MacroItem(s)
		return nil
	}
	var obj map[string]json.RawMessage
	if err := json.Unmarshal(data, &obj); err != nil {
		return fmt.Errorf("rustdoc: macro: %w", err)
	}
	if raw, ok := obj["macro_definition"]; ok {
		var s string
		if err := json.Unmarshal(raw, &s); err == nil {
			*m = MacroItem(s)
			return nil
		}
	}
	return nil
}

// ProcMacroItem describes a `#[proc_macro]` attribute / derive macro.
type ProcMacroItem struct {
	Kind    string   `json:"kind"`
	Helpers []string `json:"helpers,omitempty"`
}

// PrimitiveItem is a primitive type entry (rare; usually from std).
type PrimitiveItem struct {
	Name  string   `json:"name"`
	Impls []string `json:"impls,omitempty"`
}

// AssocConstItem is an associated const inside a trait or impl.
type AssocConstItem struct {
	Type  Type   `json:"type"`
	Value string `json:"value,omitempty"`
}

// AssocTypeItem is an associated type inside a trait or impl.
type AssocTypeItem struct {
	Generics Generics `json:"generics"`
	Bounds   []GenericBound `json:"bounds,omitempty"`
	Default  *Type    `json:"default,omitempty"`
	Type     *Type    `json:"type,omitempty"`
}
