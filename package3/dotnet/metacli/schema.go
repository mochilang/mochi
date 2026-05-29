// Package metacli defines the JSON schema output by mochi-dotnet-meta (a
// .NET CLI tool invoked at lock time) and provides parsers and surface
// extractors for that output. The Go code here is the consumer side only;
// the .NET tool that produces the JSON is a separate project.
package metacli

// AssemblyMetadata is the top-level JSON document output by mochi-dotnet-meta.
type AssemblyMetadata struct {
	Assembly        string    `json:"assembly"`
	Version         string    `json:"version"`
	TargetFramework string    `json:"target_framework"`
	Types           []TypeDef `json:"types"`
}

// TypeDefKind classifies .NET type definitions.
type TypeDefKind string

const (
	KindClass     TypeDefKind = "class"
	KindStruct    TypeDefKind = "struct"
	KindInterface TypeDefKind = "interface"
	KindEnum      TypeDefKind = "enum"
	KindDelegate  TypeDefKind = "delegate"
)

// TypeDef represents a public type exported by the assembly.
type TypeDef struct {
	Namespace  string      `json:"namespace"`
	Name       string      `json:"name"`
	Kind       TypeDefKind `json:"kind"`
	IsAbstract bool        `json:"is_abstract,omitempty"`
	IsSealed   bool        `json:"is_sealed,omitempty"`
	IsStatic   bool        `json:"is_static,omitempty"`
	IsObsolete bool        `json:"is_obsolete,omitempty"`
	IsGeneric  bool        `json:"is_generic,omitempty"`
	TypeParams []string    `json:"type_params,omitempty"`
	BaseType   string      `json:"base_type,omitempty"`
	Interfaces []string    `json:"interfaces,omitempty"`
	Methods    []MethodDef `json:"methods,omitempty"`
	Properties []PropDef   `json:"properties,omitempty"`
	Fields     []FieldDef  `json:"fields,omitempty"`
	EnumValues []EnumValue `json:"enum_values,omitempty"`
	XmlDoc     string      `json:"xml_doc,omitempty"`
}

// MethodDef represents a public method on a type.
type MethodDef struct {
	Name       string     `json:"name"`
	IsStatic   bool       `json:"is_static,omitempty"`
	IsAsync    bool       `json:"is_async,omitempty"`
	IsObsolete bool       `json:"is_obsolete,omitempty"`
	IsGeneric  bool       `json:"is_generic,omitempty"`
	TypeParams []string   `json:"type_params,omitempty"`
	ReturnType TypeRef    `json:"return_type"`
	Parameters []ParamDef `json:"parameters,omitempty"`
	XmlDoc     string     `json:"xml_doc,omitempty"`
}

// PropDef represents a public property.
type PropDef struct {
	Name       string  `json:"name"`
	Type       TypeRef `json:"type"`
	HasGetter  bool    `json:"has_getter,omitempty"`
	HasSetter  bool    `json:"has_setter,omitempty"`
	IsStatic   bool    `json:"is_static,omitempty"`
	IsObsolete bool    `json:"is_obsolete,omitempty"`
	IsIndexer  bool    `json:"is_indexer,omitempty"`
	XmlDoc     string  `json:"xml_doc,omitempty"`
}

// FieldDef represents a public field.
type FieldDef struct {
	Name       string  `json:"name"`
	Type       TypeRef `json:"type"`
	IsStatic   bool    `json:"is_static,omitempty"`
	IsConst    bool    `json:"is_const,omitempty"`
	IsReadOnly bool    `json:"is_readonly,omitempty"`
	XmlDoc     string  `json:"xml_doc,omitempty"`
}

// ParamDef represents a method parameter.
type ParamDef struct {
	Name       string  `json:"name"`
	Type       TypeRef `json:"type"`
	IsOut      bool    `json:"is_out,omitempty"`
	IsRef      bool    `json:"is_ref,omitempty"`
	IsParams   bool    `json:"is_params,omitempty"`
	HasDefault bool    `json:"has_default,omitempty"`
}

// EnumValue represents a named constant in an enum.
type EnumValue struct {
	Name  string `json:"name"`
	Value int64  `json:"value"`
}

// TypeRef is a reference to a CLR type (may be generic).
type TypeRef struct {
	// FullName is the fully-qualified CLR type name,
	// e.g. "System.Collections.Generic.List`1".
	FullName string `json:"full_name"`
	// Kind is the type kind hint.
	Kind TypeRefKind `json:"kind"`
	// TypeArgs are the generic arguments (for generic_inst kind).
	TypeArgs []TypeRef `json:"type_args,omitempty"`
	// IsNullable is true for Nullable<T> or T? reference-nullable types.
	IsNullable bool `json:"is_nullable,omitempty"`
	// ArrayRank is non-zero for array types (1 = T[], 2 = T[,]).
	ArrayRank int `json:"array_rank,omitempty"`
}

// TypeRefKind classifies how a TypeRef is used.
type TypeRefKind string

const (
	TypeRefPrimitive    TypeRefKind = "primitive"
	TypeRefClass        TypeRefKind = "class"
	TypeRefStruct       TypeRefKind = "struct"
	TypeRefInterface    TypeRefKind = "interface"
	TypeRefEnum         TypeRefKind = "enum"
	TypeRefArray        TypeRefKind = "array"
	TypeRefGenericInst  TypeRefKind = "generic_inst"
	TypeRefPointer      TypeRefKind = "pointer"
	TypeRefByRef        TypeRefKind = "byref"
	TypeRefGenericParam TypeRefKind = "generic_param"
	TypeRefVoid         TypeRefKind = "void"
	TypeRefDelegate     TypeRefKind = "delegate"
	TypeRefDynamic      TypeRefKind = "dynamic"
)
