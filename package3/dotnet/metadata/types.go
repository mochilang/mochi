// types.go defines the Go data model for .NET assembly public API surfaces,
// as extracted from ECMA-335 CLI metadata tables. This model is the output of
// the metadata parser and the input to the typemap translation pass.
package metadata

// AssemblyMeta is the top-level representation of a parsed .NET assembly's
// public surface.
type AssemblyMeta struct {
	Name          string
	Version       [4]uint16 // major, minor, build, revision
	Culture       string    // empty for invariant culture
	PublicKeyToken []byte   // 8-byte strong-name token, nil if unsigned
	TargetFramework string  // from [assembly: TargetFramework("...")] attribute
	Types         []TypeMeta
}

// TypeKind classifies the fundamental kind of a .NET type.
type TypeKind int

const (
	TypeKindClass     TypeKind = iota // class (reference type)
	TypeKindStruct                    // struct (value type)
	TypeKindEnum                      // enum (integer-backed)
	TypeKindInterface                 // interface
	TypeKindDelegate                  // delegate
	TypeKindRecord                    // C# 9+ record class or record struct
)

// TypeMeta represents a single public type exported by an assembly.
type TypeMeta struct {
	Namespace    string
	Name         string
	FullName     string // Namespace + "." + Name
	Kind         TypeKind
	IsSealed     bool
	IsAbstract   bool
	IsGeneric    bool   // has unbound generic type parameters
	GenericArity int    // number of generic type parameters (e.g. 1 for List<T>)
	BaseType     *TypeRef
	Interfaces   []TypeRef
	Fields       []FieldMeta
	Methods      []MethodMeta
	Properties   []PropertyMeta
	NestedTypes  []TypeMeta
	IsNullable   bool       // NullableAttribute(1) = not-null, (2) = nullable
	IsObsolete   bool
	DocSummary   string

	// NativeAOTCompatible is set by the pre-check pass (package3/dotnet/typemap).
	// nil = not yet checked, true = compatible, false = requires CoreCLR fallback.
	NativeAOTCompatible *bool
}

// TypeRef is a reference to a type that may be in a different assembly.
type TypeRef struct {
	Namespace  string
	Name       string
	Assembly   string // assembly name; empty if in the same assembly
	IsGenericInst bool     // e.g. List<string>
	TypeArgs   []TypeSig   // generic type arguments
}

// FieldMeta represents a public field on a type.
type FieldMeta struct {
	Name       string
	Type       TypeSig
	IsStatic   bool
	IsConst    bool
	IsReadOnly bool
	DocSummary string
}

// MethodMeta represents a public method on a type.
type MethodMeta struct {
	Name         string
	IsStatic     bool
	IsConstructor bool // name is ".ctor"
	IsAbstract   bool
	IsVirtual    bool
	IsOverride   bool
	ReturnType   TypeSig
	Params       []ParamMeta
	IsAsync      bool   // return type is Task<T> or ValueTask<T>
	IsGeneric    bool   // has generic type parameters
	GenericArity int
	IsObsolete   bool
	HasRequiresDynamicCode bool // [RequiresDynamicCode] annotation
	DocSummary   string
}

// ParamMeta represents a single parameter of a method.
type ParamMeta struct {
	Name       string
	Type       TypeSig
	IsIn       bool  // in parameter
	IsOut      bool  // out parameter
	IsRef      bool  // ref parameter
	IsOptional bool
	HasDefault bool
	DefaultVal any // string, int, float64, bool, or nil
	IsNullable bool        // NullableAttribute(2) on this parameter
	DocSummary string
}

// PropertyMeta represents a public property on a type.
type PropertyMeta struct {
	Name       string
	Type       TypeSig
	HasGetter  bool
	HasSetter  bool
	IsStatic   bool
	IsAbstract bool
	IsNullable bool
	DocSummary string
}

// TypeSig is the decoded form of a type signature from the ECMA-335 #Blob heap.
// It represents one resolved type (primitive, reference, generic instantiation,
// array, etc.).
type TypeSig struct {
	Kind     TypeSigKind
	ElemType *TypeSig  // element type for Array, SZArray, Ptr, ByRef
	TypeRef  *TypeRef  // for Class, ValueType, GenericInst
	GenericParam int   // index into the enclosing type/method's generic params
	// For GenericInst: TypeRef is the open generic type, and TypeArgs is populated.
	TypeArgs []TypeSig
	IsNullable bool // from NullableAttribute on this position
}

// TypeSigKind enumerates the ECMA-335 ELEMENT_TYPE_* codes used by the bridge.
type TypeSigKind int

const (
	SigVoid       TypeSigKind = 0x01
	SigBoolean    TypeSigKind = 0x02
	SigChar       TypeSigKind = 0x03
	SigI1         TypeSigKind = 0x04
	SigU1         TypeSigKind = 0x05
	SigI2         TypeSigKind = 0x06
	SigU2         TypeSigKind = 0x07
	SigI4         TypeSigKind = 0x08
	SigU4         TypeSigKind = 0x09
	SigI8         TypeSigKind = 0x0A
	SigU8         TypeSigKind = 0x0B
	SigR4         TypeSigKind = 0x0C
	SigR8         TypeSigKind = 0x0D
	SigString     TypeSigKind = 0x0E
	SigPtr        TypeSigKind = 0x0F
	SigByRef      TypeSigKind = 0x10
	SigValueType  TypeSigKind = 0x11
	SigClass      TypeSigKind = 0x12
	SigVar        TypeSigKind = 0x13 // generic type parameter of enclosing type
	SigArray      TypeSigKind = 0x14 // multi-dimensional array
	SigGenericInst TypeSigKind = 0x15
	SigTypedByRef TypeSigKind = 0x16
	SigI          TypeSigKind = 0x18 // native int (nint)
	SigU          TypeSigKind = 0x19 // native uint (nuint)
	SigFnPtr      TypeSigKind = 0x1B
	SigObject     TypeSigKind = 0x1C // System.Object
	SigSZArray    TypeSigKind = 0x1D // single-dim zero-lower-bound array (T[])
	SigMVar       TypeSigKind = 0x1E // generic type parameter of enclosing method
	SigCModReqd   TypeSigKind = 0x1F // required custom modifier
	SigCModOpt    TypeSigKind = 0x20 // optional custom modifier
)
