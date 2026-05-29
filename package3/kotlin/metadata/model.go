// Package metadata extracts the Kotlin API surface from @kotlin.Metadata annotations
// inside JAR class files without spawning a JVM.
package metadata

// ClassKind identifies the Kotlin class category.
type ClassKind int

const (
	ClassKindClass         ClassKind = iota
	ClassKindInterface
	ClassKindObject        // singleton object
	ClassKindEnumClass
	ClassKindDataClass
	ClassKindSealedClass
	ClassKindCompanionObject
	ClassKindAnnotationClass
	ClassKindValueClass    // inline/value class
	ClassKindJavaClass     // Java class without @kotlin.Metadata
	ClassKindFileFacade    // top-level functions file
)

// APIObject is the language-neutral Kotlin API surface for a single class or file.
type APIObject struct {
	ClassName    string      // fully qualified, dots (e.g. "kotlin.collections.List")
	JVMClassName string      // JVM internal name, slashes (e.g. "kotlin/collections/List")
	Kind         ClassKind
	Functions    []Function
	Properties   []Property
	Constructors []Constructor
	Nested       []*APIObject
	SealedSubs   []*APIObject // direct sealed subclasses
	EnumEntries  []string     // for ClassKindEnumClass
	// MetadataSchemaVersion is the [major, minor, patch] from @kotlin.Metadata.mv.
	MetadataSchemaVersion [3]int32
}

// FunctionFlags holds visibility and modality flags for a Kotlin function.
type FunctionFlags struct {
	IsPublic      bool
	IsInternal    bool
	IsPrivate     bool
	IsProtected   bool
	IsFinal       bool
	IsOpen        bool
	IsAbstract    bool
	IsInline      bool
	IsOperator    bool
	IsInfix       bool
	IsExternal    bool
	IsTailrec     bool
	IsSuspend     bool
	IsExpect      bool
}

// Function describes a single Kotlin function in the API surface.
type Function struct {
	Name           string
	JVMName        string      // may differ from Name due to @JvmName
	JVMDescriptor  string      // JVM method descriptor, e.g. "(ILjava/lang/String;)Ljava/util/List;"
	Receiver       *KotlinType // nil for non-extension functions
	Params         []Param
	TypeParams     []TypeParam
	ReturnType     KotlinType
	Flags          FunctionFlags
}

// Property describes a Kotlin property (val or var).
type Property struct {
	Name       string
	Type       KotlinType
	IsVar      bool
	IsLateinit bool
	HasBacking bool
}

// Constructor describes a Kotlin constructor.
type Constructor struct {
	IsPrimary bool
	Params    []Param
}

// Param is a single function parameter.
type Param struct {
	Name    string
	Type    KotlinType
	HasDefault bool
}

// TypeParam is a generic type parameter.
type TypeParam struct {
	Name       string
	UpperBound *KotlinType // nil = Any?
}

// KotlinType represents a Kotlin type reference.
type KotlinType struct {
	ClassName    string       // fully qualified, dots; empty if IsTypeParam
	Nullable     bool
	TypeArgs     []KotlinType
	IsTypeParam  bool         // true when this refers to an unresolved type parameter
	TypeParamName string      // name of the type parameter if IsTypeParam
	IsStarProjection bool
}
