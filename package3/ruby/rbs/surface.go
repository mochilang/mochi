// Package rbs parses RBS type-signature files bundled in Ruby gems and from
// gem_rbs_collection, extracts a machine-readable API surface, and hands it
// to the type-mapping pass. Phase 2 (bundled sigs) and phase 3
// (gem_rbs_collection fallback) are implemented here.
//
// See [website/docs/research/0076/04-rbs-yard-ingest.md] for background.
package rbs

// MethodKind classifies the receiver of an RBS method declaration.
type MethodKind int

const (
	MethodInstance MethodKind = iota // def method_name: ...
	MethodSingleton                  // def self.method_name: ...
	MethodBoth                       // def (self.)method_name: ...
)

// Type is a simplified, closed representation of an RBS type node. The bridge
// only needs the subset of RBS that the type-mapping pass can translate; nodes
// outside the closed set are represented as TypeUnknown.
type Type struct {
	Kind    TypeKind
	Elem    *Type  // for Array[T], Optional[T], etc.
	Key     *Type  // for Hash[K, V]
	Value   *Type  // for Hash[K, V]
	Members []*Type // for tuple types
	Params  []*Type // for proc/lambda parameter types
	Return  *Type  // for proc/lambda return type
	Name    string  // for TypeNamed (class/module names)
}

// TypeKind enumerates every RBS type node the bridge handles or refuses.
type TypeKind int

const (
	TypeUnknown    TypeKind = iota // anything not in the closed set
	TypeInteger                    // Integer
	TypeFloat                      // Float
	TypeString                     // String
	TypeSymbol                     // Symbol (maps to Mochi string)
	TypeBool                       // bool (RBS literal true | false)
	TypeNil                        // nil
	TypeVoid                       // void (return position only)
	TypeArray                      // Array[T]
	TypeHash                       // Hash[K, V]
	TypeOptional                   // T?  or  T | nil
	TypeProc                       // ^(A, ...) -> R
	TypeTuple                      // [A, B, ...]
	TypeNamed                      // a named class / interface / alias
	TypeTop                        // top  (refused)
	TypeBot                        // bot  (refused)
	TypeSelf                       // self (refused)
	TypeInstance                   // instance (refused)
	TypeClass                      // class (refused)
	TypeUntyped                    // untyped (refused)
)

// Param is one formal parameter in an RBS method type.
type Param struct {
	Name     string
	Type     *Type
	Optional bool // trailing `?` on the parameter name
}

// MethodType is a single overload signature for an RBS method.
type MethodType struct {
	Params  []Param
	Return  *Type
	Block   *BlockType // nil if no block parameter
}

// BlockType is an RBS block `{ (A) -> B }`.
type BlockType struct {
	Params []Param
	Return *Type
}

// MethodDecl is one method or attribute from the RBS surface.
type MethodDecl struct {
	Name       string
	Kind       MethodKind
	Types      []MethodType // one per overload
	IsAttr     bool         // true for attr_reader / attr_accessor / attr_writer
	IsOptional bool         // trailing `?` method name (predicate methods)
}

// ClassDecl groups the methods declared in one RBS class or module.
type ClassDecl struct {
	Name    string       // qualified name, e.g. "Nokogiri::HTML::Document"
	Methods []MethodDecl
	Attrs   []MethodDecl // attr_reader / attr_accessor
}

// GemSurface is the full machine-readable API surface extracted from a gem's
// RBS files. It is the output of Parse and the input to the type-mapping pass.
type GemSurface struct {
	Gem        string
	Version    string
	RBSVersion string      // schema version from the rbs gem, e.g. "3"
	RBSSHA256  string      // SHA-256 of the .rbs corpus used
	Source     SurfaceSource
	Classes    []ClassDecl
}

// SurfaceSource indicates where the RBS data came from.
type SurfaceSource int

const (
	SourceBundled         SurfaceSource = iota // sig/**/*.rbs inside the gem tarball
	SourceGemRBSCollection                     // gem_rbs_collection community sigs
	SourceYARD                                 // YARD fallback (lower fidelity)
	SourceNone                                 // no type information found
)
