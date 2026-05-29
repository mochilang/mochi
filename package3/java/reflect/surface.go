// Package reflect provides the Java reflection tool and surface types for MEP-67.
// The reflection tool runs a bundled JAR (reflect-tool.jar) against a target JAR
// to enumerate its public API, returning a Surface (JSON) that the type-mapping
// and wrapper-synthesis stages consume.
package reflect

// Surface is the top-level result of the reflection tool. It describes the
// public API of a JAR as a list of ClassInfo entries.
type Surface struct {
	// SchemaVersion is the JSON schema version emitted by the tool.
	SchemaVersion int `json:"schemaVersion"`
	// Classes is the list of public classes found in the JAR.
	Classes []ClassInfo `json:"classes"`
}

// ClassInfo describes a single class, interface, or enum.
type ClassInfo struct {
	// Name is the binary class name, e.g. "com.google.common.base.Optional".
	Name string `json:"name"`
	// Modifiers is the list of modifier tokens, e.g. ["public", "abstract"].
	Modifiers []string `json:"modifiers"`
	// Superclass is the binary name of the direct superclass. May be empty.
	Superclass string `json:"superclass,omitempty"`
	// Interfaces is the list of implemented interface names.
	Interfaces []string `json:"interfaces"`
	// Methods is the list of public methods.
	Methods []MethodInfo `json:"methods"`
	// Fields is the list of public fields.
	Fields []FieldInfo `json:"fields"`
	// Constructors is the list of public constructors.
	Constructors []ConstructorInfo `json:"constructors"`
}

// MethodInfo describes a single method on a class.
type MethodInfo struct {
	// Name is the method name.
	Name string `json:"name"`
	// Modifiers is the list of modifier tokens.
	Modifiers []string `json:"modifiers"`
	// ReturnType is the erased return type name.
	ReturnType string `json:"returnType"`
	// ParameterTypes is the list of erased parameter type names.
	ParameterTypes []string `json:"parameterTypes"`
	// ParameterNames is the list of parameter names (may be empty if not available).
	ParameterNames []string `json:"parameterNames,omitempty"`
	// GenericReturnType is the generic signature of the return type.
	GenericReturnType string `json:"genericReturnType,omitempty"`
	// GenericParameterTypes is the list of generic parameter type signatures.
	GenericParameterTypes []string `json:"genericParameterTypes,omitempty"`
	// ExceptionTypes is the list of declared checked exception type names.
	ExceptionTypes []string `json:"exceptionTypes"`
	// IsVarargs is true if the last parameter is a varargs.
	IsVarargs bool `json:"isVarargs"`
	// IsDeprecated is true if the method is annotated @Deprecated.
	IsDeprecated bool `json:"isDeprecated"`
}

// FieldInfo describes a single field on a class.
type FieldInfo struct {
	// Name is the field name.
	Name string `json:"name"`
	// Modifiers is the list of modifier tokens.
	Modifiers []string `json:"modifiers"`
	// Type is the erased field type name.
	Type string `json:"type"`
	// GenericType is the generic field type signature.
	GenericType string `json:"genericType,omitempty"`
}

// ConstructorInfo describes a single constructor on a class.
type ConstructorInfo struct {
	// Modifiers is the list of modifier tokens.
	Modifiers []string `json:"modifiers"`
	// ParameterTypes is the list of erased parameter type names.
	ParameterTypes []string `json:"parameterTypes"`
	// ParameterNames is the list of parameter names.
	ParameterNames []string `json:"parameterNames,omitempty"`
	// IsVarargs is true if the last parameter is a varargs.
	IsVarargs bool `json:"isVarargs"`
}
