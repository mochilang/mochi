// Package reflect implements the PHP Reflection API surface extractor for the
// MEP-75 PHP bridge. It invokes a PHP CLI script (reflect.php) via
// exec.Command and parses the emitted JSON surface document.
//
// The surface document schema is defined here. reflect.php emits one
// ReflectionSurface per package root, collecting all public classes,
// interfaces, abstract classes, enums, and top-level functions.
//
// See [website/docs/research/0075/03-prior-art-bridges.md] for the design.
package reflect

// ReflectionSurface is the top-level JSON document emitted by reflect.php for
// a single Composer package directory.
type ReflectionSurface struct {
	// PackageName is the Composer vendor/name string, e.g. "guzzlehttp/guzzle".
	PackageName string `json:"package_name"`
	// PHPVersion is the PHP_VERSION string at reflect time, e.g. "8.4.0".
	PHPVersion string `json:"php_version"`
	// Classes is the list of public class and abstract class definitions.
	Classes []ClassSurface `json:"classes"`
	// Interfaces is the list of interface definitions.
	Interfaces []InterfaceSurface `json:"interfaces"`
	// Enums is the list of enum definitions (PHP 8.1+).
	Enums []EnumSurface `json:"enums"`
	// Functions is the list of top-level function definitions.
	Functions []FunctionSurface `json:"functions"`
	// Errors is the list of reflection errors encountered during extraction.
	// Non-fatal; the caller logs these as warnings.
	Errors []string `json:"errors,omitempty"`
}

// ClassSurface describes a single public class or abstract class.
type ClassSurface struct {
	// FQCN is the fully-qualified class name, e.g. "GuzzleHttp\\Client".
	FQCN string `json:"fqcn"`
	// Abstract is true if the class is declared abstract.
	Abstract bool `json:"abstract"`
	// Final is true if the class is declared final.
	Final bool `json:"final"`
	// ParentFQCN is the FQCN of the parent class, or empty.
	ParentFQCN string `json:"parent_fqcn,omitempty"`
	// InterfaceFQCNs is the list of implemented interface FQCNs.
	InterfaceFQCNs []string `json:"interface_fqcns,omitempty"`
	// Methods is the list of public methods.
	Methods []MethodSurface `json:"methods"`
	// Properties is the list of public properties.
	Properties []PropertySurface `json:"properties"`
	// Constants is the list of public class constants.
	Constants []ConstantSurface `json:"constants,omitempty"`
}

// InterfaceSurface describes a single interface.
type InterfaceSurface struct {
	// FQCN is the fully-qualified interface name.
	FQCN string `json:"fqcn"`
	// ParentFQCNs is the list of parent interfaces.
	ParentFQCNs []string `json:"parent_fqcns,omitempty"`
	// Methods is the list of interface method signatures.
	Methods []MethodSurface `json:"methods"`
	// Constants is the list of interface constants.
	Constants []ConstantSurface `json:"constants,omitempty"`
}

// EnumSurface describes a PHP 8.1+ enum.
type EnumSurface struct {
	// FQCN is the fully-qualified enum name.
	FQCN string `json:"fqcn"`
	// BackingType is "int", "string", or "" for pure enums.
	BackingType string `json:"backing_type,omitempty"`
	// Cases is the list of enum cases.
	Cases []EnumCase `json:"cases"`
	// Methods is the list of enum methods.
	Methods []MethodSurface `json:"methods,omitempty"`
}

// EnumCase is one case in an enum.
type EnumCase struct {
	// Name is the case name, e.g. "Active".
	Name string `json:"name"`
	// Value is the backing value as a string (int or string; empty for pure enums).
	Value string `json:"value,omitempty"`
}

// MethodSurface describes a single public method.
type MethodSurface struct {
	// Name is the method name, e.g. "send".
	Name string `json:"name"`
	// Static is true if the method is static.
	Static bool `json:"static"`
	// Abstract is true if the method is abstract.
	Abstract bool `json:"abstract"`
	// Final is true if the method is final.
	Final bool `json:"final"`
	// Parameters is the ordered list of parameters.
	Parameters []ParameterSurface `json:"parameters"`
	// ReturnType is the declared return type string, or "" if none.
	ReturnType string `json:"return_type,omitempty"`
	// Nullable is true if the return type is nullable.
	Nullable bool `json:"nullable,omitempty"`
}

// PropertySurface describes a single public property.
type PropertySurface struct {
	// Name is the property name (without $).
	Name string `json:"name"`
	// Type is the declared type string, or "" if untyped.
	Type string `json:"type,omitempty"`
	// Nullable is true if the type is nullable.
	Nullable bool `json:"nullable,omitempty"`
	// Static is true if the property is static.
	Static bool `json:"static"`
	// Readonly is true if the property is readonly (PHP 8.1+).
	Readonly bool `json:"readonly,omitempty"`
	// DefaultValue is the string representation of the default value, or "".
	DefaultValue string `json:"default_value,omitempty"`
}

// ParameterSurface describes a single method or function parameter.
type ParameterSurface struct {
	// Name is the parameter name (without $).
	Name string `json:"name"`
	// Type is the declared type string, or "" if untyped.
	Type string `json:"type,omitempty"`
	// Nullable is true if the type is nullable.
	Nullable bool `json:"nullable,omitempty"`
	// Optional is true if the parameter has a default value.
	Optional bool `json:"optional"`
	// Variadic is true for ...$param.
	Variadic bool `json:"variadic"`
	// DefaultValue is the string representation of the default value, or "".
	DefaultValue string `json:"default_value,omitempty"`
}

// FunctionSurface describes a top-level public function.
type FunctionSurface struct {
	// Name is the function name (may include namespace prefix).
	Name string `json:"name"`
	// Parameters is the ordered list of parameters.
	Parameters []ParameterSurface `json:"parameters"`
	// ReturnType is the declared return type string, or "" if none.
	ReturnType string `json:"return_type,omitempty"`
	// Nullable is true if the return type is nullable.
	Nullable bool `json:"nullable,omitempty"`
}

// ConstantSurface describes a class or interface constant.
type ConstantSurface struct {
	// Name is the constant name.
	Name string `json:"name"`
	// Value is the string representation of the constant value.
	Value string `json:"value"`
	// Type is the declared type string (PHP 8.3+), or "".
	Type string `json:"type,omitempty"`
}
