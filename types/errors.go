package types

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

var Errors = map[string]diagnostic.Template{
	// --- Declarations & Scope ---
	"T000": {Code: "T000", Message: "`let` requires a type or a value", Help: "Use `let x = ...` or `let x: int` to declare a variable."},
	"T001": {Code: "T001", Message: "assignment to undeclared variable: %s", Help: "Declare `%s` first using `let`."},
	"T002": {Code: "T002", Message: "undefined variable: %s", Help: "Check if the variable was declared in this scope."},

	// --- Functions ---
	"T003": {Code: "T003", Message: "unknown function: %s", Help: "Ensure the function is defined before it's called."},
	"T004": {Code: "T004", Message: "`%s` is not callable", Help: "Use a function or closure in this position."},
	"T005": {Code: "T005", Message: "parameter `%s` is missing a type", Help: "Add a type like `x: int` to this parameter."},
	"T006": {Code: "T006", Message: "too many arguments: expected %d, got %d", Help: "Remove extra arguments or update the function definition."},
	"T007": {Code: "T007", Message: "argument %d: expected %s, got %s", Help: "Ensure the argument type matches the function’s signature."},

	// --- Type Mismatches ---
	"T008": {Code: "T008", Message: "type mismatch: expected %s, got %s", Help: "Change the value to match the expected type."},
	"T009": {Code: "T009", Message: "cannot assign %s to `%s` (expected %s)", Help: "Make sure the assigned value is compatible with `%s`."},
	"T010": {Code: "T010", Message: "return type mismatch: expected %s, got %s", Help: "Update the return value to match the function's return type."},

	// --- Boolean and Condition Checks ---
	"T011": {Code: "T011", Message: "`expect` must be a boolean", Help: "`expect` should evaluate to `true` or `false`."},
	"T012": {Code: "T012", Message: "incompatible types in equality comparison", Help: "Use `==` or `!=` only with values of the same type."},
	"T013": {Code: "T013", Message: "incompatible types in comparison", Help: "Use comparable types like numbers or strings with `<`, `>`."},

	// --- Expressions ---
	"T014": {Code: "T014", Message: "invalid primary expression", Help: "Expected a complete value or expression (e.g., literal, variable, function)."},
	"T020": {Code: "T020", Message: "operator `%s` cannot be used on types %s and %s", Help: "Choose an operator that supports these operand types."},
	"T021": {Code: "T021", Message: "unsupported operator: `%s`", Help: "Use a valid operator like +, -, ==, or <."},

	// --- Indexing ---
	"T015": {Code: "T015", Message: "index must be an integer", Help: "Use an `int` value for indexing (e.g., `list[0]`)."},
	"T016": {Code: "T016", Message: "missing index expression", Help: "Provide an index inside `[ ... ]`."},
	"T017": {Code: "T017", Message: "slicing not allowed on map", Help: "Maps do not support slicing like `map[a:b]`."},
	"T018": {Code: "T018", Message: "type %s does not support indexing", Help: "Only `list<T>` and `map<K,V>` can be indexed."},
	"T019": {Code: "T019", Message: "map key type mismatch: expected %s, got %s", Help: "Make sure the key matches the map’s key type."},
	// --- For Loops ---
	"T022": {Code: "T022", Message: "cannot iterate over type %s", Help: "Only `list<T>`, `map<K,V>`, or integer ranges are allowed in `for ... in ...` loops."},
	"T023": {Code: "T023", Message: "range loop requires integer start and end expressions", Help: "Use `for i in 0..10` or ensure both expressions are of type `int`."},
	"T024": {Code: "T024", Message: "cannot assign to `%s` (immutable)", Help: "Use `var` to declare mutable variables."},
	"T025": {Code: "T025", Message: "unknown type: %s", Help: "Ensure the type is defined before use."},
	"T026": {Code: "T026", Message: "unknown field `%s` on %s", Help: "Check the struct definition for valid fields."},
	"T027": {Code: "T027", Message: "%s is not a struct", Help: "Field access is only valid on struct types."},
	"T028": {Code: "T028", Message: "fetch URL must be a string", Help: "Use a string literal or variable of type string."},
	"T029": {Code: "T029", Message: "fetch options must be a map", Help: "Pass a map like `{\"method\": \"POST\"}` after `with`."},
	"T030": {Code: "T030", Message: "invalid type for fetch option `%s`: expected %s, got %s", Help: "Ensure the option value matches the expected type."},
	"T031": {Code: "T031", Message: "unknown stream: %s", Help: "Declare the stream before using it."},
}

// --- Wrapper Functions ---

func errLetMissingTypeOrValue(pos lexer.Position) error {
	return Errors["T000"].New(pos)
}

func errAssignUndeclared(pos lexer.Position, name string) error {
	return Errors["T001"].New(pos, name)
}

func errUnknownVariable(pos lexer.Position, name string) error {
	return Errors["T002"].New(pos, name)
}

func errUnknownFunction(pos lexer.Position, name string) error {
	return Errors["T003"].New(pos, name)
}

func errNotFunction(pos lexer.Position, name string) error {
	return Errors["T004"].New(pos, name)
}

func errParamMissingType(pos lexer.Position, name string) error {
	return Errors["T005"].New(pos, name)
}

func errTooManyArgs(pos lexer.Position, expected, actual int) error {
	return Errors["T006"].New(pos, expected, actual)
}

func errArgTypeMismatch(pos lexer.Position, index int, expected, actual Type) error {
	return Errors["T007"].New(pos, index+1, expected, actual)
}

func errTypeMismatch(pos lexer.Position, expected, actual Type) error {
	return Errors["T008"].New(pos, expected, actual)
}

func errCannotAssign(pos lexer.Position, rhs Type, lhsName string, lhs Type) error {
	return Errors["T009"].New(pos, rhs, lhsName, lhs)
}

func errReturnMismatch(pos lexer.Position, expected, actual Type) error {
	return Errors["T010"].New(pos, expected, actual)
}

func errExpectBoolean(pos lexer.Position) error {
	return Errors["T011"].New(pos)
}

//func errIncompatibleEquality(pos lexer.Position) error {
//	return Errors["T012"].New(pos)
//}

func errIncompatibleComparison(pos lexer.Position) error {
	return Errors["T013"].New(pos)
}

func errInvalidPrimary(pos lexer.Position) error {
	return Errors["T014"].New(pos)
}

func errIndexNotInteger(pos lexer.Position) error {
	return Errors["T015"].New(pos)
}

func errMissingIndex(pos lexer.Position) error {
	return Errors["T016"].New(pos)
}

func errInvalidMapSlice(pos lexer.Position) error {
	return Errors["T017"].New(pos)
}

func errNotIndexable(pos lexer.Position, typ Type) error {
	return Errors["T018"].New(pos, typ)
}

func errIndexTypeMismatch(pos lexer.Position, expected, actual Type) error {
	return Errors["T019"].New(pos, expected, actual)
}

func errOperatorMismatch(pos lexer.Position, op string, left, right Type) error {
	return Errors["T020"].New(pos, op, left, right)
}

func errUnsupportedOperator(pos lexer.Position, op string) error {
	return Errors["T021"].New(pos, op)
}

func errCannotIterate(pos lexer.Position, typ Type) error {
	return Errors["T022"].New(pos, typ)
}

func errRangeRequiresInts(pos lexer.Position) error {
	return Errors["T023"].New(pos)
}

func errAssignImmutableVar(pos lexer.Position, name string) error {
	return Errors["T024"].New(pos, name)
}

func errUnknownType(pos lexer.Position, name string) error {
	return Errors["T025"].New(pos, name)
}

func errUnknownField(pos lexer.Position, field string, typ Type) error {
	return Errors["T026"].New(pos, field, typ)
}

func errNotStruct(pos lexer.Position, typ Type) error {
	return Errors["T027"].New(pos, typ)
}

func errFetchURLString(pos lexer.Position) error {
	return Errors["T028"].New(pos)
}

func errFetchOptsMap(pos lexer.Position) error {
	return Errors["T029"].New(pos)
}

func errFetchOptType(pos lexer.Position, name string, expected, actual Type) error {
	return Errors["T030"].New(pos, name, expected, actual)
}

func errUnknownStream(pos lexer.Position, name string) error {
	return Errors["T031"].New(pos, name)
}
