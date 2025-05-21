package types

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

var Errors = map[string]diagnostic.Template{
	// --- Declarations & Scope ---
	"T000": {"T000", "`let` requires a type or a value", "Use `let x = ...` or `let x: int` to declare a variable."},
	"T001": {"T001", "assignment to undeclared variable: %s", "Declare `%s` first using `let`."},
	"T002": {"T002", "undefined variable: %s", "Check if the variable was declared in this scope."},

	// --- Functions ---
	"T003": {"T003", "unknown function: %s", "Ensure the function is defined before it's called."},
	"T004": {"T004", "`%s` is not callable", "Use a function or closure in this position."},
	"T005": {"T005", "parameter `%s` is missing a type", "Add a type like `x: int` to this parameter."},
	"T006": {"T006", "too many arguments: expected %d, got %d", "Remove extra arguments or update the function definition."},
	"T007": {"T007", "argument %d: expected %s, got %s", "Ensure the argument type matches the function’s signature."},

	// --- Type Mismatches ---
	"T008": {"T008", "type mismatch: expected %s, got %s", "Change the value to match the expected type."},
	"T009": {"T009", "cannot assign %s to `%s` (expected %s)", "Make sure the assigned value is compatible with `%s`."},
	"T010": {"T010", "return type mismatch: expected %s, got %s", "Update the return value to match the function's return type."},

	// --- Boolean and Condition Checks ---
	"T011": {"T011", "`expect` must be a boolean", "`expect` should evaluate to `true` or `false`."},
	"T012": {"T012", "incompatible types in equality comparison", "Use `==` or `!=` only with values of the same type."},
	"T013": {"T013", "incompatible types in comparison", "Use comparable types like numbers or strings with `<`, `>`."},

	// --- Expressions ---
	"T014": {"T014", "invalid primary expression", "Expected a complete value or expression (e.g., literal, variable, function)."},
	"T020": {"T020", "operator `%s` cannot be used on types %s and %s", "Choose an operator that supports these operand types."},
	"T021": {"T021", "unsupported operator: `%s`", "Use a valid operator like +, -, ==, or <."},

	// --- Indexing ---
	"T015": {"T015", "index must be an integer", "Use an `int` value for indexing (e.g., `list[0]`)."},
	"T016": {"T016", "missing index expression", "Provide an index inside `[ ... ]`."},
	"T017": {"T017", "slicing not allowed on map", "Maps do not support slicing like `map[a:b]`."},
	"T018": {"T018", "type %s does not support indexing", "Only `list<T>` and `map<K,V>` can be indexed."},
	"T019": {"T019", "map key type mismatch: expected %s, got %s", "Make sure the key matches the map’s key type."},
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

func errIncompatibleEquality(pos lexer.Position) error {
	return Errors["T012"].New(pos)
}

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
