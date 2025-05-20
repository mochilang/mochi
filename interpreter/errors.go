package interpreter

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

var Errors = map[string]diagnostic.Template{
	// --- Variables and Functions ---
	"I000": {"I000", "undefined variable: %s", "`%s` is not defined. Declare it before use."},
	"I001": {"I001", "undefined function or closure: %s", "`%s` is not defined. Declare it before calling."},
	"I002": {"I002", "function %s expects %d arguments, got %d", "Pass exactly %d arguments to `%s`."},
	"I003": {"I003", "internal error: argument count mismatch in closure", "Check the closure's argument handling logic."},

	// --- Literals and Expressions ---
	"I004": {"I004", "invalid primary expression", "Expected a literal, variable, or function call."},
	"I005": {"I005", "invalid literal value", "Check the format or syntax of the literal."},
	"I006": {"I006", "invalid map key: expected string, got %T", "Only `string` keys are allowed in map literals."},
	"I007": {"I007", "cannot access field `%s` on non-object of type %s", "Access fields only on objects or maps."},

	// --- Operators ---
	"I008": {"I008", "cannot apply operator '%s' to types %s and %s", "Use compatible types for the operator."},
	"I009": {"I009", "invalid use of unary '%s' on %s", "Use unary operators only with numbers or booleans."},
	"I010": {"I010", "unknown unary operator: %s", "Supported unary operators are: `-`, `!`."},
	"I011": {"I011", "range bounds must be integers, got %s and %s", "Use integers in `for x in a..b` ranges."},
	"I012": {"I012", "division by zero", "Ensure the denominator is not zero."},

	// --- Indexing and Slicing ---
	"I013": {"I013", "index must be an integer, got %T", "Use an `int` value as an index (e.g., `list[0]`)."},
	"I014": {"I014", "index %d out of bounds for length %d", "Use an index within bounds of the list or string."},
	"I015": {"I015", "invalid slice range [%d:%d] for length %d", "Make sure the slice range is valid and within bounds."},
	"I016": {"I016", "cannot take length of type %s", "Use `len(...)` only on lists, strings, or maps."},
	"I017": {"I017", "cannot take length of type %s", "Use `len(...)` only on lists and strings."},

	// --- Testing ---
	"I018": {"I018", "expect condition failed", "The condition evaluated to `false`."},
}

// --- Variables and Functions ---
func errUndefinedVariable(pos lexer.Position, name string) error {
	return Errors["I000"].New(pos, name)
}
func errUndefinedFunctionOrClosure(pos lexer.Position, name string) error {
	return Errors["I001"].New(pos, name)
}
func errTooManyFunctionArgs(pos lexer.Position, name string, expected, actual int) error {
	return Errors["I002"].New(pos, name, expected, actual)
}
func errInternalClosureArgMismatch(pos lexer.Position) error {
	return Errors["I003"].New(pos)
}

// --- Literals and Expressions ---
func errInvalidPrimaryExpression(pos lexer.Position) error {
	return Errors["I004"].New(pos)
}
func errInvalidLiteral(pos lexer.Position) error {
	return Errors["I005"].New(pos)
}
func errInvalidMapKey(pos lexer.Position, key any) error {
	return Errors["I006"].New(pos, key)
}
func errFieldAccessOnNonObject(pos lexer.Position, field, typ string) error {
	return Errors["I007"].New(pos, field, typ)
}

// --- Operators ---
func errInvalidOperator(pos lexer.Position, op, left, right string) error {
	return Errors["I008"].New(pos, op, left, right)
}
func errInvalidUnaryOperator(pos lexer.Position, op, typ string) error {
	return Errors["I009"].New(pos, op, typ)
}
func errUnknownUnaryOperator(pos lexer.Position, op string) error {
	return Errors["I010"].New(pos, op)
}
func errInvalidRangeBounds(pos lexer.Position, fromType, toType string) error {
	return Errors["I011"].New(pos, fromType, toType)
}
func errDivisionByZero(pos lexer.Position) error {
	return Errors["I012"].New(pos)
}

// --- Indexing and Slicing ---
func errInvalidIndex(pos lexer.Position, val any) error {
	return Errors["I013"].New(pos, val)
}
func errIndexOutOfBounds(pos lexer.Position, index, length int) error {
	return Errors["I014"].New(pos, index, length)
}
func errSliceOutOfBounds(pos lexer.Position, start, end, length int) error {
	return Errors["I015"].New(pos, start, end, length)
}
func errInvalidIndexTarget(pos lexer.Position, typ string) error {
	return Errors["I016"].New(pos, typ)
}
func errInvalidLenOperand(pos lexer.Position, typ string) error {
	return Errors["I017"].New(pos, typ)
}

// --- Testing ---
func errExpectFailed(pos lexer.Position) error {
	return Errors["I018"].New(pos)
}
