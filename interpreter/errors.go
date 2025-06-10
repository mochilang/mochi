package interpreter

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
	"mochi/types"
)

var Errors = map[string]diagnostic.Template{
	// --- Variables and Functions ---
	"I000": {Code: "I000", Message: "undefined variable: %s", Help: "`%s` is not defined. Declare it before use."},
	"I001": {Code: "I001", Message: "undefined function or closure: %s", Help: "`%s` is not defined. Declare it before calling."},
	"I002": {Code: "I002", Message: "function %s expects %d arguments, got %d", Help: "Pass exactly %d arguments to `%s`."},
	"I003": {Code: "I003", Message: "internal error: argument count mismatch in closure", Help: "Check the closure's argument handling logic."},

	// --- Literals and Expressions ---
	"I004": {Code: "I004", Message: "invalid primary expression", Help: "Expected a literal, variable, or function call."},
	"I005": {Code: "I005", Message: "invalid literal value", Help: "Check the format or syntax of the literal."},
	"I006": {Code: "I006", Message: "invalid map key: expected string, got %T", Help: "Only `string` keys are allowed in map literals."},
	"I007": {Code: "I007", Message: "cannot access field `%s` on non-object of type %s", Help: "Access fields only on objects or maps."},

	// --- Operators ---
	"I008": {Code: "I008", Message: "cannot apply operator '%s' to types %s and %s", Help: "Use compatible types for the operator."},
	"I009": {Code: "I009", Message: "invalid use of unary '%s' on %s", Help: "Use unary operators only with numbers or booleans."},
	"I010": {Code: "I010", Message: "unknown unary operator: %s", Help: "Supported unary operators are: `-`, `!`."},
	"I011": {Code: "I011", Message: "range bounds must be integers, got %s and %s", Help: "Use integers in `for x in a..b` ranges."},
	"I012": {Code: "I012", Message: "division by zero", Help: "Ensure the denominator is not zero."},

	// --- Indexing and Slicing ---
	"I013": {Code: "I013", Message: "index must be an integer, got %T", Help: "Use an `int` value as an index (e.g., `list[0]`)."},
	"I014": {Code: "I014", Message: "index %d out of bounds for length %d", Help: "Use an index within bounds of the list or string."},
	"I015": {Code: "I015", Message: "invalid slice range [%d:%d] for length %d", Help: "Make sure the slice range is valid and within bounds."},
	"I016": {Code: "I016", Message: "cannot index value of type %s", Help: "Indexing is supported only on lists, strings, or maps."},
	"I017": {Code: "I017", Message: "cannot take length of type %s", Help: "Use `len(...)` only on lists and strings."},

	// --- Testing ---
	"I018": {Code: "I018", Message: "expect condition failed", Help: "The condition evaluated to `false`."},

	// --- Loops and Iteration ---
	"I019": {Code: "I019", Message: "cannot iterate over value of type %s", Help: "Only `list`, `map`, and `string` are iterable in `for ... in` loops."},
	"I020": {Code: "I020", Message: "cannot cast %T to %s", Help: "Ensure the value matches the expected type."},
	"I021": {Code: "I021", Message: "missing field `%s` for %s", Help: "Check that all required fields are present."},
	"I022": {Code: "I022", Message: "extern object not registered: %s", Help: "Call extern.Register(name, obj) before use."},
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

// --- New helper functions for loop errors ---
func errInvalidIterator(pos lexer.Position, typ string) error {
	return Errors["I019"].New(pos, typ)
}

func errCastType(pos lexer.Position, val any, typ types.Type) error {
	return Errors["I020"].New(pos, val, typ)
}

func errCastMissingField(pos lexer.Position, field, typ string) error {
	return Errors["I021"].New(pos, field, typ)
}

func errExternObject(pos lexer.Position, name string) error {
	return Errors["I022"].New(pos, name)
}
