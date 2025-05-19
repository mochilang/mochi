package interpreter

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// --- Runtime Diagnostics (I000 - I099) ---

func errUndefinedVariable(pos lexer.Position, name string) error {
	return diagnostic.New("I000", pos,
		fmt.Sprintf("undefined variable: %s", name),
		"Make sure the variable is declared before use.")
}

func errFieldAccessOnNonObject(pos lexer.Position, field string, typ string) error {
	return diagnostic.New("I001", pos,
		fmt.Sprintf("cannot access field '%s' on non-object of type %s", field, typ),
		"Use a map-like object to access fields.")
}

func errInvalidPrimaryExpression(pos lexer.Position) error {
	return diagnostic.New("I002", pos,
		"invalid primary expression",
		"Expected a literal, function, or variable.")
}

func errInvalidLiteral(pos lexer.Position) error {
	return diagnostic.New("I003", pos,
		"invalid literal value",
		"Check the syntax of the literal.")
}

func errUndefinedFunctionOrClosure(pos lexer.Position, name string) error {
	return diagnostic.New("I004", pos,
		fmt.Sprintf("undefined function or closure: %s", name),
		"Ensure the function or closure exists in scope.")
}

func errTooManyFunctionArgs(pos lexer.Position, name string, expected, actual int) error {
	return diagnostic.New("I005", pos,
		fmt.Sprintf("function %s expects %d arguments, got %d", name, expected, actual),
		"Pass the correct number of arguments.")
}

func errInternalClosureArgMismatch(pos lexer.Position) error {
	return diagnostic.New("I006", pos,
		"internal error: argument count mismatch in closure",
		"Check closure invocation logic.")
}

func errInvalidOperator(pos lexer.Position, op string, left, right string) error {
	return diagnostic.New("I007", pos,
		fmt.Sprintf("cannot apply operator '%s' to types %s and %s", op, left, right),
		"Use compatible types for this operator.")
}

func errInvalidUnaryOperator(pos lexer.Position, op string, typ string) error {
	return diagnostic.New("I008", pos,
		fmt.Sprintf("invalid use of unary '%s' on %s", op, typ),
		"Use unary operators only with numeric or boolean values.")
}

func errUnknownUnaryOperator(pos lexer.Position, op string) error {
	return diagnostic.New("I009", pos,
		fmt.Sprintf("unknown unary operator: %s", op),
		"Supported operators are '-', '!'")
}

func errInvalidRangeBounds(pos lexer.Position, fromType, toType string) error {
	return diagnostic.New("I010", pos,
		fmt.Sprintf("range bounds must be integers, got %s and %s", fromType, toType),
		"Ensure both `for x in a..b` bounds are integers.")
}

func errDivisionByZero(pos lexer.Position) error {
	return diagnostic.New("I011", pos,
		"division by zero",
		"Make sure the denominator is not zero.")
}

func errInvalidIndex(pos lexer.Position, val any) error {
	return diagnostic.New("I012", pos,
		fmt.Sprintf("index must be an integer, got %T", val),
		"Use an integer value inside the brackets.")
}

func errIndexOutOfBounds(pos lexer.Position, index, length int) error {
	return diagnostic.New("I013", pos,
		fmt.Sprintf("index %d out of bounds for length %d", index, length),
		"Ensure the index is within the list or string length.")
}

func errSliceOutOfBounds(pos lexer.Position, start, end, length int) error {
	return diagnostic.New("I014", pos,
		fmt.Sprintf("invalid slice range [%d:%d] for length %d", start, end, length),
		"Start must be ≤ end and within the sequence bounds.")
}

func errInvalidIndexTarget(pos lexer.Position, typ string) error {
	return diagnostic.New("I015", pos,
		fmt.Sprintf("cannot index value of type %s", typ),
		"Only lists and strings support indexing/slicing.")
}

func errInvalidLenOperand(pos lexer.Position, typ string) error {
	return diagnostic.New("I016", pos,
		fmt.Sprintf("cannot take length of type %s", typ),
		"Use `len(...)` only with lists or strings.")
}
