package types

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// --- Type Checker Diagnostic Helpers (T000 - T099) ---

func errLetMissingTypeOrValue(pos lexer.Position) error {
	return diagnostic.New("T000", pos,
		"`let` requires a type annotation or a value",
		"Write `let x = ...` or `let x: int`.")
}

func errAssignUndeclared(pos lexer.Position, name string) error {
	return diagnostic.New("T001", pos,
		fmt.Sprintf("assignment to undeclared variable: %s", name),
		"Declare the variable using `let` first.")
}

func errUnknownVariable(pos lexer.Position, name string) error {
	return diagnostic.New("T002", pos,
		fmt.Sprintf("undefined variable: %s", name),
		"Check if the variable was declared in this scope.")
}

func errUnknownFunction(pos lexer.Position, name string) error {
	return diagnostic.New("T003", pos,
		fmt.Sprintf("unknown function: %s", name),
		"Ensure the function is defined before it's called.")
}

func errTypeMismatch(pos lexer.Position, expected, actual Type) error {
	return diagnostic.New("T004", pos,
		fmt.Sprintf("type mismatch: expected %s but got %s", expected, actual),
		"Change the value to match the expected type.")
}

func errCannotAssign(pos lexer.Position, rhs Type, lhsName string, lhs Type) error {
	return diagnostic.New("T005", pos,
		fmt.Sprintf("cannot assign %s to %s (expected %s)", rhs, lhsName, lhs),
		"Ensure the types on both sides of `=` are compatible.")
}

func errParamMissingType(pos lexer.Position, name string) error {
	return diagnostic.New("T006", pos,
		fmt.Sprintf("parameter `%s` is missing a type", name),
		"Add a type annotation like `x: int`.")
}

func errExpectBoolean(pos lexer.Position) error {
	return diagnostic.New("T007", pos,
		"`expect` must be a boolean expression",
		"Return a `true` or `false` condition inside `expect`.")
}

func errTooManyArgs(pos lexer.Position, expected, actual int) error {
	return diagnostic.New("T008", pos,
		fmt.Sprintf("too many arguments: expected %d, got %d", expected, actual),
		"Remove extra arguments or update the function definition.")
}

func errArgTypeMismatch(pos lexer.Position, index int, expected, actual Type) error {
	return diagnostic.New("T009", pos,
		fmt.Sprintf("argument %d type mismatch: expected %s, got %s", index+1, expected, actual),
		"Check the order and type of function arguments.")
}

func errNotFunction(pos lexer.Position, name string) error {
	return diagnostic.New("T010", pos,
		fmt.Sprintf("%s is not a function", name),
		"Use a function or closure in this position.")
}

func errIncompatibleEquality(pos lexer.Position) error {
	return diagnostic.New("T011", pos,
		"incompatible types in equality expression",
		"Ensure both sides of `==` or `!=` are the same type.")
}

func errIncompatibleComparison(pos lexer.Position) error {
	return diagnostic.New("T012", pos,
		"incompatible types in comparison expression",
		"Use comparable types like numbers or strings.")
}

func errInvalidPrimary(pos lexer.Position) error {
	return diagnostic.New("T013", pos,
		"invalid primary expression",
		"Check if the expression is valid or complete.")
}

func errReturnMismatch(pos lexer.Position, expected, actual Type) error {
	return diagnostic.New("T014", pos,
		fmt.Sprintf("return type mismatch: expected %s but got %s", expected, actual),
		"Ensure the returned value matches the declared function return type.")
}
