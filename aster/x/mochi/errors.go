package mochi

import (
	"fmt"

	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// Errors defines diagnostic templates for the Decorate type inference.
// The list is intentionally small and covers the error messages produced by
// the Decorate function. It mirrors the structure used in types/errors.go.
var Errors = map[string]diagnostic.Template{
	"A000": {Code: "A000", Message: "nil program", Help: "Provide a valid program"},
	"A001": {Code: "A001", Message: "%s %s missing expression", Help: "Declare %s with a value"},
	"A002": {Code: "A002", Message: "type mismatch for %s: %s vs %s", Help: "Ensure %s matches"},
	"A003": {Code: "A003", Message: "undefined variable %s", Help: "Declare %s before use"},
	"A004": {Code: "A004", Message: "group expects one child", Help: "Provide a single expression"},
	"A005": {Code: "A005", Message: "list element type mismatch", Help: "Ensure all list elements share a type"},
	"A006": {Code: "A006", Message: "invalid map entry", Help: "Map entries require key and value"},
	"A007": {Code: "A007", Message: "map entry type mismatch", Help: "Ensure map keys and values share types"},
	"A008": {Code: "A008", Message: "binary expects two operands", Help: "Binary expressions require two operands"},
	"A009": {Code: "A009", Message: "boolean operator on non-bool", Help: "Use booleans with logical operators"},
	"A010": {Code: "A010", Message: "comparison type mismatch", Help: "Operands must have the same type"},
	"A011": {Code: "A011", Message: "numeric op with non-numeric", Help: "Use numeric operands"},
	"A012": {Code: "A012", Message: "invalid string operation", Help: "Only concatenation is supported"},
	"A013": {Code: "A013", Message: "unknown binary operator %s", Help: "Use a supported operator"},
	"A014": {Code: "A014", Message: "unary expects one operand", Help: "Provide one operand"},
	"A015": {Code: "A015", Message: "! on non-bool", Help: "Use ! with boolean expressions"},
	"A016": {Code: "A016", Message: "unknown unary operator %s", Help: "Use a supported operator"},
	"A017": {Code: "A017", Message: "index on non-indexable type %s", Help: "Only lists and maps support indexing"},
	"A018": {Code: "A018", Message: "list index must be int", Help: "Use an int index"},
	"A019": {Code: "A019", Message: "map key type mismatch: %s vs %s", Help: "Ensure the key matches the map's key type"},
	"A020": {Code: "A020", Message: "unknown field %s on %s", Help: "Check the struct definition"},
	"A021": {Code: "A021", Message: "%s is not a struct", Help: "Field access is only valid on structs"},
	"A022": {Code: "A022", Message: "cast expects a type", Help: "Provide a type after 'as'"},
	"A023": {Code: "A023", Message: "parameter %s missing type", Help: "Add a type for %s"},
	"A024": {Code: "A024", Message: "unknown function %s", Help: "Declare %s before use"},
	"A025": {Code: "A025", Message: "function %s expects %d arguments, got %d", Help: "Call %s with the correct number of arguments"},
	"A026": {Code: "A026", Message: "argument %d to %s type mismatch: %s vs %s", Help: "Ensure arguments match parameter types"},
	"A027": {Code: "A027", Message: "missing return type", Help: "Specify a return type"},
	"A028": {Code: "A028", Message: "return type mismatch: expected %s, got %s", Help: "Ensure return value matches"},
}

func pos(n *Node) lexer.Position {
	if n == nil {
		return lexer.Position{}
	}
	return lexer.Position{Line: n.Start, Column: n.StartCol + 1}
}

func errNilProgram(n *Node) error {
	return Errors["A000"].New(pos(n))
}

func errMissingExpr(kind, name string, n *Node) error {
	tmpl := Errors["A001"]
	msg := fmt.Sprintf(tmpl.Message, kind, name)
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errVarTypeMismatch(name string, want, got *Node, n *Node) error {
	tmpl := Errors["A002"]
	msg := fmt.Sprintf(tmpl.Message, name, typeString(want), typeString(got))
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errUndefinedVar(name string, n *Node) error {
	tmpl := Errors["A003"]
	msg := fmt.Sprintf(tmpl.Message, name)
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errGroupOneChild(n *Node) error {
	return Errors["A004"].New(pos(n))
}

func errListElemMismatch(n *Node) error {
	return Errors["A005"].New(pos(n))
}

func errInvalidMapEntry(n *Node) error {
	return Errors["A006"].New(pos(n))
}

func errMapEntryTypeMismatch(n *Node) error {
	return Errors["A007"].New(pos(n))
}

func errBinaryTwoOperands(n *Node) error {
	return Errors["A008"].New(pos(n))
}

func errBoolOpNonBool(n *Node) error {
	return Errors["A009"].New(pos(n))
}

func errComparisonMismatch(n *Node) error {
	return Errors["A010"].New(pos(n))
}

func errNumericOpNonNumeric(n *Node) error {
	return Errors["A011"].New(pos(n))
}

func errInvalidStringOp(n *Node) error {
	return Errors["A012"].New(pos(n))
}

func errUnknownBinaryOp(op string, n *Node) error {
	return Errors["A013"].New(pos(n), op)
}

func errUnaryOneOperand(n *Node) error {
	return Errors["A014"].New(pos(n))
}

func errBangNonBool(n *Node) error {
	return Errors["A015"].New(pos(n))
}

func errUnknownUnaryOp(op string, n *Node) error {
	return Errors["A016"].New(pos(n), op)
}

func errIndexNonIndexable(t *Node, n *Node) error {
	tmpl := Errors["A017"]
	msg := fmt.Sprintf(tmpl.Message, typeString(t))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}

func errListIndexNotInt(n *Node) error {
	return Errors["A018"].New(pos(n))
}

func errMapIndexTypeMismatch(expected, actual *Node, n *Node) error {
	tmpl := Errors["A019"]
	msg := fmt.Sprintf(tmpl.Message, typeString(expected), typeString(actual))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}

func errUnknownField(field string, typ *Node, n *Node) error {
	tmpl := Errors["A020"]
	msg := fmt.Sprintf(tmpl.Message, field, typeString(typ))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}

func errNotStruct(typ *Node, n *Node) error {
	tmpl := Errors["A021"]
	msg := fmt.Sprintf(tmpl.Message, typeString(typ))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}

func errCastMissingType(n *Node) error {
	return Errors["A022"].New(pos(n))
}

func errParamMissingType(name string, n *Node) error {
	tmpl := Errors["A023"]
	msg := fmt.Sprintf(tmpl.Message, name, name)
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errUnknownFunction(name string, n *Node) error {
	tmpl := Errors["A024"]
	msg := fmt.Sprintf(tmpl.Message, name)
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errFuncArgCount(name string, expected, actual int, n *Node) error {
	tmpl := Errors["A025"]
	msg := fmt.Sprintf(tmpl.Message, name, expected, actual, name)
	help := fmt.Sprintf(tmpl.Help, name)
	return diagnostic.New(tmpl.Code, pos(n), msg, help)
}

func errFuncArgTypeMismatch(idx int, name string, want, got *Node, n *Node) error {
	tmpl := Errors["A026"]
	msg := fmt.Sprintf(tmpl.Message, idx, name, typeString(want), typeString(got))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}

func errMissingReturnType(n *Node) error {
	return Errors["A027"].New(pos(n))
}

func errReturnTypeMismatch(want, got *Node, n *Node) error {
	tmpl := Errors["A028"]
	msg := fmt.Sprintf(tmpl.Message, typeString(want), typeString(got))
	return diagnostic.New(tmpl.Code, pos(n), msg, tmpl.Help)
}
