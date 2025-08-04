package mochi

import (
	"github.com/alecthomas/participle/v2/lexer"
	"mochi/diagnostic"
)

// Errors defines decorator error templates.
var Errors = map[string]diagnostic.Template{
	"D000": {Code: "D000", Message: "nil program"},
	"D001": {Code: "D001", Message: "%s %s missing expression"},
	"D002": {Code: "D002", Message: "type mismatch for %s: %s vs %s"},
	"D003": {Code: "D003", Message: "undefined variable %s"},
	"D004": {Code: "D004", Message: "group expects one child"},
	"D005": {Code: "D005", Message: "list element type mismatch"},
	"D006": {Code: "D006", Message: "invalid map entry"},
	"D007": {Code: "D007", Message: "map entry type mismatch"},
	"D008": {Code: "D008", Message: "binary expects two operands"},
	"D009": {Code: "D009", Message: "boolean operator on non-bool"},
	"D010": {Code: "D010", Message: "comparison type mismatch"},
	"D011": {Code: "D011", Message: "invalid string operation"},
	"D012": {Code: "D012", Message: "numeric op with non-numeric"},
	"D013": {Code: "D013", Message: "unknown binary operator %s"},
	"D014": {Code: "D014", Message: "unary expects one operand"},
	"D015": {Code: "D015", Message: "! on non-bool"},
	"D016": {Code: "D016", Message: "numeric unary on non-numeric"},
	"D017": {Code: "D017", Message: "unknown unary operator %s"},
}

func errNilProgram() error { return Errors["D000"].New(lexer.Position{}) }
func errMissingExpression(pos lexer.Position, kind, name string) error {
	return Errors["D001"].New(pos, kind, name)
}
func errTypeMismatch(pos lexer.Position, name, want, got string) error {
	return Errors["D002"].New(pos, name, want, got)
}
func errUndefinedVariable(pos lexer.Position, name string) error {
	return Errors["D003"].New(pos, name)
}
func errGroupOneChild(pos lexer.Position) error          { return Errors["D004"].New(pos) }
func errListElementMismatch(pos lexer.Position) error    { return Errors["D005"].New(pos) }
func errInvalidMapEntry(pos lexer.Position) error        { return Errors["D006"].New(pos) }
func errMapEntryMismatch(pos lexer.Position) error       { return Errors["D007"].New(pos) }
func errBinaryOperands(pos lexer.Position) error         { return Errors["D008"].New(pos) }
func errBoolOpNonBool(pos lexer.Position) error          { return Errors["D009"].New(pos) }
func errComparisonMismatch(pos lexer.Position) error     { return Errors["D010"].New(pos) }
func errInvalidStringOperation(pos lexer.Position) error { return Errors["D011"].New(pos) }
func errNumericNonNumeric(pos lexer.Position) error      { return Errors["D012"].New(pos) }
func errUnknownBinary(pos lexer.Position, op string) error {
	return Errors["D013"].New(pos, op)
}
func errUnaryOperand(pos lexer.Position) error    { return Errors["D014"].New(pos) }
func errBangNonBool(pos lexer.Position) error     { return Errors["D015"].New(pos) }
func errUnaryNonNumeric(pos lexer.Position) error { return Errors["D016"].New(pos) }
func errUnknownUnary(pos lexer.Position, op string) error {
	return Errors["D017"].New(pos, op)
}
