package interpreter

import "github.com/alecthomas/participle/v2/lexer"

// binaryInstr is the function signature for builtin binary operations.
type binaryInstr func(lexer.Position, Value, Value) (Value, error)

// unaryInstr is the function signature for builtin unary operations.
type unaryInstr func(lexer.Position, Value) (Value, error)

var (
	binaryTable map[string]binaryInstr
	unaryTable  map[string]unaryInstr
)

func init() {
	binaryTable = map[string]binaryInstr{
		"+":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "+", r) },
		"-":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "-", r) },
		"*":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "*", r) },
		"/":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "/", r) },
		"%":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "%", r) },
		"<":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "<", r) },
		"<=":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "<=", r) },
		">":         func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, ">", r) },
		">=":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, ">=", r) },
		"==":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "==", r) },
		"!=":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "!=", r) },
		"&&":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "&&", r) },
		"||":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "||", r) },
		"in":        func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "in", r) },
		"union":     func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "union", r) },
		"union_all": func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "union_all", r) },
		"except":    func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "except", r) },
		"intersect": func(pos lexer.Position, l, r Value) (Value, error) { return applyBinaryValue(pos, l, "intersect", r) },
	}

	unaryTable = map[string]unaryInstr{
		"-": func(pos lexer.Position, v Value) (Value, error) { return applyUnaryValue(pos, "-", v) },
		"!": func(pos lexer.Position, v Value) (Value, error) { return applyUnaryValue(pos, "!", v) },
	}
}
