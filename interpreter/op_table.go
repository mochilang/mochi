package interpreter

import (
	"reflect"

	"github.com/alecthomas/participle/v2/lexer"
)

// binaryOpFunc defines a binary operator implementation on Values.
type binaryOpFunc func(lexer.Position, Value, Value) (Value, error)

// unaryOpFunc defines a unary operator implementation on a Value.
type unaryOpFunc func(lexer.Position, Value) (Value, error)

// Binary and unary operator tables.
var binaryOpTable map[string]binaryOpFunc
var unaryOpTable map[string]unaryOpFunc

func init() {
	binaryOpTable = map[string]binaryOpFunc{
		"+":  opAdd,
		"-":  opSub,
		"*":  opMul,
		"/":  opDiv,
		"%":  opMod,
		"==": opEq,
		"!=": opNeq,
		"<":  opLT,
		"<=": opLTE,
		">":  opGT,
		">=": opGTE,
		"&&": opAnd,
		"||": opOr,
	}
	unaryOpTable = map[string]unaryOpFunc{
		"-": opNeg,
		"!": opNot,
	}
}

// --- Binary operator implementations ---

func opAdd(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagInt, Int: left.Int + right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: left.Float + right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: float64(left.Int) + right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagFloat, Float: left.Float + float64(right.Int)}, nil
	case left.Tag == TagStr && right.Tag == TagStr:
		return Value{Tag: TagStr, Str: left.Str + right.Str}, nil
	case left.Tag == TagList && right.Tag == TagList:
		return Value{Tag: TagList, List: append(append([]Value{}, left.List...), right.List...)}, nil
	default:
		return Value{}, errInvalidOperator(pos, "+", left.Tag.String(), right.Tag.String())
	}
}

func opSub(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagInt, Int: left.Int - right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: left.Float - right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: float64(left.Int) - right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagFloat, Float: left.Float - float64(right.Int)}, nil
	default:
		return Value{}, errInvalidOperator(pos, "-", left.Tag.String(), right.Tag.String())
	}
}

func opMul(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagInt, Int: left.Int * right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: left.Float * right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: float64(left.Int) * right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagFloat, Float: left.Float * float64(right.Int)}, nil
	default:
		return Value{}, errInvalidOperator(pos, "*", left.Tag.String(), right.Tag.String())
	}
}

func opDiv(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case right.Tag == TagInt && right.Int == 0:
		return Value{}, errDivisionByZero(pos)
	case right.Tag == TagFloat && right.Float == 0:
		return Value{}, errDivisionByZero(pos)
	}
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagInt, Int: left.Int / right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: left.Float / right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagFloat, Float: float64(left.Int) / right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagFloat, Float: left.Float / float64(right.Int)}, nil
	default:
		return Value{}, errInvalidOperator(pos, "/", left.Tag.String(), right.Tag.String())
	}
}

func opMod(pos lexer.Position, left, right Value) (Value, error) {
	if right.Tag == TagInt && right.Int == 0 {
		return Value{}, errDivisionByZero(pos)
	}
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagInt, Int: left.Int % right.Int}, nil
	default:
		return Value{}, errInvalidOperator(pos, "%", left.Tag.String(), right.Tag.String())
	}
}

func opEq(_ lexer.Position, left, right Value) (Value, error) {
	if left.Tag != right.Tag {
		return Value{Tag: TagBool, Bool: false}, nil
	}
	switch left.Tag {
	case TagInt:
		return Value{Tag: TagBool, Bool: left.Int == right.Int}, nil
	case TagFloat:
		return Value{Tag: TagBool, Bool: left.Float == right.Float}, nil
	case TagStr:
		return Value{Tag: TagBool, Bool: left.Str == right.Str}, nil
	case TagBool:
		return Value{Tag: TagBool, Bool: left.Bool == right.Bool}, nil
	default:
		return Value{Tag: TagBool, Bool: reflect.DeepEqual(left, right)}, nil
	}
}

func opNeq(pos lexer.Position, left, right Value) (Value, error) {
	eq, err := opEq(pos, left, right)
	if err != nil {
		return Value{}, err
	}
	return Value{Tag: TagBool, Bool: !eq.Bool}, nil
}

func opLT(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagBool, Bool: left.Int < right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagBool, Bool: left.Float < right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagBool, Bool: float64(left.Int) < right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagBool, Bool: left.Float < float64(right.Int)}, nil
	case left.Tag == TagStr && right.Tag == TagStr:
		return Value{Tag: TagBool, Bool: left.Str < right.Str}, nil
	default:
		return Value{}, errInvalidOperator(pos, "<", left.Tag.String(), right.Tag.String())
	}
}

func opLTE(pos lexer.Position, left, right Value) (Value, error) {
	switch {
	case left.Tag == TagInt && right.Tag == TagInt:
		return Value{Tag: TagBool, Bool: left.Int <= right.Int}, nil
	case left.Tag == TagFloat && right.Tag == TagFloat:
		return Value{Tag: TagBool, Bool: left.Float <= right.Float}, nil
	case left.Tag == TagInt && right.Tag == TagFloat:
		return Value{Tag: TagBool, Bool: float64(left.Int) <= right.Float}, nil
	case left.Tag == TagFloat && right.Tag == TagInt:
		return Value{Tag: TagBool, Bool: left.Float <= float64(right.Int)}, nil
	case left.Tag == TagStr && right.Tag == TagStr:
		return Value{Tag: TagBool, Bool: left.Str <= right.Str}, nil
	default:
		return Value{}, errInvalidOperator(pos, "<=", left.Tag.String(), right.Tag.String())
	}
}

func opGT(pos lexer.Position, left, right Value) (Value, error) {
	res, err := opLTE(pos, right, left)
	if err != nil {
		return Value{}, err
	}
	return Value{Tag: TagBool, Bool: res.Bool}, nil
}

func opGTE(pos lexer.Position, left, right Value) (Value, error) {
	res, err := opLT(pos, right, left)
	if err != nil {
		return Value{}, err
	}
	return Value{Tag: TagBool, Bool: res.Bool}, nil
}

func opAnd(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagBool && right.Tag == TagBool {
		return Value{Tag: TagBool, Bool: left.Bool && right.Bool}, nil
	}
	return Value{}, errInvalidOperator(pos, "&&", left.Tag.String(), right.Tag.String())
}

func opOr(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagBool && right.Tag == TagBool {
		return Value{Tag: TagBool, Bool: left.Bool || right.Bool}, nil
	}
	return Value{}, errInvalidOperator(pos, "||", left.Tag.String(), right.Tag.String())
}

// --- Unary operator implementations ---

func opNeg(pos lexer.Position, val Value) (Value, error) {
	switch val.Tag {
	case TagInt:
		return Value{Tag: TagInt, Int: -val.Int}, nil
	case TagFloat:
		return Value{Tag: TagFloat, Float: -val.Float}, nil
	default:
		return Value{}, errInvalidUnaryOperator(pos, "-", val.Tag.String())
	}
}

func opNot(pos lexer.Position, val Value) (Value, error) {
	if val.Tag == TagBool {
		return Value{Tag: TagBool, Bool: !val.Bool}, nil
	}
	return Value{}, errInvalidUnaryOperator(pos, "!", val.Tag.String())
}
