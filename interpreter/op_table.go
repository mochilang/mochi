package interpreter

import (
	"fmt"
	"reflect"
	"strings"

	"github.com/alecthomas/participle/v2/lexer"
)

// unaryOpFunc represents a unary operation on a Value.
type unaryOpFunc func(lexer.Position, Value) (Value, error)

// binaryOpFunc represents a binary operation on two Values.
type binaryOpFunc func(lexer.Position, Value, Value) (Value, error)

var unaryOps = map[string]unaryOpFunc{
	"-": opUnaryNegate,
	"!": opUnaryNot,
}

var binaryOps = map[string]binaryOpFunc{
	"+":         opAdd,
	"-":         opSub,
	"*":         opMul,
	"/":         opDiv,
	"%":         opMod,
	"==":        opEq,
	"!=":        opNe,
	"<":         opLt,
	"<=":        opLe,
	">":         opGt,
	">=":        opGe,
	"&&":        opAnd,
	"||":        opOr,
	"in":        opIn,
	"union":     opUnion,
	"union_all": opUnionAll,
	"except":    opExcept,
	"intersect": opIntersect,
}

func opUnaryNegate(pos lexer.Position, val Value) (Value, error) {
	switch val.Tag {
	case TagInt:
		return Value{Tag: TagInt, Int: -val.Int}, nil
	case TagFloat:
		return Value{Tag: TagFloat, Float: -val.Float}, nil
	default:
		return Value{}, errInvalidUnaryOperator(pos, "-", val.Tag.String())
	}
}

func opUnaryNot(pos lexer.Position, val Value) (Value, error) {
	if val.Tag == TagBool {
		return Value{Tag: TagBool, Bool: !val.Bool}, nil
	}
	return Value{}, errInvalidUnaryOperator(pos, "!", val.Tag.String())
}

func opAdd(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagList && right.Tag == TagList {
		return Value{Tag: TagList, List: append(append([]Value{}, left.List...), right.List...)}, nil
	}
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagInt, Int: left.Int + right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: float64(left.Int) + right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: left.Float + right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagFloat, Float: left.Float + float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagStr, Str: left.Str + right.Str}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, "+", left.Tag.String(), right.Tag.String())
}

func opSub(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagInt, Int: left.Int - right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: float64(left.Int) - right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: left.Float - right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagFloat, Float: left.Float - float64(right.Int)}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, "-", left.Tag.String(), right.Tag.String())
}

func opMul(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagInt, Int: left.Int * right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: float64(left.Int) * right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagFloat, Float: left.Float * right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagFloat, Float: left.Float * float64(right.Int)}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, "*", left.Tag.String(), right.Tag.String())
}

func opDiv(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return applyIntBinaryValue(pos, left.Int, "/", right.Int)
		}
		if right.Tag == TagFloat {
			return applyFloatBinaryValue(pos, float64(left.Int), "/", right.Float)
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return applyFloatBinaryValue(pos, left.Float, "/", right.Float)
		}
		if right.Tag == TagInt {
			return applyFloatBinaryValue(pos, left.Float, "/", float64(right.Int))
		}
	}
	return Value{}, errInvalidOperator(pos, "/", left.Tag.String(), right.Tag.String())
}

func opMod(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagInt && right.Tag == TagInt {
		return applyIntBinaryValue(pos, left.Int, "%", right.Int)
	}
	return Value{}, errInvalidOperator(pos, "%", left.Tag.String(), right.Tag.String())
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

func opEq(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagList && right.Tag == TagList {
		if len(left.List) != len(right.List) {
			return Value{Tag: TagBool, Bool: false}, nil
		}
		for i := range left.List {
			eq, err := opEq(pos, left.List[i], right.List[i])
			if err != nil {
				return Value{}, err
			}
			if !eq.Bool {
				return Value{Tag: TagBool, Bool: false}, nil
			}
		}
		return Value{Tag: TagBool, Bool: true}, nil
	}
	switch left.Tag {
	case TagBool:
		if right.Tag == TagBool {
			return Value{Tag: TagBool, Bool: left.Bool == right.Bool}, nil
		}
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Int == right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: float64(left.Int) == right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: left.Float == right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Float == float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagBool, Bool: left.Str == right.Str}, nil
		}
	}
	if reflect.DeepEqual(valueToAny(left), valueToAny(right)) {
		return Value{Tag: TagBool, Bool: true}, nil
	}
	return Value{Tag: TagBool, Bool: false}, nil
}

func opNe(pos lexer.Position, left, right Value) (Value, error) {
	eq, err := opEq(pos, left, right)
	if err != nil {
		return Value{}, err
	}
	return Value{Tag: TagBool, Bool: !eq.Bool}, nil
}

func opLt(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Int < right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: float64(left.Int) < right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: left.Float < right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Float < float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagBool, Bool: left.Str < right.Str}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, "<", left.Tag.String(), right.Tag.String())
}

func opLe(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Int <= right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: float64(left.Int) <= right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: left.Float <= right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Float <= float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagBool, Bool: left.Str <= right.Str}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, "<=", left.Tag.String(), right.Tag.String())
}

func opGt(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Int > right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: float64(left.Int) > right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: left.Float > right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Float > float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagBool, Bool: left.Str > right.Str}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, ">", left.Tag.String(), right.Tag.String())
}

func opGe(pos lexer.Position, left, right Value) (Value, error) {
	switch left.Tag {
	case TagInt:
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Int >= right.Int}, nil
		}
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: float64(left.Int) >= right.Float}, nil
		}
	case TagFloat:
		if right.Tag == TagFloat {
			return Value{Tag: TagBool, Bool: left.Float >= right.Float}, nil
		}
		if right.Tag == TagInt {
			return Value{Tag: TagBool, Bool: left.Float >= float64(right.Int)}, nil
		}
	case TagStr:
		if right.Tag == TagStr {
			return Value{Tag: TagBool, Bool: left.Str >= right.Str}, nil
		}
	}
	return Value{}, errInvalidOperator(pos, ">=", left.Tag.String(), right.Tag.String())
}

func opIn(pos lexer.Position, left, right Value) (Value, error) {
	switch right.Tag {
	case TagList:
		for _, item := range right.List {
			eq, err := opEq(pos, left, item)
			if err == nil && eq.Tag == TagBool && eq.Bool {
				return Value{Tag: TagBool, Bool: true}, nil
			}
			if err != nil && reflect.DeepEqual(valueToAny(left), valueToAny(item)) {
				return Value{Tag: TagBool, Bool: true}, nil
			}
		}
		return Value{Tag: TagBool, Bool: false}, nil
	case TagMap:
		key := fmt.Sprint(valueToAny(left))
		_, ok := right.Map[key]
		return Value{Tag: TagBool, Bool: ok}, nil
	case TagStr:
		if left.Tag == TagStr {
			return Value{Tag: TagBool, Bool: strings.Contains(right.Str, left.Str)}, nil
		}
	default:
		return Value{}, errInvalidOperator(pos, "in", left.Tag.String(), right.Tag.String())
	}
	return Value{}, errInvalidOperator(pos, "in", left.Tag.String(), right.Tag.String())
}

func opUnionAll(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag == TagList && right.Tag == TagList {
		return Value{Tag: TagList, List: append(append([]Value{}, left.List...), right.List...)}, nil
	}
	return Value{}, errInvalidOperator(pos, "union_all", left.Tag.String(), right.Tag.String())
}

func opUnion(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag != TagList || right.Tag != TagList {
		return Value{}, errInvalidOperator(pos, "union", left.Tag.String(), right.Tag.String())
	}
	merged := append([]Value{}, left.List...)
	for _, rv := range right.List {
		found := false
		for _, lv := range merged {
			eq, err := opEq(pos, lv, rv)
			if err == nil && eq.Tag == TagBool && eq.Bool {
				found = true
				break
			}
			if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
				found = true
				break
			}
		}
		if !found {
			merged = append(merged, rv)
		}
	}
	return Value{Tag: TagList, List: merged}, nil
}

func opExcept(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag != TagList || right.Tag != TagList {
		return Value{}, errInvalidOperator(pos, "except", left.Tag.String(), right.Tag.String())
	}
	diff := []Value{}
	for _, lv := range left.List {
		found := false
		for _, rv := range right.List {
			eq, err := opEq(pos, lv, rv)
			if err == nil && eq.Tag == TagBool && eq.Bool {
				found = true
				break
			}
			if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
				found = true
				break
			}
		}
		if !found {
			diff = append(diff, lv)
		}
	}
	return Value{Tag: TagList, List: diff}, nil
}

func opIntersect(pos lexer.Position, left, right Value) (Value, error) {
	if left.Tag != TagList || right.Tag != TagList {
		return Value{}, errInvalidOperator(pos, "intersect", left.Tag.String(), right.Tag.String())
	}
	inter := []Value{}
	for _, lv := range left.List {
		match := false
		for _, rv := range right.List {
			eq, err := opEq(pos, lv, rv)
			if err == nil && eq.Tag == TagBool && eq.Bool {
				match = true
				break
			}
			if err != nil && reflect.DeepEqual(valueToAny(lv), valueToAny(rv)) {
				match = true
				break
			}
		}
		if match {
			exists := false
			for _, iv := range inter {
				eq, err := opEq(pos, iv, lv)
				if err == nil && eq.Tag == TagBool && eq.Bool {
					exists = true
					break
				}
				if err != nil && reflect.DeepEqual(valueToAny(iv), valueToAny(lv)) {
					exists = true
					break
				}
			}
			if !exists {
				inter = append(inter, lv)
			}
		}
	}
	return Value{Tag: TagList, List: inter}, nil
}
