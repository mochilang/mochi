package vm

import (
	"fmt"
	"math/big"
	"reflect"
)

// ValueTag represents the type of a runtime value.
type ValueTag uint8

const (
	ValueNull ValueTag = iota
	ValueInt
	ValueBigInt
	ValueFloat
	ValueBigRat
	ValueStr
	ValueBool
	ValueList
	ValueMap
	ValueFunc
)

// Value is a tagged union used at runtime to avoid reflection.
type Value struct {
	Tag    ValueTag
	Int    int
	BigInt *big.Int
	BigRat *big.Rat
	Float  float64
	Str    string
	Bool   bool
	List   []Value
	Map    map[string]Value
	Func   any
}

// Truthy returns the boolean interpretation of v.
func (v Value) Truthy() bool {
	switch v.Tag {
	case ValueBool:
		return v.Bool
	case ValueInt:
		return v.Int != 0
	case ValueBigInt:
		return v.BigInt != nil && v.BigInt.Sign() != 0
	case ValueBigRat:
		return v.BigRat != nil && v.BigRat.Sign() != 0
	case ValueFloat:
		return v.Float != 0
	case ValueStr:
		return v.Str != ""
	case ValueList:
		return len(v.List) > 0
	case ValueMap:
		return len(v.Map) > 0
	default:
		return false
	}
}

// ToAny converts v to a Go value using primitive types, slices, and maps.
// Small enough for the compiler to inline.
func (v Value) ToAny() any {
	switch v.Tag {
	case ValueInt:
		return v.Int
	case ValueBigInt:
		if v.BigInt == nil {
			return (*big.Int)(nil)
		}
		return new(big.Int).Set(v.BigInt)
	case ValueFloat:
		return v.Float
	case ValueBigRat:
		if v.BigRat == nil {
			return (*big.Rat)(nil)
		}
		return new(big.Rat).Set(v.BigRat)
	case ValueBool:
		return v.Bool
	case ValueStr:
		return v.Str
	case ValueList:
		out := make([]any, len(v.List))
		for i, x := range v.List {
			out[i] = x.ToAny()
		}
		return out
	case ValueMap:
		m := make(map[string]any, len(v.Map))
		for k, x := range v.Map {
			m[k] = x.ToAny()
		}
		return m
	case ValueFunc:
		return v.Func
	default:
		return nil
	}
}

// FromAny converts basic Go values into a Value. Unsupported
// types yield a ValueNull. This helper keeps type assertions out of
// hot paths and may inline into callers.
func FromAny(v any) Value {
	switch val := v.(type) {
	case nil:
		return Value{Tag: ValueNull}
	case int:
		return Value{Tag: ValueInt, Int: val}
	case int64:
		return Value{Tag: ValueInt, Int: int(val)}
	case int32:
		return Value{Tag: ValueInt, Int: int(val)}
	case int16:
		return Value{Tag: ValueInt, Int: int(val)}
	case int8:
		return Value{Tag: ValueInt, Int: int(val)}
	case uint:
		return Value{Tag: ValueInt, Int: int(val)}
	case uint64:
		return Value{Tag: ValueInt, Int: int(val)}
	case uint32:
		return Value{Tag: ValueInt, Int: int(val)}
	case uint16:
		return Value{Tag: ValueInt, Int: int(val)}
	case uint8:
		return Value{Tag: ValueInt, Int: int(val)}
	case *big.Int:
		if val == nil {
			return Value{Tag: ValueBigInt}
		}
		return Value{Tag: ValueBigInt, BigInt: new(big.Int).Set(val)}
	case big.Int:
		tmp := new(big.Int)
		tmp.Set(&val)
		return Value{Tag: ValueBigInt, BigInt: tmp}
	case *big.Rat:
		if val == nil {
			return Value{Tag: ValueBigRat}
		}
		return Value{Tag: ValueBigRat, BigRat: new(big.Rat).Set(val)}
	case big.Rat:
		tmp := new(big.Rat)
		tmp.Set(&val)
		return Value{Tag: ValueBigRat, BigRat: tmp}
	case float64:
		return Value{Tag: ValueFloat, Float: val}
	case float32:
		return Value{Tag: ValueFloat, Float: float64(val)}
	case string:
		return Value{Tag: ValueStr, Str: val}
	case bool:
		return Value{Tag: ValueBool, Bool: val}
	case []any:
		list := make([]Value, len(val))
		for i, x := range val {
			list[i] = FromAny(x)
		}
		return Value{Tag: ValueList, List: list}
	case map[string]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[k] = FromAny(x)
		}
		return Value{Tag: ValueMap, Map: m}
	case map[int]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[fmt.Sprintf("%d", k)] = FromAny(x)
		}
		return Value{Tag: ValueMap, Map: m}
	case map[any]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[fmt.Sprint(k)] = FromAny(x)
		}
		return Value{Tag: ValueMap, Map: m}
	default:
		rv := reflect.ValueOf(v)
		switch rv.Kind() {
		case reflect.Slice, reflect.Array:
			list := make([]Value, rv.Len())
			for i := 0; i < rv.Len(); i++ {
				list[i] = FromAny(rv.Index(i).Interface())
			}
			return Value{Tag: ValueList, List: list}
		case reflect.Map:
			m := make(map[string]Value, rv.Len())
			for _, key := range rv.MapKeys() {
				m[fmt.Sprint(key.Interface())] = FromAny(rv.MapIndex(key).Interface())
			}
			return Value{Tag: ValueMap, Map: m}
		}
		return Value{Tag: ValueNull}
	}
}
