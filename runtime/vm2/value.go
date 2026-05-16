// Package vm2 is a from-scratch reimplementation of the Mochi VM
// designed around MEP-20's tight Value layout, frame/regs pooling,
// and the MEP-18/19 dispatch features. It is deliberately minimal:
// only the opcodes needed to run the MEP-17 benchmark corpus are
// implemented. The intent is to validate the layout decisions on
// real numbers without paying the cost of rewriting every handler
// in runtime/vm in place.
package vm2

import (
	"fmt"
	"math"
)

// Tag identifies the kind of value held in a Value cell.
type Tag uint8

const (
	TagNull Tag = iota
	TagBool
	TagInt
	TagFloat
	TagStr
	TagList
	TagMap
	TagFunc
)

// Value is the narrow 24-byte runtime cell that MEP-20 specifies.
// Numeric paths read Tag and Num only; reference paths use Ref.
//
//	Tag uint8     // 1 byte
//	_   [7]byte   // padding so Num is 8-byte aligned
//	Num int64     // payload: int as-is, float via Float64bits, bool 0/1
//	Ref any       // string, []Value, map[string]Value, *Closure, ...
//
// Total: 24 bytes (vs ~100 bytes in runtime/vm.Value). Every register
// move is a 24-byte copy, the GC scans only Ref for pointer payloads.
type Value struct {
	Tag Tag
	_   [7]byte
	Num int64
	Ref any
}

// VNull is the canonical null value; cheap to compare and copy.
var VNull = Value{Tag: TagNull}

// VBool, VInt, VFloat, VStr, VList, VMap, VFunc construct typed
// values. Keeping construction behind helpers means a later layout
// change (NaN-boxing? typed Ref union?) is a single-file edit.

func VBool(b bool) Value {
	if b {
		return Value{Tag: TagBool, Num: 1}
	}
	return Value{Tag: TagBool}
}

func VInt(n int) Value { return Value{Tag: TagInt, Num: int64(n)} }

func VInt64(n int64) Value { return Value{Tag: TagInt, Num: n} }

func VFloat(f float64) Value { return Value{Tag: TagFloat, Num: int64(math.Float64bits(f))} }

func VStr(s string) Value { return Value{Tag: TagStr, Ref: s} }

func VList(xs []Value) Value { return Value{Tag: TagList, Ref: xs} }

func VMap(m map[string]Value) Value { return Value{Tag: TagMap, Ref: m} }

func VFunc(cl *Closure) Value { return Value{Tag: TagFunc, Ref: cl} }

// AsInt etc. project the payload back to a typed Go value.
// They do not check the tag; callers that need a check should
// inspect v.Tag first.

func (v Value) AsBool() bool       { return v.Num != 0 }
func (v Value) AsInt() int         { return int(v.Num) }
func (v Value) AsInt64() int64     { return v.Num }
func (v Value) AsFloat() float64   { return math.Float64frombits(uint64(v.Num)) }
func (v Value) AsStr() string      { s, _ := v.Ref.(string); return s }
func (v Value) AsList() []Value    { l, _ := v.Ref.([]Value); return l }
func (v Value) AsMap() map[string]Value {
	m, _ := v.Ref.(map[string]Value)
	return m
}
func (v Value) AsFunc() *Closure { f, _ := v.Ref.(*Closure); return f }

// Truthy returns Mochi truthiness for v.
func (v Value) Truthy() bool {
	switch v.Tag {
	case TagBool:
		return v.AsBool()
	case TagInt:
		return v.Num != 0
	case TagFloat:
		return v.AsFloat() != 0
	case TagStr:
		return v.AsStr() != ""
	case TagList:
		return len(v.AsList()) > 0
	case TagMap:
		return len(v.AsMap()) > 0
	case TagFunc:
		return v.Ref != nil
	default:
		return false
	}
}

// Equal compares two values structurally for the tag-aware paths the
// VM actually uses (int, float, bool, str). Container equality is not
// supported here; callers wanting deep equality should walk Ref.
func (v Value) Equal(o Value) bool {
	if v.Tag != o.Tag {
		// Allow int/float cross-comparison the way runtime/vm does.
		if v.Tag == TagInt && o.Tag == TagFloat {
			return float64(v.Num) == o.AsFloat()
		}
		if v.Tag == TagFloat && o.Tag == TagInt {
			return v.AsFloat() == float64(o.Num)
		}
		return false
	}
	switch v.Tag {
	case TagNull:
		return true
	case TagBool, TagInt:
		return v.Num == o.Num
	case TagFloat:
		return v.AsFloat() == o.AsFloat()
	case TagStr:
		return v.AsStr() == o.AsStr()
	default:
		return false
	}
}

// Format renders v for Print. Kept simple; no big.Int / big.Rat in vm2.
func (v Value) String() string {
	switch v.Tag {
	case TagNull:
		return "nil"
	case TagBool:
		if v.AsBool() {
			return "true"
		}
		return "false"
	case TagInt:
		return fmt.Sprintf("%d", v.AsInt())
	case TagFloat:
		return fmt.Sprintf("%g", v.AsFloat())
	case TagStr:
		return v.AsStr()
	case TagList:
		return fmt.Sprintf("%v", v.AsList())
	case TagMap:
		return fmt.Sprintf("%v", v.AsMap())
	case TagFunc:
		return "<func>"
	default:
		return "?"
	}
}
