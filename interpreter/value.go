package interpreter

import (
	"fmt"
	"reflect"
	"sort"
	"strconv"
	"strings"
)

// ValueTag represents the type of a runtime value.
type ValueTag uint8

const (
	TagNull ValueTag = iota
	TagInt
	TagFloat
	TagStr
	TagBool
	TagList
	TagMap
	TagFunc
)

func (t ValueTag) String() string {
	switch t {
	case TagNull:
		return "null"
	case TagInt:
		return "int"
	case TagFloat:
		return "float"
	case TagStr:
		return "string"
	case TagBool:
		return "bool"
	case TagList:
		return "list"
	case TagMap:
		return "map"
	case TagFunc:
		return "func"
	default:
		return "unknown"
	}
}

// Value is a tagged union used at runtime to avoid reflection.
type Value struct {
	Tag   ValueTag
	Int   int
	Float float64
	Str   string
	Bool  bool
	List  []Value
	Map   map[string]Value
	Func  any
}

// Truthy returns the boolean interpretation of v.
func (v Value) Truthy() bool {
	switch v.Tag {
	case TagBool:
		return v.Bool
	case TagInt:
		return v.Int != 0
	case TagFloat:
		return v.Float != 0
	case TagStr:
		return v.Str != ""
	case TagList:
		return len(v.List) > 0
	case TagMap:
		return len(v.Map) > 0
	default:
		return false
	}
}

func anyToValue(v any) Value {
	switch val := v.(type) {
	case nil:
		return Value{Tag: TagNull}
	case int:
		return Value{Tag: TagInt, Int: val}
	case int64:
		return Value{Tag: TagInt, Int: int(val)}
	case float64:
		return Value{Tag: TagFloat, Float: val}
	case string:
		return Value{Tag: TagStr, Str: val}
	case bool:
		return Value{Tag: TagBool, Bool: val}
	case []Value:
		return Value{Tag: TagList, List: val}
	case []any:
		list := make([]Value, len(val))
		for i, x := range val {
			list[i] = anyToValue(x)
		}
		return Value{Tag: TagList, List: list}
	case map[string]Value:
		return Value{Tag: TagMap, Map: val}
	case map[string]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[k] = anyToValue(x)
		}
		return Value{Tag: TagMap, Map: m}
	case map[int]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[strconv.Itoa(k)] = anyToValue(x)
		}
		return Value{Tag: TagMap, Map: m}
	case map[any]any:
		m := make(map[string]Value, len(val))
		for k, x := range val {
			m[fmt.Sprint(k)] = anyToValue(x)
		}
		return Value{Tag: TagMap, Map: m}
	default:
		rv := reflect.ValueOf(v)
		switch rv.Kind() {
		case reflect.Slice, reflect.Array:
			list := make([]Value, rv.Len())
			for i := 0; i < rv.Len(); i++ {
				list[i] = anyToValue(rv.Index(i).Interface())
			}
			return Value{Tag: TagList, List: list}
		case reflect.Map:
			m := make(map[string]Value, rv.Len())
			for _, key := range rv.MapKeys() {
				m[fmt.Sprint(key.Interface())] = anyToValue(rv.MapIndex(key).Interface())
			}
			return Value{Tag: TagMap, Map: m}
		}
		return Value{Tag: TagNull}
	case Closure:
		return Value{Tag: TagFunc, Func: &val}
	case *Closure:
		return Value{Tag: TagFunc, Func: val}
	}
}

func valueToAny(v Value) any {
	switch v.Tag {
	case TagInt:
		return v.Int
	case TagFloat:
		return v.Float
	case TagStr:
		return v.Str
	case TagBool:
		return v.Bool
	case TagList:
		list := make([]any, len(v.List))
		for i, x := range v.List {
			list[i] = valueToAny(x)
		}
		return list
	case TagMap:
		m := make(map[string]any, len(v.Map))
		for k, x := range v.Map {
			m[k] = valueToAny(x)
		}
		return m
	case TagFunc:
		return v.Func
	default:
		return nil
	}
}

func argsKey(args []any) string {
	var sb strings.Builder
	for i, a := range args {
		if i > 0 {
			sb.WriteByte('|')
		}
		writeArgKey(&sb, a)
	}
	return sb.String()
}

func writeArgKey(sb *strings.Builder, v any) {
	switch x := v.(type) {
	case int:
		sb.WriteString("i:")
		sb.WriteString(strconv.Itoa(x))
	case float64:
		sb.WriteString("f:")
		sb.WriteString(strconv.FormatFloat(x, 'g', -1, 64))
	case string:
		sb.WriteString("s:")
		sb.WriteString(strconv.Quote(x))
	case bool:
		sb.WriteString("b:")
		if x {
			sb.WriteByte('1')
		} else {
			sb.WriteByte('0')
		}
	case []any:
		sb.WriteByte('[')
		for i, e := range x {
			if i > 0 {
				sb.WriteByte(',')
			}
			writeArgKey(sb, e)
		}
		sb.WriteByte(']')
	case map[string]any:
		sb.WriteByte('{')
		keys := make([]string, 0, len(x))
		for k := range x {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for i, k := range keys {
			if i > 0 {
				sb.WriteByte(',')
			}
			sb.WriteString(k)
			sb.WriteByte(':')
			writeArgKey(sb, x[k])
		}
		sb.WriteByte('}')
	default:
		sb.WriteString("p:")
		sb.WriteString(fmt.Sprintf("%p", v))
	}
}
