package interpreter

import (
	"sort"

	"mochi/parser"
	"mochi/runtime/llm"
	"mochi/types"
)

func typeToSchema(t types.Type) map[string]any {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return map[string]any{"type": "integer"}
	case types.FloatType:
		return map[string]any{"type": "number"}
	case types.StringType:
		return map[string]any{"type": "string"}
	case types.BoolType:
		return map[string]any{"type": "boolean"}
	case types.ListType:
		return map[string]any{
			"type":  "array",
			"items": typeToSchema(tt.Elem),
		}
	case types.MapType:
		return map[string]any{
			"type":                 "object",
			"additionalProperties": typeToSchema(tt.Value),
		}
	case types.StructType:
		return structToSchema(tt)
	default:
		return map[string]any{}
	}
}

func structToSchema(st types.StructType) map[string]any {
	props := make(map[string]any, len(st.Fields))
	required := make([]string, 0, len(st.Fields))
	for name, ft := range st.Fields {
		props[name] = typeToSchema(ft)
		required = append(required, name)
	}
	sort.Strings(required)
	schema := map[string]any{
		"type":       "object",
		"properties": props,
	}
	if len(required) > 0 {
		schema["required"] = required
	}
	return schema
}

func funcToTool(name string, fn *parser.FunStmt, ft types.FuncType) llm.Tool {
	props := make(map[string]any, len(fn.Params))
	required := make([]string, 0, len(fn.Params))
	for i, p := range fn.Params {
		var t types.Type
		if i < len(ft.Params) {
			t = ft.Params[i]
		} else {
			t = types.AnyType{}
		}
		props[p.Name] = typeToSchema(t)
		required = append(required, p.Name)
	}
	schema := map[string]any{"type": "object", "properties": props}
	if len(required) > 0 {
		schema["required"] = required
	}
	return llm.Tool{Name: name, Parameters: schema}
}
