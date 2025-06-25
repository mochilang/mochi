package cscode

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"mochi/parser"
	"mochi/types"
)

func delegateType(params []string, ret string) string {
	if ret == "" || ret == "void" {
		if len(params) == 0 {
			return "Action"
		}
		return fmt.Sprintf("Action<%s>", strings.Join(params, ", "))
	}
	generics := append(params, ret)
	return fmt.Sprintf("Func<%s>", strings.Join(generics, ", "))
}

func repoRoot() string {
	dir, _ := os.Getwd()
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return ""
}

func csTypeOf(t types.Type) string {
	switch tt := t.(type) {
	case types.IntType, types.Int64Type:
		return "long"
	case types.FloatType:
		return "double"
	case types.StringType:
		return "string"
	case types.BoolType:
		return "bool"
	case types.ListType:
		return csTypeOf(tt.Elem) + "[]"
	case types.MapType:
		return fmt.Sprintf("Dictionary<%s, %s>", csTypeOf(tt.Key), csTypeOf(tt.Value))
	case types.StructType:
		return sanitizeName(tt.Name)
	case types.UnionType:
		return sanitizeName(tt.Name)
	case types.FuncType:
		params := make([]string, len(tt.Params))
		for i, p := range tt.Params {
			params[i] = csTypeOf(p)
		}
		ret := csTypeOf(tt.Return)
		return delegateType(params, ret)
	case types.VoidType:
		return "void"
	case types.AnyType:
		return "dynamic"
	default:
		return "dynamic"
	}
}

func isAny(t types.Type) bool { _, ok := t.(types.AnyType); return ok }

func contains(sl []string, s string) bool {
	for _, v := range sl {
		if v == s {
			return true
		}
	}
	return false
}

func (c *Compiler) resolveTypeRef(t *parser.TypeRef) types.Type {
	return types.ResolveTypeRef(t, c.env)
}
