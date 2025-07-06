package any2mochi

import (
	"bytes"
	"encoding/json"
	"strings"

	luaast "github.com/yuin/gopher-lua/ast"
	luaparser "github.com/yuin/gopher-lua/parse"
)

// luaChunk represents a simplified Lua AST used for JSON serialization.
type luaChunk struct {
	Stmts []luaStmt `json:"stmts"`
}

type luaStmt struct {
	Type  string    `json:"type"`
	Name  string    `json:"name,omitempty"`
	Func  *luaFunc  `json:"func,omitempty"`
	Value string    `json:"value,omitempty"`
	Stmts []luaStmt `json:"stmts,omitempty"`
}

type luaFunc struct {
	Params []string  `json:"params"`
	Body   []luaStmt `json:"body"`
}

// ParseLuaAST parses src using gopher-lua and returns a luaChunk structure.
func ParseLuaAST(src string) (*luaChunk, error) {
	stmts, err := luaparser.Parse(bytes.NewReader([]byte(src)), "src.lua")
	if err != nil {
		return nil, err
	}
	c := &luaChunk{}
	for _, st := range stmts {
		c.Stmts = append(c.Stmts, convertLuaStmt(st))
	}
	return c, nil
}

func convertLuaStmt(st luaast.Stmt) luaStmt {
	switch s := st.(type) {
	case *luaast.FuncDefStmt:
		fn := convertLuaFunc(s.Func)
		name := luaFuncName(s.Name)
		return luaStmt{Type: "funcdef", Name: name, Func: &fn}
	case *luaast.LocalAssignStmt:
		if len(s.Names) == 1 && len(s.Exprs) == 1 {
			if f, ok := s.Exprs[0].(*luaast.FunctionExpr); ok {
				fn := convertLuaFunc(f)
				return luaStmt{Type: "localfunc", Name: s.Names[0], Func: &fn}
			}
		}
		stmt := luaStmt{Type: "local"}
		if len(s.Names) == 1 && len(s.Exprs) == 1 {
			stmt.Name = s.Names[0]
			stmt.Value = luaExprString(s.Exprs[0])
		}
		return stmt
	case *luaast.ReturnStmt:
		if len(s.Exprs) == 1 {
			return luaStmt{Type: "return", Value: luaExprString(s.Exprs[0])}
		}
	case *luaast.FuncCallStmt:
		return luaStmt{Type: "call", Value: luaExprString(s.Expr)}
	case *luaast.DoBlockStmt:
		var inner []luaStmt
		for _, st := range s.Stmts {
			inner = append(inner, convertLuaStmt(st))
		}
		return luaStmt{Type: "block", Stmts: inner}
	}
	return luaStmt{Type: "unknown"}
}

func convertLuaFunc(fn *luaast.FunctionExpr) luaFunc {
	var params []string
	if fn.ParList != nil {
		params = append(params, fn.ParList.Names...)
	}
	var body []luaStmt
	for _, st := range fn.Stmts {
		body = append(body, convertLuaStmt(st))
	}
	return luaFunc{Params: params, Body: body}
}

func luaFuncName(n *luaast.FuncName) string {
	if n == nil {
		return ""
	}
	parts := []string{}
	if n.Func != nil {
		parts = append(parts, luaExprString(n.Func))
	}
	if n.Method != "" {
		parts = append(parts, n.Method)
	}
	if n.Receiver != nil {
		r := luaExprString(n.Receiver)
		if r != "" {
			parts = append([]string{r}, parts...)
		}
	}
	return strings.Join(parts, ".")
}

// LuaASTToJSON parses Lua source and returns its AST in JSON form.
func LuaASTToJSON(src string) ([]byte, error) {
	c, err := ParseLuaAST(src)
	if err != nil {
		return nil, err
	}
	return json.MarshalIndent(c, "", "  ")
}
