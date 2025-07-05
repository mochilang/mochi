package any2mochi

import (
    "bytes"
    "encoding/json"
    "fmt"
    "os/exec"
    "path/filepath"
    "strings"
)

// ParseLuaString converts Lua source into simple Mochi statements by invoking
// a Node CLI that returns the Lua AST in JSON format. Only top-level
// __print calls are translated to Mochi print statements.
func ParseLuaString(src string) ([]string, error) {
    ast, err := luaAST(src)
    if err != nil {
        return nil, err
    }
    body, ok := ast["body"].([]interface{})
    if !ok {
        return nil, fmt.Errorf("unexpected ast")
    }
    var out []string
    for _, st := range body {
        m, ok := st.(map[string]interface{})
        if !ok || m["type"] != "CallStatement" {
            continue
        }
        expr, ok := m["expression"].(map[string]interface{})
        if !ok || expr["type"] != "CallExpression" {
            continue
        }
        base, ok := expr["base"].(map[string]interface{})
        if !ok || base["type"] != "Identifier" || base["name"] != "__print" {
            continue
        }
        args, ok := expr["arguments"].([]interface{})
        if !ok {
            continue
        }
        var parts []string
        for _, a := range args {
            if m, ok := a.(map[string]interface{}); ok {
                if s := luaExprString(m); s != "" {
                    parts = append(parts, s)
                }
            }
        }
        out = append(out, "print("+strings.Join(parts, ", ")+")")
    }
    return out, nil
}

func luaExprString(m map[string]interface{}) string {
    switch m["type"] {
    case "StringLiteral":
        if raw, ok := m["raw"].(string); ok {
            return raw
        }
    case "NumericLiteral":
        if raw, ok := m["raw"].(string); ok {
            return raw
        }
        if v, ok := m["value"].(float64); ok {
            return fmt.Sprintf("%v", v)
        }
    case "BooleanLiteral":
        if v, ok := m["value"].(bool); ok {
            if v {
                return "true"
            }
            return "false"
        }
    case "Identifier":
        if name, ok := m["name"].(string); ok {
            return name
        }
    }
    return ""
}

func luaAST(src string) (map[string]interface{}, error) {
    root, err := repoRoot()
    if err != nil {
        return nil, err
    }
    script := filepath.Join(root, "tools", "any2mochi", "lua_ast.js")
    cmd := exec.Command("node", script)
    cmd.Stdin = strings.NewReader(src)
    var out bytes.Buffer
    cmd.Stdout = &out
    if err := cmd.Run(); err != nil {
        return nil, fmt.Errorf("node: %v", err)
    }
    var ast map[string]interface{}
    if err := json.Unmarshal(out.Bytes(), &ast); err != nil {
        return nil, err
    }
    return ast, nil
}
