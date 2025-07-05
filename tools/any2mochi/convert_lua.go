package any2mochi

import (
    "fmt"
    "os"
    "strings"
)

// ConvertLua converts Lua source code to Mochi by parsing the Lua AST
// using the Node-based CLI and translating simple print statements.
func ConvertLua(src string) ([]byte, error) {
    lines, err := ParseLuaString(src)
    if err != nil {
        return nil, err
    }
    if len(lines) == 0 {
        return nil, fmt.Errorf("no convertible statements found")
    }
    return []byte(strings.Join(lines, "\n")), nil
}

// ConvertLuaFile reads the Lua file and converts it to Mochi.
func ConvertLuaFile(path string) ([]byte, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, err
    }
    return ConvertLua(string(data))
}
