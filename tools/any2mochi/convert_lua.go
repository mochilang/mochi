package any2mochi

import (
	"fmt"
	"os"
	"strings"
)

// ConvertLua converts Lua source code to Mochi using simple regex parsing.
func ConvertLua(src string) ([]byte, error) {
	lines := ParseLuaString(src)
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
