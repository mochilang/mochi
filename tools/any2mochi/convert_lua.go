package any2mochi

import "os"

// ConvertLua converts Lua source code to Mochi using the language server.
func ConvertLua(src string) ([]byte, error) {
	return ConvertSource("lua", src)
}

// ConvertLuaFile reads the file at path and converts it to Mochi.
func ConvertLuaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertLua(string(data))
}
