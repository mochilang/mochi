package any2mochi

import "os"

// ConvertLua converts lua source code to Mochi using the language server.
func ConvertLua(src string) ([]byte, error) {
	ls := Servers["lua"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertLuaFile reads the lua file and converts it to Mochi.
func ConvertLuaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertLua(string(data))
}
