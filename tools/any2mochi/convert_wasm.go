package any2mochi

import "os"

// ConvertWasm converts wasm source code to Mochi using the language server.
func ConvertWasm(src string) ([]byte, error) {
	ls := Servers["wasm"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertWasmFile reads the wasm file and converts it to Mochi.
func ConvertWasmFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertWasm(string(data))
}
