package any2mochi

import "os"

// ConvertRust converts rust source code to Mochi using the language server.
func ConvertRust(src string) ([]byte, error) {
	ls := Servers["rust"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertRustFile reads the rust file and converts it to Mochi.
func ConvertRustFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRust(string(data))
}
