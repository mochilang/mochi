package any2mochi

import "os"

// ConvertC converts c source code to Mochi using the language server.
func ConvertC(src string) ([]byte, error) {
	ls := Servers["c"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertCFile reads the c file and converts it to Mochi.
func ConvertCFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertC(string(data))
}
