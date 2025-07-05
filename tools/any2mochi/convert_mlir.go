package any2mochi

import "os"

// ConvertMlir converts mlir source code to Mochi using the language server.
func ConvertMlir(src string) ([]byte, error) {
	ls := Servers["mlir"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertMlirFile reads the mlir file and converts it to Mochi.
func ConvertMlirFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertMlir(string(data))
}
