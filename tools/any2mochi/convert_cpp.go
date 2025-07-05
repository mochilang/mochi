package any2mochi

import "os"

// ConvertCpp converts cpp source code to Mochi using the language server.
func ConvertCpp(src string) ([]byte, error) {
	ls := Servers["cpp"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertCppFile reads the cpp file and converts it to Mochi.
func ConvertCppFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCpp(string(data))
}
