package any2mochi

import "os"

// ConvertCs converts cs source code to Mochi using the language server.
func ConvertCs(src string) ([]byte, error) {
	ls := Servers["cs"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertCsFile reads the cs file and converts it to Mochi.
func ConvertCsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCs(string(data))
}
