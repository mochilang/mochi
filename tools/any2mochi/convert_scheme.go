package any2mochi

import "os"

// ConvertScheme converts scheme source code to Mochi using the language server.
func ConvertScheme(src string) ([]byte, error) {
	ls := Servers["scheme"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertSchemeFile reads the scheme file and converts it to Mochi.
func ConvertSchemeFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScheme(string(data))
}
