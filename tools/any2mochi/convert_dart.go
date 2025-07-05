package any2mochi

import "os"

// ConvertDart converts dart source code to Mochi using the language server.
func ConvertDart(src string) ([]byte, error) {
	ls := Servers["dart"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertDartFile reads the dart file and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}
