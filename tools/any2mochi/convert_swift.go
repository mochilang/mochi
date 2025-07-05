package any2mochi

import "os"

// ConvertSwift converts swift source code to Mochi using the language server.
func ConvertSwift(src string) ([]byte, error) {
	ls := Servers["swift"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertSwiftFile reads the swift file and converts it to Mochi.
func ConvertSwiftFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSwift(string(data))
}
