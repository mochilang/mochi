package any2mochi

import "os"

// ConvertHs converts hs source code to Mochi using the language server.
func ConvertHs(src string) ([]byte, error) {
	ls := Servers["hs"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertHsFile reads the hs file and converts it to Mochi.
func ConvertHsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertHs(string(data))
}
