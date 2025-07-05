package any2mochi

import "os"

// ConvertKt converts kt source code to Mochi using the language server.
func ConvertKt(src string) ([]byte, error) {
	ls := Servers["kt"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertKtFile reads the kt file and converts it to Mochi.
func ConvertKtFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertKt(string(data))
}
