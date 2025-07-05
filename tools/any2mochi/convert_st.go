package any2mochi

import "os"

// ConvertSt converts st source code to Mochi using the language server.
func ConvertSt(src string) ([]byte, error) {
	ls := Servers["st"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertStFile reads the st file and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}
