package any2mochi

import "os"

// ConvertPas converts pas source code to Mochi using the language server.
func ConvertPas(src string) ([]byte, error) {
	ls := Servers["pas"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertPasFile reads the pas file and converts it to Mochi.
func ConvertPasFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPas(string(data))
}
