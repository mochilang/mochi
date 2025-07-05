package any2mochi

import "os"

// ConvertPl converts pl source code to Mochi using the language server.
func ConvertPl(src string) ([]byte, error) {
	ls := Servers["pl"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertPlFile reads the pl file and converts it to Mochi.
func ConvertPlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPl(string(data))
}
