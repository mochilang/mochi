package any2mochi

import "os"

// ConvertPhp converts php source code to Mochi using the language server.
func ConvertPhp(src string) ([]byte, error) {
	ls := Servers["php"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertPhpFile reads the php file and converts it to Mochi.
func ConvertPhpFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPhp(string(data))
}
