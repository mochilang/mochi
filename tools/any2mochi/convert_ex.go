package any2mochi

import "os"

// ConvertEx converts ex source code to Mochi using the language server.
func ConvertEx(src string) ([]byte, error) {
	ls := Servers["ex"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertExFile reads the ex file and converts it to Mochi.
func ConvertExFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertEx(string(data))
}
