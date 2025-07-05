package any2mochi

import "os"

// ConvertRb converts rb source code to Mochi using the language server.
func ConvertRb(src string) ([]byte, error) {
	ls := Servers["rb"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertRbFile reads the rb file and converts it to Mochi.
func ConvertRbFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRb(string(data))
}
