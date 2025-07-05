package any2mochi

import "os"

// ConvertRkt converts rkt source code to Mochi using the language server.
func ConvertRkt(src string) ([]byte, error) {
	ls := Servers["rkt"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertRktFile reads the rkt file and converts it to Mochi.
func ConvertRktFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRkt(string(data))
}
