package any2mochi

import "os"

// ConvertGo converts Go source code to a minimal Mochi representation using
// symbols reported by gopls.
func ConvertGo(src string) ([]byte, error) {
	ls := Servers["go"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertGo(string(data))
}
