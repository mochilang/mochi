package any2mochi

import "os"

// ConvertFs converts fs source code to Mochi using the language server.
func ConvertFs(src string) ([]byte, error) {
	ls := Servers["fs"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertFsFile reads the fs file and converts it to Mochi.
func ConvertFsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFs(string(data))
}
