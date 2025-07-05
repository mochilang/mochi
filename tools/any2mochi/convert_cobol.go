package any2mochi

import "os"

// ConvertCobol converts COBOL source code to Mochi using the language server.
func ConvertCobol(src string) ([]byte, error) {
	ls := Servers["cobol"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertCobolFile reads the COBOL file and converts it to Mochi.
func ConvertCobolFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCobol(string(data))
}
