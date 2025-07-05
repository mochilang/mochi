package any2mochi

import "os"

// ConvertTypeScript converts TypeScript source code to a minimal Mochi representation using the language server.
func ConvertTypeScript(src string) ([]byte, error) {
	ls := Servers["typescript"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertTypeScriptFile reads the TS file and converts it to Mochi.
func ConvertTypeScriptFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertTypeScript(string(data))
}

// ConvertTypeScriptWithJSON converts the source and also returns the parsed
// symbols encoded as JSON.
func ConvertTypeScriptWithJSON(src string) ([]byte, []byte, error) {
	ls := Servers["typescript"]
	return ConvertWithServerJSON(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertTypeScriptFileWithJSON reads the TS file and converts it to Mochi
// while also returning the parsed symbols as JSON.
func ConvertTypeScriptFileWithJSON(path string) ([]byte, []byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, nil, err
	}
	return ConvertTypeScriptWithJSON(string(data))
}
