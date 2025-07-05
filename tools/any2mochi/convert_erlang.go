package any2mochi

import "os"

// ConvertErlang converts erlang source code to Mochi using the language server.
func ConvertErlang(src string) ([]byte, error) {
	ls := Servers["erlang"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertErlangFile reads the erlang file and converts it to Mochi.
func ConvertErlangFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertErlang(string(data))
}
