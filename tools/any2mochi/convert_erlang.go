package any2mochi

import "os"

// ConvertErlang converts Erlang source code to Mochi using the language server.
func ConvertErlang(src string) ([]byte, error) {
	return ConvertSource("erlang", src)
}

// ConvertErlangFile reads the file at path and converts it to Mochi.
func ConvertErlangFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertErlang(string(data))
}
