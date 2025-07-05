package any2mochi

import "os"

// ConvertRkt converts Rkt source code to Mochi using the language server.
func ConvertRkt(src string) ([]byte, error) {
	return ConvertSource("rkt", src)
}

// ConvertRktFile reads the file at path and converts it to Mochi.
func ConvertRktFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRkt(string(data))
}
