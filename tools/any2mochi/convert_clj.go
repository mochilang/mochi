package any2mochi

import "os"

// ConvertClj converts Clj source code to Mochi using the language server.
func ConvertClj(src string) ([]byte, error) {
	return ConvertSource("clj", src)
}

// ConvertCljFile reads the file at path and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}
