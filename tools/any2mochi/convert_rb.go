package any2mochi

import "os"

// ConvertRb converts Rb source code to Mochi using the language server.
func ConvertRb(src string) ([]byte, error) {
	return ConvertSource("rb", src)
}

// ConvertRbFile reads the file at path and converts it to Mochi.
func ConvertRbFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRb(string(data))
}
