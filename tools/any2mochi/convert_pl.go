package any2mochi

import "os"

// ConvertPl converts Pl source code to Mochi using the language server.
func ConvertPl(src string) ([]byte, error) {
	return ConvertSource("pl", src)
}

// ConvertPlFile reads the file at path and converts it to Mochi.
func ConvertPlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPl(string(data))
}
