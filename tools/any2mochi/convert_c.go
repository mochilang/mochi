package any2mochi

import "os"

// ConvertC converts C source code to Mochi using the language server.
func ConvertC(src string) ([]byte, error) {
	return ConvertSource("c", src)
}

// ConvertCFile reads the file at path and converts it to Mochi.
func ConvertCFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertC(string(data))
}
