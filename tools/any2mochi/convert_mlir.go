package any2mochi

import "os"

// ConvertMlir converts Mlir source code to Mochi using the language server.
func ConvertMlir(src string) ([]byte, error) {
	return ConvertSource("mlir", src)
}

// ConvertMlirFile reads the file at path and converts it to Mochi.
func ConvertMlirFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertMlir(string(data))
}
