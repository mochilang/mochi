package any2mochi

import "os"

// ConvertCs converts Cs source code to Mochi using the language server.
func ConvertCs(src string) ([]byte, error) {
	return ConvertSource("cs", src)
}

// ConvertCsFile reads the file at path and converts it to Mochi.
func ConvertCsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCs(string(data))
}
