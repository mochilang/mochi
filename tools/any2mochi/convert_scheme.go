package any2mochi

import "os"

// ConvertScheme converts Scheme source code to Mochi using the language server.
func ConvertScheme(src string) ([]byte, error) {
	return ConvertSource("scheme", src)
}

// ConvertSchemeFile reads the file at path and converts it to Mochi.
func ConvertSchemeFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScheme(string(data))
}
