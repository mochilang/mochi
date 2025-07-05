package any2mochi

import "os"

// ConvertSwift converts Swift source code to Mochi using the language server.
func ConvertSwift(src string) ([]byte, error) {
	return ConvertSource("swift", src)
}

// ConvertSwiftFile reads the file at path and converts it to Mochi.
func ConvertSwiftFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSwift(string(data))
}
