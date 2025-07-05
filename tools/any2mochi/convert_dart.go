package any2mochi

import "os"

// ConvertDart converts Dart source code to Mochi using the language server.
func ConvertDart(src string) ([]byte, error) {
	return ConvertSource("dart", src)
}

// ConvertDartFile reads the file at path and converts it to Mochi.
func ConvertDartFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertDart(string(data))
}
