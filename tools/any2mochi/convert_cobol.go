package any2mochi

import "os"

// ConvertCobol converts Cobol source code to Mochi using the language server.
func ConvertCobol(src string) ([]byte, error) {
	return ConvertSource("cobol", src)
}

// ConvertCobolFile reads the file at path and converts it to Mochi.
func ConvertCobolFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCobol(string(data))
}
