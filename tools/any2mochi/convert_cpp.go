package any2mochi

import "os"

// ConvertCpp converts Cpp source code to Mochi using the language server.
func ConvertCpp(src string) ([]byte, error) {
	return ConvertSource("cpp", src)
}

// ConvertCppFile reads the file at path and converts it to Mochi.
func ConvertCppFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertCpp(string(data))
}
