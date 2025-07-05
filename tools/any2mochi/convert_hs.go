package any2mochi

import "os"

// ConvertHs converts Hs source code to Mochi using the language server.
func ConvertHs(src string) ([]byte, error) {
	return ConvertSource("hs", src)
}

// ConvertHsFile reads the file at path and converts it to Mochi.
func ConvertHsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertHs(string(data))
}
