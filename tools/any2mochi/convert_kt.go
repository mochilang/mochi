package any2mochi

import "os"

// ConvertKt converts Kt source code to Mochi using the language server.
func ConvertKt(src string) ([]byte, error) {
	return ConvertSource("kt", src)
}

// ConvertKtFile reads the file at path and converts it to Mochi.
func ConvertKtFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertKt(string(data))
}
