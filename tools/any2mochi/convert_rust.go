package any2mochi

import "os"

// ConvertRust converts Rust source code to Mochi using the language server.
func ConvertRust(src string) ([]byte, error) {
	return ConvertSource("rust", src)
}

// ConvertRustFile reads the file at path and converts it to Mochi.
func ConvertRustFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertRust(string(data))
}
