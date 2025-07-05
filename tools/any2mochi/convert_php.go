package any2mochi

import "os"

// ConvertPhp converts Php source code to Mochi using the language server.
func ConvertPhp(src string) ([]byte, error) {
	return ConvertSource("php", src)
}

// ConvertPhpFile reads the file at path and converts it to Mochi.
func ConvertPhpFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPhp(string(data))
}
