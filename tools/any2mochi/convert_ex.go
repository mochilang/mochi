package any2mochi

import "os"

// ConvertEx converts Ex source code to Mochi using the language server.
func ConvertEx(src string) ([]byte, error) {
	return ConvertSource("ex", src)
}

// ConvertExFile reads the file at path and converts it to Mochi.
func ConvertExFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertEx(string(data))
}
