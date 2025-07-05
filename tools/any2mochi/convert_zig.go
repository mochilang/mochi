package any2mochi

import "os"

// ConvertZig converts Zig source code to Mochi using the language server.
func ConvertZig(src string) ([]byte, error) {
	return ConvertSource("zig", src)
}

// ConvertZigFile reads the file at path and converts it to Mochi.
func ConvertZigFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertZig(string(data))
}
