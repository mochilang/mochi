package any2mochi

import "os"

// ConvertFs converts Fs source code to Mochi using the language server.
func ConvertFs(src string) ([]byte, error) {
	return ConvertSource("fs", src)
}

// ConvertFsFile reads the file at path and converts it to Mochi.
func ConvertFsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFs(string(data))
}
