package any2mochi

import "os"

// ConvertSt converts St source code to Mochi using the language server.
func ConvertSt(src string) ([]byte, error) {
	return ConvertSource("st", src)
}

// ConvertStFile reads the file at path and converts it to Mochi.
func ConvertStFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertSt(string(data))
}
