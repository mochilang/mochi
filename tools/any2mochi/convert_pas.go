package any2mochi

import "os"

// ConvertPas converts Pas source code to Mochi using the language server.
func ConvertPas(src string) ([]byte, error) {
	return ConvertSource("pas", src)
}

// ConvertPasFile reads the file at path and converts it to Mochi.
func ConvertPasFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPas(string(data))
}
