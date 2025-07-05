package any2mochi

import "os"

// ConvertOcaml converts Ocaml source code to Mochi using the language server.
func ConvertOcaml(src string) ([]byte, error) {
	return ConvertSource("ocaml", src)
}

// ConvertOcamlFile reads the file at path and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}
