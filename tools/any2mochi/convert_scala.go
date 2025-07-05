package any2mochi

import "os"

// ConvertScala converts Scala source code to Mochi using the language server.
func ConvertScala(src string) ([]byte, error) {
	return ConvertSource("scala", src)
}

// ConvertScalaFile reads the file at path and converts it to Mochi.
func ConvertScalaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScala(string(data))
}
