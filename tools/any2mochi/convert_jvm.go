package any2mochi

import "os"

// ConvertJvm converts Jvm source code to Mochi using the language server.
func ConvertJvm(src string) ([]byte, error) {
	return ConvertSource("jvm", src)
}

// ConvertJvmFile reads the file at path and converts it to Mochi.
func ConvertJvmFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertJvm(string(data))
}
