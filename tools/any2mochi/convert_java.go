package any2mochi

import "os"

// ConvertJava converts Java source code to Mochi using the language server.
func ConvertJava(src string) ([]byte, error) {
	return ConvertSource("java", src)
}

// ConvertJavaFile reads the file at path and converts it to Mochi.
func ConvertJavaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertJava(string(data))
}
