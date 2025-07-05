package any2mochi

import "os"

// ConvertGo converts Go source code to Mochi using the built-in translator.
func ConvertGo(src string) ([]byte, error) {
	tmp, err := os.CreateTemp("", "go-src-*.go")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	return convertGo(tmp.Name())
}

// ConvertGoFile reads the Go file at path and converts it to Mochi.
func ConvertGoFile(path string) ([]byte, error) {
	return convertGo(path)
}
