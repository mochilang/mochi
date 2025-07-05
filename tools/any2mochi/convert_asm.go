package any2mochi

import "os"

// ConvertAsm converts Asm source code to Mochi using the language server.
func ConvertAsm(src string) ([]byte, error) {
	return ConvertSource("asm", src)
}

// ConvertAsmFile reads the file at path and converts it to Mochi.
func ConvertAsmFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertAsm(string(data))
}
