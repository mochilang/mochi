package any2mochi

import "os"

// ConvertFortran converts Fortran source code to Mochi using the language server.
func ConvertFortran(src string) ([]byte, error) {
	return ConvertSource("fortran", src)
}

// ConvertFortranFile reads the file at path and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFortran(string(data))
}
