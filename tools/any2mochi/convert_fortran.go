package any2mochi

import "os"

// ConvertFortran converts fortran source code to Mochi using the language server.
func ConvertFortran(src string) ([]byte, error) {
	ls := Servers["fortran"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertFortranFile reads the fortran file and converts it to Mochi.
func ConvertFortranFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFortran(string(data))
}
