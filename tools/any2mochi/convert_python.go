package any2mochi

import "os"

// ConvertPython converts Python source code to a minimal Mochi representation using pyright.
func ConvertPython(src string) ([]byte, error) {
	ls := Servers["python"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertPythonFile reads the Python file at path and converts it to Mochi.
func ConvertPythonFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertPython(string(data))
}
