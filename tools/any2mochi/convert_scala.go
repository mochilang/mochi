package any2mochi

import "os"

// ConvertScala converts scala source code to Mochi using the language server.
func ConvertScala(src string) ([]byte, error) {
	ls := Servers["scala"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertScalaFile reads the scala file and converts it to Mochi.
func ConvertScalaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertScala(string(data))
}
