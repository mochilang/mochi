package any2mochi

import "os"

// ConvertClj converts Clojure source code to Mochi using the language server.
func ConvertClj(src string) ([]byte, error) {
	ls := Servers["clj"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertCljFile reads the Clojure file and converts it to Mochi.
func ConvertCljFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertClj(string(data))
}
