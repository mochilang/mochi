package any2mochi

import "os"

// ConvertOcaml converts ocaml source code to Mochi using the language server.
func ConvertOcaml(src string) ([]byte, error) {
	ls := Servers["ocaml"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertOcamlFile reads the ocaml file and converts it to Mochi.
func ConvertOcamlFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertOcaml(string(data))
}
