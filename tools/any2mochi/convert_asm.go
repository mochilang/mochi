package any2mochi

import "os"

// ConvertAsm converts assembly source code to Mochi using the language server.
func ConvertAsm(src string) ([]byte, error) {
	ls := Servers["asm"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertAsmFile reads the asm file and converts it to Mochi.
func ConvertAsmFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertAsm(string(data))
}
