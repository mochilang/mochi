package any2mochi

import "os"

// ConvertZig converts zig source code to Mochi using the language server.
func ConvertZig(src string) ([]byte, error) {
	ls := Servers["zig"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertZigFile reads the zig file and converts it to Mochi.
func ConvertZigFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertZig(string(data))
}
