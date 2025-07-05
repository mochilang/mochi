package any2mochi

import "os"

// ConvertJvm converts JVM assembly source to Mochi using the language server.
func ConvertJvm(src string) ([]byte, error) {
	ls := Servers["jvm"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertJvmFile reads the jvm file and converts it to Mochi.
func ConvertJvmFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertJvm(string(data))
}
