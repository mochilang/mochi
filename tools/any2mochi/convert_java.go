package any2mochi

import "os"

// ConvertJava converts java source code to Mochi using the language server.
func ConvertJava(src string) ([]byte, error) {
	ls := Servers["java"]
	return ConvertWithServer(ls.Command, ls.Args, ls.LangID, src)
}

// ConvertJavaFile reads the java file and converts it to Mochi.
func ConvertJavaFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertJava(string(data))
}
