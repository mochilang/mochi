package any2mochi

import (
	"fmt"
	"os"
)

// ConvertFs converts F# source code to Mochi using a simple Go translator.
// The F# code is first parsed via its language server to surface any
// diagnostics before translation.
func ConvertFs(src string) ([]byte, error) {
	ls := Servers["fs"]
	_, diags, err := EnsureAndParse(ls.Command, ls.Args, ls.LangID, src)
	if err != nil {
		return nil, err
	}
	if len(diags) > 0 {
		return nil, fmt.Errorf("%s", formatDiagnostics(src, diags))
	}
	out := translateFs(src)
	return []byte(out), nil
}

// ConvertFsFile reads the fs file and converts it to Mochi.
func ConvertFsFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertFs(string(data))
}
