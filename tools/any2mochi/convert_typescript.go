package any2mochi

import (
	"os"

	"mochi/tools/ts2mochi"
)

// ConvertTypeScript converts TypeScript source code to Mochi using the ts2mochi helper.
func ConvertTypeScript(src string) ([]byte, error) {
	tmp, err := os.CreateTemp("", "ts-src-*.ts")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	return ts2mochi.ConvertFile(tmp.Name())
}

// ConvertTypeScriptFile reads the TS file and converts it to Mochi.
func ConvertTypeScriptFile(path string) ([]byte, error) {
	return ts2mochi.ConvertFile(path)
}

// ConvertTypeScriptWithJSON converts the source and also returns the parsed
// symbols encoded as JSON.
func ConvertTypeScriptWithJSON(src string) ([]byte, []byte, error) {
	tmp, err := os.CreateTemp("", "ts-src-*.ts")
	if err != nil {
		return nil, nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())
	return ts2mochi.ConvertFileWithJSON(tmp.Name())
}

// ConvertTypeScriptFileWithJSON reads the TS file and converts it to Mochi
// while also returning the parsed symbols as JSON.
func ConvertTypeScriptFileWithJSON(path string) ([]byte, []byte, error) {
	return ts2mochi.ConvertFileWithJSON(path)
}
