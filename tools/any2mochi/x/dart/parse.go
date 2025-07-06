package dart

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

type paramJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type funcJSON struct {
	Name      string      `json:"name"`
	Params    []paramJSON `json:"params"`
	Ret       string      `json:"ret"`
	Body      []string    `json:"body"`
	StartLine int         `json:"start"`
	EndLine   int         `json:"end"`
	Doc       string      `json:"doc,omitempty"`
}

type fieldJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type classJSON struct {
	Name      string      `json:"name"`
	Fields    []fieldJSON `json:"fields"`
	StartLine int         `json:"start"`
	EndLine   int         `json:"end"`
	Doc       string      `json:"doc,omitempty"`
}

type ast struct {
	Functions []funcJSON  `json:"functions"`
	Classes   []classJSON `json:"classes"`
}

func parseCLI(src string) ([]function, []class, error) {
	if dartPath, err := exec.LookPath("dart"); err == nil {
		root, rErr := repoRoot()
		if rErr == nil {
			script := filepath.Join(root, "tools", "any2mochi", "x", "dart", "parser.dart")
			cmd := exec.Command(dartPath, script)
			cmd.Stdin = bytes.NewBufferString(src)
			var out bytes.Buffer
			var errBuf bytes.Buffer
			cmd.Stdout = &out
			cmd.Stderr = &errBuf
			if err := cmd.Run(); err == nil {
				f, c, dErr := decodeFuncs(out.Bytes())
				return f, c, dErr
			}
		}
	}

	path, err := exec.LookPath("dartast")
	if err != nil {
		root, rErr := repoRoot()
		if rErr != nil {
			return nil, nil, rErr
		}
		cmd := exec.Command("go", "run", filepath.Join(root, "cmd", "dartast"))
		cmd.Stdin = bytes.NewBufferString(src)
		var out bytes.Buffer
		var errBuf bytes.Buffer
		cmd.Stdout = &out
		cmd.Stderr = &errBuf
		if runErr := cmd.Run(); runErr != nil {
			if errBuf.Len() > 0 {
				return nil, nil, fmt.Errorf("%v: %s", runErr, errBuf.String())
			}
			return nil, nil, runErr
		}
		f, c, dErr := decodeFuncs(out.Bytes())
		return f, c, dErr
	}
	cmd := exec.Command(path)
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	var errBuf bytes.Buffer
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, nil, fmt.Errorf("%v: %s", err, errBuf.String())
		}
		return nil, nil, err
	}
	f, c, dErr := decodeFuncs(out.Bytes())
	return f, c, dErr
}

func decodeFuncs(data []byte) ([]function, []class, error) {
	var raw ast
	if err := json.Unmarshal(data, &raw); err != nil {
		return nil, nil, err
	}
	funcs := make([]function, 0, len(raw.Functions))
	for _, f := range raw.Functions {
		params := make([]param, 0, len(f.Params))
		for _, p := range f.Params {
			params = append(params, param{name: p.Name, typ: toMochiType(p.Type)})
		}
		funcs = append(funcs, function{
			Name:      f.Name,
			Params:    params,
			Body:      f.Body,
			Ret:       toMochiType(f.Ret),
			StartLine: f.StartLine,
			EndLine:   f.EndLine,
			Doc:       f.Doc,
		})
	}
	classes := make([]class, 0, len(raw.Classes))
	for _, c := range raw.Classes {
		fields := make([]param, 0, len(c.Fields))
		for _, p := range c.Fields {
			fields = append(fields, param{name: p.Name, typ: toMochiType(p.Type)})
		}
		classes = append(classes, class{Name: c.Name, Fields: fields, StartLine: c.StartLine, EndLine: c.EndLine, Doc: c.Doc})
	}
	return funcs, classes, nil
}

func repoRoot() (string, error) {
	dir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	return "", os.ErrNotExist
}
