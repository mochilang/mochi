package dart

import (
	"bytes"
	"encoding/json"
	"os/exec"
)

type paramJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type funcJSON struct {
	Name   string      `json:"name"`
	Params []paramJSON `json:"params"`
	Ret    string      `json:"ret"`
	Body   []string    `json:"body"`
}

type ast struct {
	Functions []funcJSON `json:"functions"`
}

func parseCLI(src string) ([]function, error) {
	cmd := exec.Command("dartast")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var raw ast
	if err := json.Unmarshal(out.Bytes(), &raw); err != nil {
		return nil, err
	}
	funcs := make([]function, 0, len(raw.Functions))
	for _, f := range raw.Functions {
		params := make([]param, 0, len(f.Params))
		for _, p := range f.Params {
			params = append(params, param{name: p.Name, typ: toMochiType(p.Type)})
		}
		funcs = append(funcs, function{Name: f.Name, Params: params, Body: f.Body, Ret: toMochiType(f.Ret)})
	}
	return funcs, nil
}
