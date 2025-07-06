package any2mochi

import (
	"bytes"
	"encoding/json"
	"os/exec"
)

type dartParamJSON struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type dartFuncJSON struct {
	Name   string          `json:"name"`
	Params []dartParamJSON `json:"params"`
	Ret    string          `json:"ret"`
	Body   []string        `json:"body"`
}

type dartAst struct {
	Functions []dartFuncJSON `json:"functions"`
}

func parseDartCLI(src string) ([]dartFunc, error) {
	cmd := exec.Command("dartast")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var raw dartAst
	if err := json.Unmarshal(out.Bytes(), &raw); err != nil {
		return nil, err
	}
	funcs := make([]dartFunc, 0, len(raw.Functions))
	for _, f := range raw.Functions {
		params := make([]dartParam, 0, len(f.Params))
		for _, p := range f.Params {
			params = append(params, dartParam{name: p.Name, typ: dartToMochiType(p.Type)})
		}
		funcs = append(funcs, dartFunc{Name: f.Name, Params: params, Body: f.Body, Ret: dartToMochiType(f.Ret)})
	}
	return funcs, nil
}
