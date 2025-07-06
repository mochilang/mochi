package any2mochi

import (
	"bytes"
	"encoding/json"
	"os/exec"
)

type FsVar struct {
	Name string `json:"name"`
	Expr string `json:"expr"`
}

type FsProgram struct {
	Vars   []FsVar  `json:"vars"`
	Prints []string `json:"prints"`
}

func parseFsAST(src string) (*FsProgram, error) {
	cmd := exec.Command("fsparse")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var prog FsProgram
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
