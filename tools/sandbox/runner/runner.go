package runner

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

type Request struct {
	Files map[string]string `json:"files"`
}

type Response struct {
	Output string `json:"output"`
	Error  string `json:"error,omitempty"`
}

// Run executes the provided command after writing files from the request.
func Run(cmdArgs []string) {
	var req Request
	if err := json.NewDecoder(os.Stdin).Decode(&req); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	for name, content := range req.Files {
		if err := os.WriteFile(name, []byte(content), 0644); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
	out, err := cmd.CombinedOutput()
	resp := Response{Output: string(out)}
	if err != nil {
		resp.Error = err.Error()
	}
	if err := json.NewEncoder(os.Stdout).Encode(resp); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if err != nil {
		os.Exit(1)
	}
}
