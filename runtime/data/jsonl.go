package data

import (
	"bufio"
	"encoding/json"
	"os"
)

// LoadJSONL reads a JSON lines file and returns its rows as a slice of maps.
func LoadJSONL(path string) ([]map[string]any, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	out := []map[string]any{}
	for scanner.Scan() {
		line := scanner.Bytes()
		if len(line) == 0 {
			continue
		}
		var m map[string]any
		if err := json.Unmarshal(line, &m); err != nil {
			return nil, err
		}
		out = append(out, m)
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return out, nil
}
