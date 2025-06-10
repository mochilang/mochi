package data

import (
	"bufio"
	"encoding/json"
	"io"
	"os"
)

// LoadJSONL reads a JSON lines file and returns its rows as a slice of maps.
func LoadJSONL(path string) ([]map[string]any, error) {
	var r io.Reader
	if path == "" || path == "-" {
		r = os.Stdin
	} else {
		f, err := os.Open(path)
		if err != nil {
			return nil, err
		}
		defer f.Close()
		r = f
	}
	return LoadJSONLReader(r)
}

// LoadJSONLReader reads JSONL data from the provided reader.
func LoadJSONLReader(r io.Reader) ([]map[string]any, error) {
	scanner := bufio.NewScanner(r)
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

// SaveJSONL writes rows to a JSON lines file. If path is empty or "-",
// data is written to stdout.
func SaveJSONL(rows []map[string]any, path string) error {
	var w io.Writer
	if path == "" || path == "-" {
		w = os.Stdout
	} else {
		f, err := os.Create(path)
		if err != nil {
			return err
		}
		defer f.Close()
		w = f
	}
	return SaveJSONLWriter(rows, w)
}

// SaveJSONLWriter writes rows to the provided writer in JSONL format.
func SaveJSONLWriter(rows []map[string]any, w io.Writer) error {
	bw := bufio.NewWriter(w)
	enc := json.NewEncoder(bw)
	for _, row := range rows {
		if err := enc.Encode(row); err != nil {
			return err
		}
	}
	return bw.Flush()
}
