package data

import (
	"encoding/csv"
	"os"
	"strconv"
)

// LoadCSV reads a CSV file and returns its rows as a slice of maps.
func LoadCSV(path string) ([]map[string]any, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	r := csv.NewReader(f)
	rows, err := r.ReadAll()
	if err != nil {
		return nil, err
	}
	if len(rows) == 0 {
		return nil, nil
	}
	headers := rows[0]
	out := make([]map[string]any, 0, len(rows)-1)
	for _, rec := range rows[1:] {
		m := map[string]any{}
		for idx, h := range headers {
			var val string
			if idx < len(rec) {
				val = rec[idx]
			}
			if iv, err := strconv.Atoi(val); err == nil {
				m[h] = iv
			} else if fv, err := strconv.ParseFloat(val, 64); err == nil {
				m[h] = fv
			} else {
				m[h] = val
			}
		}
		out = append(out, m)
	}
	return out, nil
}
