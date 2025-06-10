package data

import (
	"encoding/csv"
	"encoding/json"
	"os"
	"sort"
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

// SaveCSV writes rows to a CSV file using the given options.
func SaveCSV(rows []map[string]any, path string, header bool, delim rune) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer f.Close()
	w := csv.NewWriter(f)
	if delim != 0 {
		w.Comma = delim
	}

	if len(rows) == 0 {
		if header {
			w.Write([]string{})
		}
		w.Flush()
		return w.Error()
	}

	// Determine column order from first row
	headers := make([]string, 0, len(rows[0]))
	for k := range rows[0] {
		headers = append(headers, k)
	}
	sort.Strings(headers)
	if header {
		if err := w.Write(headers); err != nil {
			return err
		}
	}
	for _, row := range rows {
		rec := make([]string, len(headers))
		for i, h := range headers {
			if v, ok := row[h]; ok {
				switch x := v.(type) {
				case nil:
				case string:
					rec[i] = x
				case int:
					rec[i] = strconv.Itoa(x)
				case int64:
					rec[i] = strconv.FormatInt(x, 10)
				case float64:
					rec[i] = strconv.FormatFloat(x, 'f', -1, 64)
				case bool:
					rec[i] = strconv.FormatBool(x)
				default:
					b, _ := json.Marshal(x)
					rec[i] = string(b)
				}
			}
		}
		if err := w.Write(rec); err != nil {
			return err
		}
	}
	w.Flush()
	return w.Error()
}
