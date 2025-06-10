package data

import (
	"encoding/csv"
	"encoding/json"
	"io"
	"os"
	"sort"
	"strconv"
)

// LoadCSV reads a CSV file and returns its rows as a slice of maps.
func LoadCSV(path string) ([]map[string]any, error) {
	var reader io.Reader
	if path == "" || path == "-" {
		reader = os.Stdin
	} else {
		f, err := os.Open(path)
		if err != nil {
			return nil, err
		}
		defer f.Close()
		reader = f
	}
	return LoadCSVReader(reader)
}

// LoadCSVReader reads CSV data from the provided reader.
func LoadCSVReader(r io.Reader) ([]map[string]any, error) {
	csvr := csv.NewReader(r)
	rows, err := csvr.ReadAll()
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
	var writer io.Writer
	if path == "" || path == "-" {
		writer = os.Stdout
	} else {
		f, err := os.Create(path)
		if err != nil {
			return err
		}
		defer f.Close()
		writer = f
	}
	return SaveCSVWriter(rows, writer, header, delim)
}

// SaveCSVWriter writes CSV rows to the provided writer.
func SaveCSVWriter(rows []map[string]any, w io.Writer, header bool, delim rune) error {
	csvw := csv.NewWriter(w)
	if delim != 0 {
		csvw.Comma = delim
	}

	if len(rows) == 0 {
		if header {
			csvw.Write([]string{})
		}
		csvw.Flush()
		return csvw.Error()
	}

	// Determine column order from first row
	headers := make([]string, 0, len(rows[0]))
	for k := range rows[0] {
		headers = append(headers, k)
	}
	sort.Strings(headers)
	if header {
		if err := csvw.Write(headers); err != nil {
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
		if err := csvw.Write(rec); err != nil {
			return err
		}
	}
	csvw.Flush()
	return csvw.Error()
}
