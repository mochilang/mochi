package libmochi

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"reflect"
	"strconv"
	"strings"
)

// RunOptions configures execution of the mochi process.
type RunOptions struct {
	// Binary is the path to the mochi executable. Defaults to "mochi".
	Binary string
	// Dir sets the working directory for the process.
	Dir string
	// Env adds additional environment variables (in key=value form).
	Env []string
}

func (o *RunOptions) binary() string {
	if o != nil && o.Binary != "" {
		return o.Binary
	}
	return "mochi"
}

// Run executes the given Mochi source and returns its standard output.
func Run(source string, opts *RunOptions) (string, error) {
	file, err := os.CreateTemp("", "mochi_*.mochi")
	if err != nil {
		return "", err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(source); err != nil {
		file.Close()
		return "", err
	}
	if err := file.Close(); err != nil {
		return "", err
	}
	return RunFile(file.Name(), opts)
}

// RunFile executes the Mochi file at path and returns its standard output.
func RunFile(path string, opts *RunOptions) (string, error) {
	bin := opts.binary()
	cmd := exec.Command(bin, "run", path)
	if opts != nil {
		if opts.Dir != "" {
			cmd.Dir = opts.Dir
		}
		if len(opts.Env) > 0 {
			cmd.Env = append(os.Environ(), opts.Env...)
		}
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("mochi error: %w\n%s", err, out)
	}
	return string(out), nil
}

// Call defines a function in code and invokes it with args, decoding the JSON result.
func Call(code, fn string, args []any, opts *RunOptions) (any, error) {
	argsLit := make([]string, len(args))
	for i, v := range args {
		s, err := toMochi(v)
		if err != nil {
			return nil, err
		}
		argsLit[i] = s
	}
	snippet := fmt.Sprintf("%s\njson(%s(%s))\n", code, fn, strings.Join(argsLit, ", "))
	out, err := Run(snippet, opts)
	if err != nil {
		return nil, err
	}
	var res any
	if err := json.Unmarshal([]byte(strings.TrimSpace(out)), &res); err != nil {
		return nil, err
	}
	return res, nil
}

func toMochi(v any) (string, error) {
	if v == nil {
		return "null", nil
	}
	rv := reflect.ValueOf(v)
	switch rv.Kind() {
	case reflect.Bool:
		if rv.Bool() {
			return "true", nil
		}
		return "false", nil
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Float32, reflect.Float64:
		return fmt.Sprint(v), nil
	case reflect.String:
		return strconv.Quote(rv.String()), nil
	case reflect.Slice, reflect.Array:
		parts := make([]string, rv.Len())
		for i := 0; i < rv.Len(); i++ {
			s, err := toMochi(rv.Index(i).Interface())
			if err != nil {
				return "", err
			}
			parts[i] = s
		}
		return "[" + strings.Join(parts, ", ") + "]", nil
	case reflect.Map:
		keys := rv.MapKeys()
		parts := make([]string, len(keys))
		for i, k := range keys {
			ks, err := toMochi(k.Interface())
			if err != nil {
				return "", err
			}
			vs, err := toMochi(rv.MapIndex(k).Interface())
			if err != nil {
				return "", err
			}
			parts[i] = fmt.Sprintf("%s: %s", ks, vs)
		}
		return "{" + strings.Join(parts, ", ") + "}", nil
	default:
		return "", fmt.Errorf("unsupported value type: %s", rv.Kind())
	}
}
