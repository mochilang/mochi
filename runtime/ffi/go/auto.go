package goffi

import (
	"encoding/json"
	"fmt"
	"os"
	"strings"

	goexec "mochi/runtime/go"
)

// AttrAuto retrieves or calls a symbol from a package using `go run` with reflection.
func AttrAuto(pkg, name string, args ...any) (any, error) {
	src := fmt.Sprintf(`package main
import (
    "encoding/json"
    "fmt"
    "os"
    "reflect"
    m "%s"
)
func main() {
    var args []any
    json.Unmarshal([]byte(os.Getenv("MOCHI_ARGS")), &args)
    v := reflect.ValueOf(m.%s)
    if v.Kind() == reflect.Func {
        in := make([]reflect.Value, len(args))
        for i, a := range args {
            val := reflect.ValueOf(a)
            param := v.Type().In(i)
            if val.Type().ConvertibleTo(param) {
                val = val.Convert(param)
            }
            in[i] = val
        }
        outs := v.Call(in)
        var res any
        if len(outs) == 1 {
            res = outs[0].Interface()
        } else if len(outs) > 1 {
            tmp := make([]any, len(outs))
            for i, o := range outs {
                tmp[i] = o.Interface()
            }
            res = tmp
        }
        data, _ := json.Marshal(res)
        fmt.Print(string(data))
        return
    }
    if len(args) != 0 {
        fmt.Fprintf(os.Stderr, "%%s.%%s is not callable", "%%s", "%%s")
        os.Exit(1)
    }
    data, _ := json.Marshal(v.Interface())
    fmt.Print(string(data))
}
`, pkg, name)

	file, err := os.CreateTemp("", "mochi_go_*.go")
	if err != nil {
		return nil, err
	}
	if _, err := file.WriteString(src); err != nil {
		file.Close()
		os.Remove(file.Name())
		return nil, err
	}
	file.Close()
	defer os.Remove(file.Name())

	if args == nil {
		args = []any{}
	}
	data, err := json.Marshal(args)
	if err != nil {
		return nil, err
	}

	cmd := goexec.Command("run", file.Name())
	cmd.Env = append(os.Environ(), "MOCHI_ARGS="+string(data))
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("go run error: %w\n%s", err, out)
	}

	var result any
	if len(out) == 0 {
		return nil, nil
	}
	if err := json.Unmarshal(out, &result); err != nil {
		return nil, fmt.Errorf("decode error: %w\noutput: %s", err, out)
	}
	return result, nil
}

// CallAuto invokes pkg.fn with args using AttrAuto.
func CallAuto(name string, args ...any) (any, error) {
	parts := strings.SplitN(name, ".", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("goffi: name must be pkg.Func")
	}
	return AttrAuto(parts[0], parts[1], args...)
}
