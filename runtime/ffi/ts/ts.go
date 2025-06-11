package ts

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"

	"mochi/runtime/ffi"
)

// Ensure *Runtime implements ffi.Caller.
var _ ffi.Caller = (*Runtime)(nil)

type Runtime struct{}

func NewRuntime() *Runtime { return &Runtime{} }

var defaultRuntime = NewRuntime()

func Invoke(name string, args ...any) (any, error) {
	return defaultRuntime.Call(name, args...)
}

func (r *Runtime) Call(name string, args ...any) (any, error) {
	parts := strings.SplitN(name, ":", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("ts: name must be module:function")
	}
	return Attr(parts[0], parts[1], args...)
}

func (r *Runtime) Exec(code string, args ...any) (any, error) {
	return Exec(code, args...)
}

func Attr(module, name string, args ...any) (any, error) {
	src := fmt.Sprintf(`import * as mod from "%s";
const attr = mod["%s"];
const args = JSON.parse(Deno.env.get("MOCHI_ARGS") ?? "[]");
let res;
if (typeof attr === 'function') {
  res = await attr(...args);
} else {
  if (args.length !== 0) throw new Error("%s.%s is not callable");
  res = attr;
}
console.log(JSON.stringify(res));
`, module, name, module, name)
	return run(src, args)
}

func Exec(code string, args ...any) (any, error) {
	indented := indent(code, "  ")
	src := fmt.Sprintf(`const args = JSON.parse(Deno.env.get("MOCHI_ARGS") ?? "[]");
async function __mochi_fn(args) {
%s
}
const res = await __mochi_fn(args);
console.log(JSON.stringify(res));
`, indented)
	return run(src, args)
}

func run(code string, args []any) (any, error) {
	file, err := os.CreateTemp("", "mochi_ts_*.ts")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(code); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	if args == nil {
		args = []any{}
	}
	data, err := json.Marshal(args)
	if err != nil {
		return nil, err
	}

	cmd := exec.Command("deno", "run", "--quiet", "--unsafely-ignore-certificate-errors", "--allow-env=MOCHI_ARGS", "--allow-read", file.Name())
	cmd.Env = append(os.Environ(), "MOCHI_ARGS="+string(data))
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("deno error: %w\n%s", err, out)
	}

	var result any
	if err := json.Unmarshal(out, &result); err != nil {
		return nil, fmt.Errorf("decode error: %w\noutput: %s", err, out)
	}
	return result, nil
}

func indent(code, prefix string) string {
	if code == "" {
		return ""
	}
	lines := strings.Split(code, "\n")
	for i, l := range lines {
		lines[i] = prefix + l
	}
	return strings.Join(lines, "\n")
}
