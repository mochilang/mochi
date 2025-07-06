package main

import (
	"bytes"
	"io"
	"os"
	"os/exec"
)

const pyScript = `import sys, json, re
src = sys.stdin.read()
lines = src.splitlines()
structs = []
funcs = []

i = 0
while i < len(lines):
    line = lines[i].strip()
    m = re.match(r'static class (\w+)', line)
    if m:
        name = m.group(1)
        i += 1
        fields = []
        while i < len(lines) and not lines[i].strip().startswith('}'):
            fl = lines[i].strip()
            fm = re.match(r'(\w+) (\w+);', fl)
            if fm:
                fields.append({"name": fm.group(2), "type": fm.group(1)})
            i += 1
        structs.append({"name": name, "fields": fields})
        while i < len(lines) and not lines[i].strip().startswith('}'):
            i += 1
        if i < len(lines):
            i += 1
        continue
    m = re.match(r'(?:public )?static ([\w<>\[\]]+) (\w+)\(([^)]*)\)\s*{', line)
    if m:
        ret = m.group(1)
        fname = m.group(2)
        params = []
        ps = m.group(3).strip()
        if ps:
            for p in ps.split(','):
                p = p.strip()
                if not p:
                    continue
                parts = p.split()
                if len(parts) == 2:
                    params.append({"name": parts[1], "type": parts[0]})
        i += 1
        body = []
        depth = 1
        while i < len(lines) and depth > 0:
            l = lines[i]
            if '{' in l:
                depth += l.count('{')
            if '}' in l:
                depth -= l.count('}')
            if depth > 0:
                body.append(l.strip())
            i += 1
        funcs.append({"name": fname, "ret": ret, "params": params, "body": body})
        continue
    i += 1

json.dump({"structs": structs, "funcs": funcs}, sys.stdout)`

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	cmd := exec.Command("python3", "-c", pyScript)
	cmd.Stdin = bytes.NewReader(data)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	}
	os.Stdout.Write(out.Bytes())
}
