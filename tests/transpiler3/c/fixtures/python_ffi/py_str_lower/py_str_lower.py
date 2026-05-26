#!/usr/bin/env python3
# Python companion for py_str_lower fixture (Phase 10.3).
import sys
import json

def py_lower(s):
    return s.lower()

for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    try:
        req = json.loads(line)
    except json.JSONDecodeError:
        print('{"error":"parse error"}')
        sys.stdout.flush()
        continue
    fn = req.get("fn", "")
    args = req.get("args", [])
    if fn == "py_lower":
        result = py_lower(str(args[0]))
        print(json.dumps({"result": result}))
    else:
        print(json.dumps({"error": f"unknown function {fn}"}))
    sys.stdout.flush()
