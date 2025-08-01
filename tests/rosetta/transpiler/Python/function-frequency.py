# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-30 21:42 +0700
import json
import os
import resource
import time

import sys
sys.set_int_max_str_digits(0)
import os
if os.path.dirname(__file__) in sys.path:
    sys.path.remove(os.path.dirname(__file__))


_now_seed = 0
_now_seeded = False
s = os.getenv("MOCHI_NOW_SEED")
if s and s != "":
    try:
        _now_seed = int(s)
        _now_seeded = True
    except Exception:
        pass

def _now():
    global _now_seed
    if _now_seeded:
        _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647
        return _now_seed
    return int(time.time_ns())

def join(xs, sep):
    res = ""
    i = 0
    while i < len(xs):
        if i > 0:
            res = res + sep
        res = res + xs[i]
        i = i + 1
    return res
def sortPairs(xs):
    arr = xs
    i = 1
    while i < len(arr):
        j = i
        while j > 0 and (int(arr[j - 1].get("count"))) < (int(arr[j].get("count"))):
            tmp = arr[j - 1]
            arr[j - 1] = arr[j]
            arr[j] = tmp
            j = j - 1
        i = i + 1
    return arr
def isAlphaNumDot(ch):
    return (ch >= "A" and ch <= "Z") or (ch >= "a" and ch <= "z") or (ch >= "0" and ch <= "9") or ch == "_" or ch == "."
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    srcLines = ["package main", "", "import (", "    \"fmt\"", "    \"go/ast\"", "    \"go/parser\"", "    \"go/token\"", "    \"io/ioutil\"", "    \"os\"", "    \"sort\"", ")", "", "func main() {", "    if len(os.Args) != 2 {", "        fmt.Println(\"usage ff <go source filename>\")", "        return", "    }", "    src, err := ioutil.ReadFile(os.Args[1])", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    fs := token.NewFileSet()", "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    f := fs.File(a.Pos())", "    m := make(map[string]int)", "    ast.Inspect(a, func(n ast.Node) bool {", "        if ce, ok := n.(*ast.CallExpr); ok {", "            start := f.Offset(ce.Pos())", "            end := f.Offset(ce.Lparen)", "            m[string(src[start:end])]++", "        }", "        return true", "    })", "    cs := make(calls, 0, len(m))", "    for k, v := range m {", "        cs = append(cs, &call{k, v})", "    }", "    sort.Sort(cs)", "    for i, c := range cs {", "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)", "        if i == 9 {", "            break", "        }", "    }", "}", "", "type call struct {", "    expr  string", "    count int", "}", "type calls []*call", "", "func (c calls) Len() int           { return len(c) }", "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }", "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"]
    src = join(srcLines, "\n")
    freq = {}
    i = 0
    order = []
    while i < len(src):
        ch = src[i:i + 1]
        if (ch >= "A" and ch <= "Z") or (ch >= "a" and ch <= "z") or ch == "_":
            j = i + 1
            while j < len(src) and isAlphaNumDot(src[j:j + 1]):
                j = j + 1
            token = src[i:j]
            k = j
            while k < len(src):
                cc = src[k:k + 1]
                if cc == " " or cc == "\t" or cc == "\n" or cc == "\r":
                    k = k + 1
                else:
                    break
            if k < len(src) and src[k:k + 1] == "(":
                p = i - 1
                while p >= 0 and (src[p:p + 1] == " " or src[p:p + 1] == "\t"):
                    p = p - 1
                skip = False
                if p >= 3:
                    before = src[p - 3:p + 1]
                    if before == "func":
                        skip = True
                if not skip:
                    if token in freq:
                        freq[token] = freq[token] + 1
                    else:
                        freq[token] = 1
                        order = order + [token]
            i = j
        else:
            i = i + 1
    pairs = []
    for t in order:
        pairs = pairs + [{"expr": t, "count": freq[t]}]
    pairs = sortPairs(pairs)
    idx = 0
    while idx < len(pairs) and idx < 10:
        p = pairs[idx]
        print(p.get("expr") + " " + str(p.get("count")))
        idx = idx + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
