// Generated by Mochi 0.10.52 on 2025-07-31 09:21 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class Program {
    static bool seededNow = false;
    static long nowSeed = 0;
    static long _now() {
        if (!seededNow) {
            var s = Environment.GetEnvironmentVariable("MOCHI_NOW_SEED");
            if (long.TryParse(s, out var v)) {
                nowSeed = v;
                seededNow = true;
            }
        }
        if (seededNow) {
            nowSeed = unchecked(nowSeed * 1664525 + 1013904223);
            nowSeed %= 9223372036854775783L;
            if (nowSeed < 0) nowSeed += 9223372036854775783L;
            return nowSeed;
        }
        return DateTime.UtcNow.Ticks / 100;
    }
    static long _mem() {
        return GC.GetTotalMemory(false);
    }
    static string _fmt(object v) {
        if (v is Array a) {
            var parts = new List<string>();
            foreach (var x in a) parts.Add(_fmt(x));
            return "[" + string.Join(" ", parts) + "]";
        }
        if (v is System.Collections.IDictionary d) {
            var keys = new List<string>();
            foreach (var k in d.Keys) keys.Add(k.ToString());
            keys.Sort();
            var parts = new List<string>();
            foreach (var k in keys) parts.Add(k + ":" + _fmt(d[k]));
            return "map[" + string.Join(" ", parts) + "]";
        }
        if (v is System.Collections.IEnumerable e && !(v is string)) {
            var parts = new List<string>();
            foreach (var x in e) parts.Add(_fmt(x));
            return string.Join(" ", parts);
        }
        if (v is bool b) return b ? "1" : "0";
        return Convert.ToString(v);
    }
    static string _fmtTop(object v) {
        if (v is Array a && a.Length > 0 && a.GetValue(0) is Array) {
            var parts = new List<string>();
            foreach (var x in a) parts.Add(_fmt(x));
            return string.Join(" ", parts);
        }
        return _fmt(v);
    }
    static string join(string[] xs_0, string sep_1) {
        string res_2 = "";
        long i_3 = 0;
        while ((i_3 < xs_0.Length)) {
            if ((i_3 > 0)) {
                res_2 = (res_2 + sep_1);
            }
            res_2 = (res_2 + xs_0[(int)(i_3)]);
            i_3 = (i_3 + 1);
        };
        return res_2;
    }

    static Dictionary<string, object>[] sortPairs(Dictionary<string, object>[] xs_4) {
        Dictionary<string, object>[] arr_5 = xs_4;
        long i_6 = 1;
        while ((i_6 < arr_5.Length)) {
            long j_7 = i_6;
            while (((j_7 > 0) && (Convert.ToInt64(((dynamic)arr_5[(int)((j_7 - 1))])["count"]) < Convert.ToInt64(((dynamic)arr_5[(int)(j_7)])["count"])))) {
                Dictionary<string, object> tmp_8 = arr_5[(int)((j_7 - 1))];
                arr_5[(j_7 - 1)] = arr_5[(int)(j_7)];
                arr_5[j_7] = tmp_8;
                j_7 = (j_7 - 1);
            }
            i_6 = (i_6 + 1);
        };
        return arr_5;
    }

    static bool isAlphaNumDot(string ch_9) {
        return ((((((string.Compare(ch_9, "A") >= 0) && (string.Compare(ch_9, "Z") <= 0)) || ((string.Compare(ch_9, "a") >= 0) && (string.Compare(ch_9, "z") <= 0))) || ((string.Compare(ch_9, "0") >= 0) && (string.Compare(ch_9, "9") <= 0))) || (ch_9 == "_")) || (ch_9 == "."));
    }

    static void main() {
        string[] srcLines_10 = new string[]{"package main", "", "import (", "    \"fmt\"", "    \"go/ast\"", "    \"go/parser\"", "    \"go/token\"", "    \"io/ioutil\"", "    \"os\"", "    \"sort\"", ")", "", "func main() {", "    if len(os.Args) != 2 {", "        fmt.Println(\"usage ff <go source filename>\")", "        return", "    }", "    src, err := ioutil.ReadFile(os.Args[1])", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    fs := token.NewFileSet()", "    a, err := parser.ParseFile(fs, os.Args[1], src, 0)", "    if err != nil {", "        fmt.Println(err)", "        return", "    }", "    f := fs.File(a.Pos())", "    m := make(map[string]int)", "    ast.Inspect(a, func(n ast.Node) bool {", "        if ce, ok := n.(*ast.CallExpr); ok {", "            start := f.Offset(ce.Pos())", "            end := f.Offset(ce.Lparen)", "            m[string(src[start:end])]++", "        }", "        return true", "    })", "    cs := make(calls, 0, len(m))", "    for k, v := range m {", "        cs = append(cs, &call{k, v})", "    }", "    sort.Sort(cs)", "    for i, c := range cs {", "        fmt.Printf(\"%-20s %4d\\n\", c.expr, c.count)", "        if i == 9 {", "            break", "        }", "    }", "}", "", "type call struct {", "    expr  string", "    count int", "}", "type calls []*call", "", "func (c calls) Len() int           { return len(c) }", "func (c calls) Swap(i, j int)      { c[i], c[j] = c[j], c[i] }", "func (c calls) Less(i, j int) bool { return c[i].count > c[j].count }"};
        string src_11 = join(srcLines_10, "\n");
        Dictionary<string, long> freq_12 = new Dictionary<string, long>{};
        long i_13 = 0;
        string[] order_14 = new string[]{};
        while ((i_13 < src_11.Length)) {
            string ch_15 = src_11.Substring((int)(i_13), (int)((i_13 + 1) - i_13));
            if (((((string.Compare(ch_15, "A") >= 0) && (string.Compare(ch_15, "Z") <= 0)) || ((string.Compare(ch_15, "a") >= 0) && (string.Compare(ch_15, "z") <= 0))) || (ch_15 == "_"))) {
                long j_16 = (i_13 + 1);
                while (((j_16 < src_11.Length) && isAlphaNumDot(src_11.Substring((int)(j_16), (int)((j_16 + 1) - j_16))))) {
                    j_16 = (j_16 + 1);
                }
                string token_17 = src_11.Substring((int)(i_13), (int)(j_16 - i_13));
                long k_18 = j_16;
                while ((k_18 < src_11.Length)) {
                    string cc_19 = src_11.Substring((int)(k_18), (int)((k_18 + 1) - k_18));
                    if (((((cc_19 == " ") || (cc_19 == "\t")) || (cc_19 == "\n")) || (cc_19 == "\r"))) {
                        k_18 = (k_18 + 1);
                    } else {
                        break;
                    }
                }
                if (((k_18 < src_11.Length) && (src_11.Substring((int)(k_18), (int)((k_18 + 1) - k_18)) == "("))) {
                    long p_20 = (i_13 - 1);
                    while (((p_20 >= 0) && ((src_11.Substring((int)(p_20), (int)((p_20 + 1) - p_20)) == " ") || (src_11.Substring((int)(p_20), (int)((p_20 + 1) - p_20)) == "\t")))) {
                        p_20 = (p_20 - 1);
                    }
                    bool skip_21 = false;
                    if ((p_20 >= 3)) {
                        string before_22 = src_11.Substring((int)((p_20 - 3)), (int)((p_20 + 1) - (p_20 - 3)));
                        if ((before_22 == "func")) {
                            skip_21 = true;
                        }
                    }
                    if ((!skip_21)) {
                        if ((freq_12.ContainsKey(token_17))) {
                            freq_12[token_17] = ((freq_12.ContainsKey(token_17) ? freq_12[token_17] : 0) + 1);
                        } else {
                            freq_12[token_17] = 1;
                            order_14 = (Enumerable.ToArray(Enumerable.Append(order_14, token_17)));
                        }
                    }
                }
                i_13 = j_16;
            } else {
                i_13 = (i_13 + 1);
            }
        };
        Dictionary<string, object>[] pairs_23 = new Dictionary<string, object>[]{};
        foreach (var t_24 in order_14) {
            pairs_23 = (Enumerable.ToArray(Enumerable.Append(pairs_23, new Dictionary<string, object>{{"expr", t_24}, {"count", (freq_12.ContainsKey(t_24) ? freq_12[t_24] : 0)}})));
        };
        pairs_23 = sortPairs(pairs_23);
        long idx_25 = 0;
        while (((idx_25 < pairs_23.Length) && (idx_25 < 10))) {
            Dictionary<string, object> p_26 = pairs_23[(int)(idx_25)];
            Console.WriteLine(_fmtTop(((((dynamic)(((dynamic)p_26)["expr"])) + ((dynamic)(" "))) + (((dynamic)p_26)["count"]).ToString())));
            idx_25 = (idx_25 + 1);
        };
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            main();
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
