// Generated by Mochi 0.10.52 on 2025-07-31 08:37 UTC
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
        if (v is bool b) return b ? "true" : "false";
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
    static string quibble(string[] items_0) {
        var n_1 = items_0.Length;
        if ((n_1 == 0)) {
            return "{}";
        } else {
            if ((n_1 == 1)) {
                return (("{" + items_0[(int)(0)]) + "}");
            } else {
                if ((n_1 == 2)) {
                    return (((("{" + items_0[(int)(0)]) + " and ") + items_0[(int)(1)]) + "}");
                } else {
                    string prefix_2 = "";
                    for (var i_3 = 0; i_3 < (((dynamic)(n_1)) - ((dynamic)(1))); i_3++) {
                        if ((i_3 == (((dynamic)(n_1)) - ((dynamic)(1))))) {
                            break;
                        }
                        if ((i_3 > 0)) {
                            prefix_2 = (prefix_2 + ", ");
                        }
                        prefix_2 = (prefix_2 + items_0[(int)(i_3)]);
                    }
                    return (((("{" + prefix_2) + " and ") + items_0[(int)((((dynamic)(n_1)) - ((dynamic)(1))))]) + "}");
                }
            }
        };
    }

    static void main() {
        Console.WriteLine(_fmtTop(quibble(new string[]{})));
        Console.WriteLine(_fmtTop(quibble(new string[]{"ABC"})));
        Console.WriteLine(_fmtTop(quibble(new string[]{"ABC", "DEF"})));
        Console.WriteLine(_fmtTop(quibble(new string[]{"ABC", "DEF", "G", "H"})));
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
