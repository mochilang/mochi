// Generated by Mochi 0.10.52 on 2025-07-31 10:42 +0700
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
    static Dictionary<string, long> removeKey(Dictionary<string, long> m_0, string k_1) {
        Dictionary<string, long> out_2 = new Dictionary<string, long>{};
        foreach (var key_3 in m_0.Keys) {
            if ((key_3 != k_1)) {
                out_2[key_3] = (m_0.ContainsKey(key_3) ? m_0[key_3] : 0);
            }
        };
        return out_2;
    }

    static void main() {
        Dictionary<string, long> x_4 = null;
        x_4 = new Dictionary<string, long>{};
        x_4["foo"] = 3;
        long y1_5 = (x_4.ContainsKey("bar") ? x_4["bar"] : 0);
        bool ok_6 = (x_4.ContainsKey("bar"));
        Console.WriteLine(_fmtTop(y1_5));
        Console.WriteLine((ok_6 ? 1 : 0));
        x_4 = removeKey(x_4, "foo");
        x_4 = new Dictionary<string, long>{{"foo", 2}, {"bar", 42}, {"baz", -1}};
        Console.WriteLine(string.Join(" ", new string[]{_fmtTop((x_4.ContainsKey("foo") ? x_4["foo"] : 0)), _fmtTop((x_4.ContainsKey("bar") ? x_4["bar"] : 0)), _fmtTop((x_4.ContainsKey("baz") ? x_4["baz"] : 0))}));
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
