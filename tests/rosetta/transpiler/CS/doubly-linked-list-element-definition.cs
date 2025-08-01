// Generated by Mochi 0.10.50 on 2025-07-31 00:08 +0700
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
    static Dictionary<string, object> Node(string value_0, object next_1, object prev_2) {
        return new Dictionary<string, object>{{"value", value_0}, {"next", next_1}, {"prev", prev_2}};
    }

    static void main() {
        Dictionary<string, object> a_3 = Node("A", null, null);
        Dictionary<string, object> b_4 = Node("B", null, a_3);
        a_3["next"] = b_4;
        Dictionary<string, object> c_5 = Node("C", null, b_4);
        b_4["next"] = c_5;
        Dictionary<string, object> p_6 = a_3;
        string line_7 = "";
        while ((p_6 != null)) {
            line_7 = (((dynamic)line_7) + ((dynamic)((dynamic)p_6)["value"]));
            p_6 = ((dynamic)p_6)["next"];
            if ((p_6 != null)) {
                line_7 = (line_7 + " ");
            }
        };
        Console.WriteLine(_fmtTop(line_7));
        p_6 = c_5;
        line_7 = "";
        while ((p_6 != null)) {
            line_7 = (((dynamic)line_7) + ((dynamic)((dynamic)p_6)["value"]));
            p_6 = ((dynamic)p_6)["prev"];
            if ((p_6 != null)) {
                line_7 = (line_7 + " ");
            }
        };
        Console.WriteLine(_fmtTop(line_7));
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
