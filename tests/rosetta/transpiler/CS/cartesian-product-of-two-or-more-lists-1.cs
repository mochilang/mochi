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
    static long[][] cart2(long[] a_0, long[] b_1) {
        long[][] p_2 = new long[][]{};
        foreach (var x_3 in a_0) {
            foreach (var y_4 in b_1) {
                p_2 = (Enumerable.ToArray(Enumerable.Append(p_2, new long[]{x_3, y_4})));
            }
        };
        return p_2;
    }

    static string llStr(long[][] lst_5) {
        string s_6 = "[";
        long i_7 = 0;
        while ((Convert.ToDouble(i_7) < Convert.ToDouble(lst_5.Length))) {
            long[] row_8 = lst_5[(int)(i_7)];
            s_6 = (s_6 + "[");
            long j_9 = 0;
            while ((Convert.ToDouble(j_9) < Convert.ToDouble(row_8.Length))) {
                s_6 = (s_6 + _fmt(row_8[(int)(j_9)]));
                if ((j_9 < (((dynamic)(row_8.Length)) - ((dynamic)(1))))) {
                    s_6 = (s_6 + " ");
                }
                j_9 = (j_9 + 1);
            }
            s_6 = (s_6 + "]");
            if ((i_7 < (((dynamic)(lst_5.Length)) - ((dynamic)(1))))) {
                s_6 = (s_6 + " ");
            }
            i_7 = (i_7 + 1);
        };
        s_6 = (s_6 + "]");
        return s_6;
    }

    static void main() {
        Console.WriteLine(_fmtTop(llStr(cart2(new long[]{1, 2}, new long[]{3, 4}))));
        Console.WriteLine(_fmtTop(llStr(cart2(new long[]{3, 4}, new long[]{1, 2}))));
        Console.WriteLine(_fmtTop(llStr(cart2(new long[]{1, 2}, new long[]{}))));
        Console.WriteLine(_fmtTop(llStr(cart2(new long[]{}, new long[]{1, 2}))));
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
