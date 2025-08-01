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
    static void floyd(long n_0) {
        Console.WriteLine(_fmtTop((("Floyd " + (n_0).ToString()) + ":")));
        long lowerLeftCorner_1 = (((n_0 * (n_0 - 1)) / 2) + 1);
        long lastInColumn_2 = lowerLeftCorner_1;
        long lastInRow_3 = 1;
        long i_4 = 1;
        long row_5 = 1;
        string line_6 = "";
        while ((row_5 <= n_0)) {
            var w_7 = (lastInColumn_2).ToString().Length;
            if ((i_4 < lastInRow_3)) {
                line_6 = ((line_6 + pad((i_4).ToString(), w_7)) + " ");
                lastInColumn_2 = (lastInColumn_2 + 1);
            } else {
                line_6 = (line_6 + pad((i_4).ToString(), w_7));
                Console.WriteLine(_fmtTop(line_6));
                line_6 = "";
                row_5 = (row_5 + 1);
                lastInRow_3 = (lastInRow_3 + row_5);
                lastInColumn_2 = lowerLeftCorner_1;
            }
            i_4 = (i_4 + 1);
        };
    }

    static string pad(string s_8, long w_9) {
        string t_10 = s_8;
        while ((t_10.Length < w_9)) {
            t_10 = (" " + t_10);
        };
        return t_10;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            floyd(5);
            floyd(14);
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
