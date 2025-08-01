// Generated by Mochi 0.10.52 on 2025-07-31 14:19 +0700
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
    static string[][] g_18 = circle(10);
    static string[][] initGrid(long size_0) {
        string[][] g_1 = new string[][]{};
        long y_2 = 0;
        while ((y_2 < size_0)) {
            string[] row_3 = new string[]{};
            long x_4 = 0;
            while ((x_4 < size_0)) {
                row_3 = (Enumerable.ToArray(Enumerable.Append(row_3, " ")));
                x_4 = (x_4 + 1);
            }
            g_1 = (Enumerable.ToArray(Enumerable.Append(g_1, row_3)));
            y_2 = (y_2 + 1);
        };
        return g_1;
    }

    static void set(string[][] g_5, long x_6, long y_7) {
        if (((((x_6 >= 0) && (Convert.ToDouble(x_6) < Convert.ToDouble(g_5[(int)(0)].Length))) && (y_7 >= 0)) && (Convert.ToDouble(y_7) < Convert.ToDouble(g_5.Length)))) {
            g_5[(int)(y_7)][x_6] = "#";
        };
    }

    static string[][] circle(long r_8) {
        long size_9 = ((r_8 * 2) + 1);
        string[][] g_10 = initGrid(size_9);
        long x_11 = r_8;
        long y_12 = 0;
        long err_13 = (1 - r_8);
        while ((y_12 <= x_11)) {
            set(g_10, (r_8 + x_11), (r_8 + y_12));
            set(g_10, (r_8 + y_12), (r_8 + x_11));
            set(g_10, (r_8 - x_11), (r_8 + y_12));
            set(g_10, (r_8 - y_12), (r_8 + x_11));
            set(g_10, (r_8 - x_11), (r_8 - y_12));
            set(g_10, (r_8 - y_12), (r_8 - x_11));
            set(g_10, (r_8 + x_11), (r_8 - y_12));
            set(g_10, (r_8 + y_12), (r_8 - x_11));
            y_12 = (y_12 + 1);
            if ((err_13 < 0)) {
                err_13 = ((err_13 + (2 * y_12)) + 1);
            } else {
                x_11 = (x_11 - 1);
                err_13 = ((err_13 + (2 * (y_12 - x_11))) + 1);
            }
        };
        return g_10;
    }

    static string trimRight(string[] row_14) {
        long end_15 = row_14.Length;
        while (((end_15 > 0) && (row_14[(int)((end_15 - 1))] == " "))) {
            end_15 = (end_15 - 1);
        };
        string s_16 = "";
        long i_17 = 0;
        while ((i_17 < end_15)) {
            s_16 = (s_16 + row_14[(int)(i_17)]);
            i_17 = (i_17 + 1);
        };
        return s_16;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            foreach (var row_19 in g_18) {
                Console.WriteLine(_fmtTop(trimRight(row_19)));
            }
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
