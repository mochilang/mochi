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
    static long indexOf(string s_0, string ch_1) {
        long i_2 = 0;
        while ((i_2 < s_0.Length)) {
            if ((s_0.Substring((int)(i_2), (int)((i_2 + 1) - i_2)) == ch_1)) {
                return i_2;
            }
            i_2 = (i_2 + 1);
        };
        return -1;
    }

    static string fmt1(double x_3) {
        double y_4 = (((dynamic)Convert.ToDouble(Convert.ToInt64(((x_3 * 10) + 0.5)))) / ((dynamic)10));
        string s_5 = (y_4).ToString();
        long dot_6 = indexOf(s_5, ".");
        if ((dot_6 < 0)) {
            s_5 = (s_5 + ".0");
        };
        return s_5;
    }

    static string listToString1(double[] xs_7) {
        string s_8 = "[";
        long i_9 = 0;
        while ((i_9 < xs_7.Length)) {
            s_8 = (s_8 + fmt1(xs_7[(int)(i_9)]));
            if ((i_9 < (((dynamic)xs_7.Length) - ((dynamic)1)))) {
                s_8 = (s_8 + " ");
            }
            i_9 = (i_9 + 1);
        };
        return (s_8 + "]");
    }

    static double[] deconv(double[] g_10, double[] f_11) {
        double[] out_12 = new double[]{};
        long i_13 = 0;
        while ((i_13 <= (((dynamic)g_10.Length) - ((dynamic)f_11.Length)))) {
            double sum_14 = g_10[(int)(i_13)];
            long j_15 = 1;
            while ((j_15 < f_11.Length)) {
                if ((j_15 <= i_13)) {
                    sum_14 = (sum_14 - (out_12[(int)((i_13 - j_15))] * f_11[(int)(j_15)]));
                }
                j_15 = (j_15 + 1);
            }
            out_12 = (Enumerable.ToArray(Enumerable.Append(out_12, (sum_14 / f_11[(int)(0)]))));
            i_13 = (i_13 + 1);
        };
        return out_12;
    }

    static void main() {
        double[] h_16 = new double[]{-8, -9, -3, -1, -6, 7};
        double[] f_17 = new double[]{-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1};
        double[] g_18 = new double[]{24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7};
        Console.WriteLine(_fmtTop(listToString1(h_16)));
        Console.WriteLine(_fmtTop(listToString1(deconv(g_18, f_17))));
        Console.WriteLine(_fmtTop(listToString1(f_17)));
        Console.WriteLine(_fmtTop(listToString1(deconv(g_18, h_16))));
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
