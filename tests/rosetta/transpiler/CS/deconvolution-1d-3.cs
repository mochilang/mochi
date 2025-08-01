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

    static void printColumnMatrix(double[] xs_7) {
        if ((xs_7.Length == 0)) {
            return;
        };
        Console.WriteLine(_fmtTop((("⎡" + fmt1(xs_7[(int)(0)])) + "⎤")));
        long i_8 = 1;
        while ((i_8 < (((dynamic)xs_7.Length) - ((dynamic)1)))) {
            Console.WriteLine(_fmtTop((("⎢" + fmt1(xs_7[(int)(i_8)])) + "⎥")));
            i_8 = (i_8 + 1);
        };
        Console.WriteLine(_fmtTop((("⎣ " + fmt1(xs_7[(int)((((dynamic)xs_7.Length) - ((dynamic)1)))])) + "⎦")));
    }

    static double[] deconv(double[] g_9, double[] f_10) {
        double[] h_11 = new double[]{};
        long n_12 = 0;
        long hn_13 = (((dynamic)(((dynamic)g_9.Length) - ((dynamic)f_10.Length))) + ((dynamic)1));
        while ((n_12 < hn_13)) {
            double v_14 = g_9[(int)(n_12)];
            long lower_15 = 0;
            if ((n_12 >= f_10.Length)) {
                lower_15 = ((((dynamic)n_12) - ((dynamic)f_10.Length)) + 1);
            }
            long i_16 = lower_15;
            while ((i_16 < n_12)) {
                v_14 = (v_14 - (h_11[(int)(i_16)] * f_10[(int)((n_12 - i_16))]));
                i_16 = (i_16 + 1);
            }
            v_14 = (v_14 / f_10[(int)(0)]);
            h_11 = (Enumerable.ToArray(Enumerable.Append(h_11, v_14)));
            n_12 = (n_12 + 1);
        };
        return h_11;
    }

    static void main() {
        double[] h_17 = new double[]{-8, -9, -3, -1, -6, 7};
        double[] f_18 = new double[]{-3, -6, -1, 8, -6, 3, -1, -9, -9, 3, -2, 5, 2, -2, -7, -1};
        double[] g_19 = new double[]{24, 75, 71, -34, 3, 22, -45, 23, 245, 25, 52, 25, -67, -96, 96, 31, 55, 36, 29, -43, -7};
        Console.WriteLine(_fmtTop("deconv(g, f) ="));
        printColumnMatrix(deconv(g_19, f_18));
        Console.WriteLine(_fmtTop(""));
        Console.WriteLine(_fmtTop("deconv(g, h) ="));
        printColumnMatrix(deconv(g_19, h_17));
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
