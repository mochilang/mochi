// Generated by Mochi 0.10.50 on 2025-07-31 08:23 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;
using math = System.Math;

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
    static double entropy(string s_0) {
        Dictionary<string, long> counts_1 = new Dictionary<string, long>{};
        long i_2 = 0;
        while ((i_2 < s_0.Length)) {
            string ch_3 = s_0.Substring((int)(i_2), (int)((i_2 + 1) - i_2));
            if ((counts_1.ContainsKey(ch_3))) {
                counts_1[ch_3] = ((counts_1.ContainsKey(ch_3) ? counts_1[ch_3] : 0) + 1);
            } else {
                counts_1[ch_3] = 1;
            }
            i_2 = (i_2 + 1);
        };
        double hm_4 = 0;
        foreach (var k_5 in counts_1.Keys) {
            var c_6 = Convert.ToDouble((counts_1.ContainsKey(k_5) ? counts_1[k_5] : 0));
            hm_4 = (hm_4 + (((dynamic)(c_6)) * ((dynamic)((Math.Log(c_6) / Math.Log(2))))));
        };
        var l_7 = Convert.ToDouble(s_0.Length);
        return ((Math.Log(l_7) / Math.Log(2)) - (((dynamic)(hm_4)) / ((dynamic)(l_7))));
    }

    static string fibonacciWord(long n_8) {
        string a_9 = "1";
        string b_10 = "0";
        long i_11 = 1;
        while ((i_11 < n_8)) {
            string tmp_12 = b_10;
            b_10 = (b_10 + a_9);
            a_9 = tmp_12;
            i_11 = (i_11 + 1);
        };
        return a_9;
    }

    static void main() {
        Console.WriteLine(_fmtTop(((pad("N", 3) + pad("Length", 9)) + "  Entropy      Word")));
        long n_13 = 1;
        while ((n_13 < 10)) {
            string s_14 = fibonacciWord(n_13);
            Console.WriteLine(_fmtTop((((((pad((n_13).ToString(), 3) + pad((s_14.Length).ToString(), 9)) + "  ") + fmt(entropy(s_14))) + "  ") + s_14)));
            n_13 = (n_13 + 1);
        };
        while ((n_13 <= 37)) {
            string s_15 = fibonacciWord(n_13);
            Console.WriteLine(_fmtTop((((pad((n_13).ToString(), 3) + pad((s_15.Length).ToString(), 9)) + "  ") + fmt(entropy(s_15)))));
            n_13 = (n_13 + 1);
        };
    }

    static string pad(string s_16, long w_17) {
        string t_18 = s_16;
        while ((t_18.Length < w_17)) {
            t_18 = (" " + t_18);
        };
        return t_18;
    }

    static string fmt(double x_19) {
        double y_20 = (floorf(((x_19 * 1e+08) + 0.5)) / 1e+08);
        string s_21 = (y_20).ToString();
        var dot_22 = s_21.IndexOf(".");
        if ((dot_22 == (0 - 1))) {
            s_21 = (s_21 + ".00000000");
        } else {
            long d_23 = (((dynamic)((((dynamic)(s_21.Length)) - ((dynamic)(dot_22))))) - ((dynamic)(1)));
            while ((d_23 < 8)) {
                s_21 = (s_21 + "0");
                d_23 = (d_23 + 1);
            }
        };
        return s_21;
    }

    static double floorf(double x_24) {
        var y_25 = Convert.ToInt64(x_24);
        return Convert.ToDouble(y_25);
    }

    static long indexOf(string s_26, string ch_27) {
        long i_28 = 0;
        while ((i_28 < s_26.Length)) {
            if ((s_26.Substring((int)(i_28), (int)((i_28 + 1) - i_28)) == ch_27)) {
                return i_28;
            }
            i_28 = (i_28 + 1);
        };
        return (0 - 1);
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
