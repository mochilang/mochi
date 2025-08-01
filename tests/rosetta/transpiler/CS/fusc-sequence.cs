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
    static long fuscVal(long n_0) {
        long a_1 = 1;
        long b_2 = 0;
        long x_3 = n_0;
        while ((x_3 > 0)) {
            if (((x_3 % 2) == 0)) {
                x_3 = (x_3 / 2);
                a_1 = (a_1 + b_2);
            } else {
                x_3 = ((x_3 - 1) / 2);
                b_2 = (a_1 + b_2);
            }
        };
        if ((n_0 == 0)) {
            return 0;
        };
        return b_2;
    }

    static long[] firstFusc(long n_4) {
        long[] arr_5 = new long[]{};
        long i_6 = 0;
        while ((i_6 < n_4)) {
            arr_5 = (Enumerable.ToArray(Enumerable.Append(arr_5, fuscVal(i_6))));
            i_6 = (i_6 + 1);
        };
        return arr_5;
    }

    static string commatize(long n_7) {
        string s_8 = (n_7).ToString();
        bool neg_9 = false;
        if ((n_7 < 0)) {
            neg_9 = true;
            s_8 = s_8.Substring((int)(1), (int)(s_8.Length - 1));
        };
        long i_10 = (((dynamic)(s_8.Length)) - ((dynamic)(3)));
        while ((i_10 >= 1)) {
            s_8 = ((s_8.Substring((int)(0), (int)(i_10 - 0)) + ",") + s_8.Substring((int)(i_10), (int)(s_8.Length - i_10)));
            i_10 = (i_10 - 3);
        };
        if (neg_9) {
            return ("-" + s_8);
        };
        return s_8;
    }

    static string padLeft(string s_11, long w_12) {
        string out_13 = s_11;
        while ((out_13.Length < w_12)) {
            out_13 = (" " + out_13);
        };
        return out_13;
    }

    static void main() {
        Console.WriteLine(_fmtTop("The first 61 fusc numbers are:"));
        Console.WriteLine(_fmtTop((firstFusc(61)).ToString()));
        Console.WriteLine(_fmtTop("\nThe fusc numbers whose length > any previous fusc number length are:"));
        long[] idxs_14 = new long[]{0, 37, 1173, 35499, 699051, 19573419};
        long i_15 = 0;
        while ((i_15 < idxs_14.Length)) {
            long idx_16 = idxs_14[(int)(i_15)];
            long val_17 = fuscVal(idx_16);
            string numStr_18 = padLeft(commatize(val_17), 7);
            string idxStr_19 = padLeft(commatize(idx_16), 10);
            Console.WriteLine(_fmtTop((((numStr_18 + " (index ") + idxStr_19) + ")")));
            i_15 = (i_15 + 1);
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
