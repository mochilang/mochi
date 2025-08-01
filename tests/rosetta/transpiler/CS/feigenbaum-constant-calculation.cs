// Generated by Mochi 0.10.50 on 2025-07-31 08:23 +0700
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
    static double floorf(double x_0) {
        var y_1 = Convert.ToInt64(x_0);
        return Convert.ToDouble(y_1);
    }

    static long indexOf(string s_2, string ch_3) {
        long i_4 = 0;
        while ((i_4 < s_2.Length)) {
            if ((s_2.Substring((int)(i_4), (int)((i_4 + 1) - i_4)) == ch_3)) {
                return i_4;
            }
            i_4 = (i_4 + 1);
        };
        return (0 - 1);
    }

    static string fmt8(double x_5) {
        double y_6 = (floorf(((x_5 * 1e+08) + 0.5)) / 1e+08);
        string s_7 = (y_6).ToString();
        long dot_8 = indexOf(s_7, ".");
        if ((dot_8 == (0 - 1))) {
            s_7 = (s_7 + ".00000000");
        } else {
            long decs_9 = ((((dynamic)(s_7.Length)) - ((dynamic)(dot_8))) - 1);
            while ((decs_9 < 8)) {
                s_7 = (s_7 + "0");
                decs_9 = (decs_9 + 1);
            }
        };
        return s_7;
    }

    static string pad2(long x_10) {
        string s_11 = (x_10).ToString();
        if ((s_11.Length < 2)) {
            s_11 = (" " + s_11);
        };
        return s_11;
    }

    static void main() {
        long maxIt_12 = 13;
        long maxItJ_13 = 10;
        double a1_14 = 1;
        double a2_15 = 0;
        double d1_16 = 3.2;
        Console.WriteLine(_fmtTop(" i       d"));
        long i_17 = 2;
        while ((i_17 <= maxIt_12)) {
            double a_18 = (a1_14 + ((a1_14 - a2_15) / d1_16));
            long j_19 = 1;
            while ((j_19 <= maxItJ_13)) {
                double x_20 = 0;
                double y_21 = 0;
                long k_22 = 1;
                long limit_23 = pow_int(2, i_17);
                while ((k_22 <= limit_23)) {
                    y_21 = (1 - ((2 * y_21) * x_20));
                    x_20 = (a_18 - (x_20 * x_20));
                    k_22 = (k_22 + 1);
                }
                a_18 = (a_18 - (x_20 / y_21));
                j_19 = (j_19 + 1);
            }
            double d_24 = ((a1_14 - a2_15) / (a_18 - a1_14));
            Console.WriteLine(_fmtTop(((pad2(i_17) + "    ") + fmt8(d_24))));
            d1_16 = d_24;
            a2_15 = a1_14;
            a1_14 = a_18;
            i_17 = (i_17 + 1);
        };
    }

    static long pow_int(long base_25, long exp_26) {
        long r_27 = 1;
        long b_28 = base_25;
        long e_29 = exp_26;
        while ((e_29 > 0)) {
            if (((e_29 % 2) == 1)) {
                r_27 = (r_27 * b_28);
            }
            b_28 = (b_28 * b_28);
            e_29 = Convert.ToInt64((e_29 / 2));
        };
        return r_27;
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
