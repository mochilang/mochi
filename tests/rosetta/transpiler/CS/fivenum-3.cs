// Generated by Mochi 0.10.50 on 2025-07-30 21:05 +0700
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
    static double[] x1_20 = new double[]{36, 40, 7, 39, 41, 15};
    static double[] x2_21 = new double[]{15, 6, 42, 41, 7, 36, 49, 40, 39, 47, 43};
    static double[] x3_22 = new double[]{0.14082834, 0.0974879, 1.73131507, 0.87636009, -1.95059594, 0.73438555, -0.03035726, 1.4667597, -0.74621349, -0.72588772, 0.6390516, 0.61501527, -0.9898378, -1.00447874, -0.62759469, 0.66206163, 1.04312009, -0.10305385, 0.75775634, 0.32566578};
    static double[] sortFloat(double[] xs_0) {
        double[] arr_1 = xs_0;
        var n_2 = arr_1.Length;
        long i_3 = 0;
        while ((i_3 < n_2)) {
            long j_4 = 0;
            while ((j_4 < (((dynamic)n_2) - ((dynamic)1)))) {
                if ((arr_1[(int)(j_4)] > arr_1[(int)((j_4 + 1))])) {
                    double t_5 = arr_1[(int)(j_4)];
                    arr_1[j_4] = arr_1[(int)((j_4 + 1))];
                    arr_1[(j_4 + 1)] = t_5;
                }
                j_4 = (j_4 + 1);
            }
            i_3 = (i_3 + 1);
        };
        return arr_1;
    }

    static long ceilf(double x_6) {
        var i_7 = Convert.ToInt64(x_6);
        if ((x_6 > Convert.ToDouble(i_7))) {
            return (((dynamic)i_7) + ((dynamic)1));
        };
        return i_7;
    }

    static double[] fivenum(double[] a_8) {
        double[] arr_9 = sortFloat(a_8);
        var n_10 = arr_9.Length;
        long half_11 = ((((dynamic)n_10) + ((dynamic)3)) - ((((dynamic)n_10) + ((dynamic)3)) % 2));
        double n4_12 = (((dynamic)Convert.ToDouble((half_11 / 2))) / ((dynamic)2));
        var nf_13 = Convert.ToDouble(n_10);
        double[] d_14 = new double[]{1, n4_12, ((((dynamic)nf_13) + ((dynamic)1)) / 2), ((((dynamic)nf_13) + ((dynamic)1)) - n4_12), nf_13};
        double[] result_15 = new double[]{};
        long idx_16 = 0;
        while ((idx_16 < d_14.Length)) {
            double de_17 = d_14[(int)(idx_16)];
            var fl_18 = Convert.ToInt64((de_17 - 1));
            long cl_19 = ceilf((de_17 - 1));
            result_15 = (Enumerable.ToArray(Enumerable.Append(result_15, (0.5 * (arr_9[(int)(fl_18)] + arr_9[(int)(cl_19)])))));
            idx_16 = (idx_16 + 1);
        };
        return result_15;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            Console.WriteLine(_fmtTop((fivenum(x1_20)).ToString()));
            Console.WriteLine(_fmtTop((fivenum(x2_21)).ToString()));
            Console.WriteLine(_fmtTop((fivenum(x3_22)).ToString()));
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
