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
    static double pow10(long n_0) {
        double r_1 = 1;
        long i_2 = 0;
        while ((i_2 < n_0)) {
            r_1 = (r_1 * 10);
            i_2 = (i_2 + 1);
        };
        return r_1;
    }

    static string formatFloat(double f_3, long prec_4) {
        double scale_5 = pow10(prec_4);
        double scaled_6 = ((f_3 * scale_5) + 0.5);
        var n_7 = Convert.ToInt64(scaled_6);
        string digits_8 = (n_7).ToString();
        while ((digits_8.Length <= prec_4)) {
            digits_8 = ("0" + digits_8);
        };
        string intPart_9 = digits_8.Substring((int)(0), (int)((((dynamic)digits_8.Length) - ((dynamic)prec_4)) - 0));
        string fracPart_10 = digits_8.Substring((int)((((dynamic)digits_8.Length) - ((dynamic)prec_4))), (int)(digits_8.Length - (((dynamic)digits_8.Length) - ((dynamic)prec_4))));
        return ((intPart_9 + ".") + fracPart_10);
    }

    static string padLeft(string s_11, long w_12) {
        string res_13 = "";
        long n_14 = (((dynamic)w_12) - ((dynamic)s_11.Length));
        while ((n_14 > 0)) {
            res_13 = (res_13 + " ");
            n_14 = (n_14 - 1);
        };
        return (res_13 + s_11);
    }

    static double averageSquareDiff(double f_15, double[] preds_16) {
        double av_17 = 0;
        long i_18 = 0;
        while ((i_18 < preds_16.Length)) {
            av_17 = (av_17 + ((preds_16[(int)(i_18)] - f_15) * (preds_16[(int)(i_18)] - f_15)));
            i_18 = (i_18 + 1);
        };
        av_17 = (((dynamic)av_17) / ((dynamic)Convert.ToDouble(preds_16.Length)));
        return av_17;
    }

    static double[] diversityTheorem(double truth_19, double[] preds_20) {
        double av_21 = 0;
        long i_22 = 0;
        while ((i_22 < preds_20.Length)) {
            av_21 = (av_21 + preds_20[(int)(i_22)]);
            i_22 = (i_22 + 1);
        };
        av_21 = (((dynamic)av_21) / ((dynamic)Convert.ToDouble(preds_20.Length)));
        double avErr_23 = averageSquareDiff(truth_19, preds_20);
        double crowdErr_24 = ((truth_19 - av_21) * (truth_19 - av_21));
        double div_25 = averageSquareDiff(av_21, preds_20);
        return new double[]{avErr_23, crowdErr_24, div_25};
    }

    static void main() {
        double[][] predsArray_26 = new double[][]{new double[]{48, 47, 51}, new double[]{48, 47, 51, 42}};
        double truth_27 = 49;
        long i_28 = 0;
        while ((i_28 < predsArray_26.Length)) {
            double[] preds_29 = predsArray_26[(int)(i_28)];
            double[] res_30 = diversityTheorem(truth_27, preds_29);
            Console.WriteLine(_fmtTop(("Average-error : " + padLeft(formatFloat(res_30[(int)(0)], 3), 6))));
            Console.WriteLine(_fmtTop(("Crowd-error   : " + padLeft(formatFloat(res_30[(int)(1)], 3), 6))));
            Console.WriteLine(_fmtTop(("Diversity     : " + padLeft(formatFloat(res_30[(int)(2)], 3), 6))));
            Console.WriteLine(_fmtTop(""));
            i_28 = (i_28 + 1);
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
