// Generated by Mochi 0.10.40 on 2025-07-26 09:54 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
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
    static double PI_0 = 3.141592653589793;
    static double sinApprox(double x) {
        double term_1 = x;
        double sum_2 = x;
        long n_3 = 1;
        while ((n_3 <= 8)) {
            var denom_4 = Convert.ToDouble(((2 * n_3) * ((2 * n_3) + 1)));
            term_1 = (((dynamic)((-term_1 * x) * x)) / ((dynamic)denom_4));
            sum_2 = (sum_2 + term_1);
            n_3 = (n_3 + 1);
        };
        return sum_2;
    }

    static double cosApprox(double x) {
        double term_5 = 1;
        double sum_6 = 1;
        long n_7 = 1;
        while ((n_7 <= 8)) {
            var denom_8 = Convert.ToDouble((((2 * n_7) - 1) * (2 * n_7)));
            term_5 = (((dynamic)((-term_5 * x) * x)) / ((dynamic)denom_8));
            sum_6 = (sum_6 + term_5);
            n_7 = (n_7 + 1);
        };
        return sum_6;
    }

    static double atanApprox(double x) {
        if ((x > 1)) {
            return ((PI_0 / 2) - (x / ((x * x) + 0.28)));
        };
        if ((x < -1)) {
            return ((-PI_0 / 2) - (x / ((x * x) + 0.28)));
        };
        return (x / (1 + ((0.28 * x) * x)));
    }

    static double atan2Approx(double y, double x) {
        if ((x > 0)) {
            return atanApprox((y / x));
        };
        if ((x < 0)) {
            if ((y >= 0)) {
                return (atanApprox((y / x)) + PI_0);
            }
            return (atanApprox((y / x)) - PI_0);
        };
        if ((y > 0)) {
            return (PI_0 / 2);
        };
        if ((y < 0)) {
            return (-PI_0 / 2);
        };
        return 0;
    }

    static long digit(string ch) {
        string digits_9 = "0123456789";
        long i_10 = 0;
        while ((i_10 < digits_9.Length)) {
            if ((digits_9.Substring((int)(i_10), (int)((i_10 + 1) - i_10)) == ch)) {
                return i_10;
            }
            i_10 = (i_10 + 1);
        };
        return 0;
    }

    static long parseTwo(string s, long idx) {
        return ((digit(s.Substring((int)(idx), (int)((idx + 1) - idx))) * 10) + digit(s.Substring((int)((idx + 1)), (int)((idx + 2) - (idx + 1)))));
    }

    static double parseSec(string s) {
        long h_11 = parseTwo(s, 0);
        long m_12 = parseTwo(s, 3);
        long sec_13 = parseTwo(s, 6);
        long tmp_14 = ((((h_11 * 60) + m_12) * 60) + sec_13);
        return Convert.ToDouble(tmp_14);
    }

    static string pad(long n) {
        if ((n < 10)) {
            return ("0" + (n).ToString());
        };
        return (n).ToString();
    }

    static string meanTime(string[] times) {
        double ssum_15 = 0;
        double csum_16 = 0;
        long i_17 = 0;
        while ((i_17 < times.Length)) {
            double sec_18 = parseSec(times[(int)(i_17)]);
            double ang_19 = (((sec_18 * 2) * PI_0) / 86400);
            ssum_15 = (ssum_15 + sinApprox(ang_19));
            csum_16 = (csum_16 + cosApprox(ang_19));
            i_17 = (i_17 + 1);
        };
        double theta_20 = atan2Approx(ssum_15, csum_16);
        double frac_21 = (theta_20 / (2 * PI_0));
        while ((frac_21 < 0)) {
            frac_21 = (frac_21 + 1);
        };
        double total_22 = (frac_21 * 86400);
        var si_23 = Convert.ToInt64(total_22);
        var h_24 = Convert.ToInt64((((dynamic)si_23) / ((dynamic)3600)));
        var m_25 = Convert.ToInt64(((((dynamic)si_23) % ((dynamic)3600)) / 60));
        var s_26 = Convert.ToInt64((((dynamic)si_23) % ((dynamic)60)));
        return ((((pad(h_24) + ":") + pad(m_25)) + ":") + pad(s_26));
    }

    static void main() {
        string[] inputs_27 = new string[]{"23:00:17", "23:40:20", "00:12:45", "00:17:19"};
        Console.WriteLine(_fmtTop(meanTime(inputs_27)));
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
