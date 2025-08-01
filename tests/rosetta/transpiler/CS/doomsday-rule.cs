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
    static string[] days_5 = new string[]{"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"};
    static long[] firstDaysCommon_8 = new long[]{3, 7, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5};
    static long[] firstDaysLeap_9 = new long[]{4, 1, 7, 4, 2, 6, 4, 1, 5, 3, 7, 5};
    static long parseIntStr(string str_0) {
        long i_1 = 0;
        bool neg_2 = false;
        if (((str_0.Length > 0) && (str_0.Substring((int)(0), (int)(1 - 0)) == "-"))) {
            neg_2 = true;
            i_1 = 1;
        };
        long n_3 = 0;
        Dictionary<string, long> digits_4 = new Dictionary<string, long>{{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9}};
        while ((i_1 < str_0.Length)) {
            n_3 = ((n_3 * 10) + (digits_4.ContainsKey(str_0.Substring((int)(i_1), (int)((i_1 + 1) - i_1))) ? digits_4[str_0.Substring((int)(i_1), (int)((i_1 + 1) - i_1))] : 0));
            i_1 = (i_1 + 1);
        };
        if (neg_2) {
            n_3 = -n_3;
        };
        return n_3;
    }

    static long anchorDay(long y_6) {
        return ((((2 + (5 * (y_6 % 4))) + (4 * (y_6 % 100))) + (6 * (y_6 % 400))) % 7);
    }

    static bool isLeapYear(long y_7) {
        return (((y_7 % 4) == 0) && (((y_7 % 100) != 0) || ((y_7 % 400) == 0)));
    }

    static void main() {
        string[] dates_10 = new string[]{"1800-01-06", "1875-03-29", "1915-12-07", "1970-12-23", "2043-05-14", "2077-02-12", "2101-04-02"};
        Console.WriteLine(_fmtTop("Days of week given by Doomsday rule:"));
        foreach (var date_11 in dates_10) {
            long y_12 = parseIntStr(date_11.Substring((int)(0), (int)(4 - 0)));
            long m_13 = (parseIntStr(date_11.Substring((int)(5), (int)(7 - 5))) - 1);
            long d_14 = parseIntStr(date_11.Substring((int)(8), (int)(10 - 8)));
            long a_15 = anchorDay(y_12);
            long f_16 = firstDaysCommon_8[(int)(m_13)];
            if (isLeapYear(y_12)) {
                f_16 = firstDaysLeap_9[(int)(m_13)];
            }
            long w_17 = (d_14 - f_16);
            if ((w_17 < 0)) {
                w_17 = (7 + w_17);
            }
            long dow_18 = ((a_15 + w_17) % 7);
            Console.WriteLine(_fmtTop(((date_11 + " -> ") + days_5[(int)(dow_18)])));
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
