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
    static long indexOf3(string s_0, string ch_1, long start_2) {
        long i_3 = start_2;
        while ((i_3 < s_0.Length)) {
            if ((s_0.Substring((int)(i_3), (int)((i_3 + 1) - i_3)) == ch_1)) {
                return i_3;
            }
            i_3 = (i_3 + 1);
        };
        return -1;
    }

    static long ord(string ch_4) {
        string digits_5 = "0123456789";
        long idx_6 = indexOf3(digits_5, ch_4, 0);
        if ((idx_6 >= 0)) {
            return (48 + idx_6);
        };
        if ((ch_4 == "X")) {
            return 88;
        };
        if ((ch_4 == "é")) {
            return 233;
        };
        if ((ch_4 == "😍")) {
            return 128525;
        };
        if ((ch_4 == "🐡")) {
            return 128033;
        };
        return 0;
    }

    static string toHex(long n_7) {
        string digits_8 = "0123456789ABCDEF";
        if ((n_7 == 0)) {
            return "0";
        };
        long v_9 = n_7;
        string out_10 = "";
        while ((v_9 > 0)) {
            long d_11 = (v_9 % 16);
            out_10 = (digits_8.Substring((int)(d_11), (int)((d_11 + 1) - d_11)) + out_10);
            v_9 = (v_9 / 16);
        };
        return out_10;
    }

    static void analyze(string s_12) {
        var le_13 = s_12.Length;
        Console.WriteLine(_fmtTop((((("Analyzing \"" + s_12) + "\" which has a length of ") + (le_13).ToString()) + ":")));
        if ((le_13 > 1)) {
            long i_14 = 0;
            while ((i_14 < (((dynamic)le_13) - ((dynamic)1)))) {
                long j_15 = (i_14 + 1);
                while ((j_15 < le_13)) {
                    if ((s_12.Substring((int)(j_15), (int)((j_15 + 1) - j_15)) == s_12.Substring((int)(i_14), (int)((i_14 + 1) - i_14)))) {
                        string ch_16 = s_12.Substring((int)(i_14), (int)((i_14 + 1) - i_14));
                        Console.WriteLine(_fmtTop("  Not all characters in the string are unique."));
                        Console.WriteLine(_fmtTop((((((((("  '" + ch_16) + "' (0x") + toHex(ord(ch_16)).ToLower()) + ") is duplicated at positions ") + ((i_14 + 1)).ToString()) + " and ") + ((j_15 + 1)).ToString()) + ".\n")));
                        return;
                    }
                    j_15 = (j_15 + 1);
                }
                i_14 = (i_14 + 1);
            }
        };
        Console.WriteLine(_fmtTop("  All characters in the string are unique.\n"));
    }

    static void main() {
        string[] strings_17 = new string[]{"", ".", "abcABC", "XYZ ZYX", "1234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ", "01234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ0X", "hétérogénéité", "🎆🎃🎇🎈", "😍😀🙌💃😍🙌", "🐠🐟🐡🦈🐬🐳🐋🐡"};
        long i_18 = 0;
        while ((i_18 < strings_17.Length)) {
            analyze(strings_17[(int)(i_18)]);
            i_18 = (i_18 + 1);
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
