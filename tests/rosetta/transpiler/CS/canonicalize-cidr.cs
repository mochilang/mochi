// Generated by Mochi 0.10.52 on 2025-07-31 08:37 UTC
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
        if (v is bool b) return b ? "true" : "false";
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
    static string[] tests_38 = new string[]{"87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18"};
    static string[] split(string s_0, string sep_1) {
        string[] parts_2 = new string[]{};
        string cur_3 = "";
        long i_4 = 0;
        while ((Convert.ToDouble(i_4) < Convert.ToDouble(s_0.Length))) {
            if ((((Convert.ToDouble(sep_1.Length) > Convert.ToDouble(0)) && (Convert.ToDouble((((dynamic)(i_4)) + ((dynamic)(sep_1.Length)))) <= Convert.ToDouble(s_0.Length))) && (s_0.Substring((int)(i_4), (int)((((dynamic)(i_4)) + ((dynamic)(sep_1.Length))) - i_4)) == sep_1))) {
                parts_2 = (Enumerable.ToArray(Enumerable.Append(parts_2, cur_3)));
                cur_3 = "";
                i_4 = (((dynamic)(i_4)) + ((dynamic)(sep_1.Length)));
            } else {
                cur_3 = (cur_3 + s_0.Substring((int)(i_4), (int)((i_4 + 1) - i_4)));
                i_4 = (i_4 + 1);
            }
        };
        parts_2 = (Enumerable.ToArray(Enumerable.Append(parts_2, cur_3)));
        return parts_2;
    }

    static string join(string[] xs_5, string sep_6) {
        string res_7 = "";
        long i_8 = 0;
        while ((Convert.ToDouble(i_8) < Convert.ToDouble(xs_5.Length))) {
            if ((i_8 > 0)) {
                res_7 = (res_7 + sep_6);
            }
            res_7 = (res_7 + xs_5[(int)(i_8)]);
            i_8 = (i_8 + 1);
        };
        return res_7;
    }

    static string repeat(string ch_9, long n_10) {
        string out_11 = "";
        long i_12 = 0;
        while ((i_12 < n_10)) {
            out_11 = (out_11 + ch_9);
            i_12 = (i_12 + 1);
        };
        return out_11;
    }

    static long parseIntStr(string str_13) {
        long i_14 = 0;
        bool neg_15 = false;
        if (((Convert.ToDouble(str_13.Length) > Convert.ToDouble(0)) && (str_13.Substring((int)(0), (int)(1 - 0)) == "-"))) {
            neg_15 = true;
            i_14 = 1;
        };
        long n_16 = 0;
        Dictionary<string, long> digits_17 = new Dictionary<string, long>{{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9}};
        while ((Convert.ToDouble(i_14) < Convert.ToDouble(str_13.Length))) {
            n_16 = ((n_16 * 10) + (digits_17.ContainsKey(str_13.Substring((int)(i_14), (int)((i_14 + 1) - i_14))) ? digits_17[str_13.Substring((int)(i_14), (int)((i_14 + 1) - i_14))] : 0));
            i_14 = (i_14 + 1);
        };
        if (neg_15) {
            n_16 = -n_16;
        };
        return n_16;
    }

    static string toBinary(long n_18, long bits_19) {
        string b_20 = "";
        long val_21 = n_18;
        long i_22 = 0;
        while ((i_22 < bits_19)) {
            b_20 = (_fmt((val_21 % 2)) + b_20);
            val_21 = Convert.ToInt64((val_21 / 2));
            i_22 = (i_22 + 1);
        };
        return b_20;
    }

    static long binToInt(string bits_23) {
        long n_24 = 0;
        long i_25 = 0;
        while ((Convert.ToDouble(i_25) < Convert.ToDouble(bits_23.Length))) {
            n_24 = ((n_24 * 2) + parseIntStr(bits_23.Substring((int)(i_25), (int)((i_25 + 1) - i_25))));
            i_25 = (i_25 + 1);
        };
        return n_24;
    }

    static string padRight(string s_26, long width_27) {
        string out_28 = s_26;
        while ((Convert.ToDouble(out_28.Length) < Convert.ToDouble(width_27))) {
            out_28 = (out_28 + " ");
        };
        return out_28;
    }

    static string canonicalize(string cidr_29) {
        string[] parts_30 = split(cidr_29, "/");
        string dotted_31 = parts_30[(int)(0)];
        long size_32 = parseIntStr(parts_30[(int)(1)]);
        string[] binParts_33 = new string[]{};
        foreach (var p_34 in split(dotted_31, ".")) {
            binParts_33 = (Enumerable.ToArray(Enumerable.Append(binParts_33, toBinary(parseIntStr(p_34), 8))));
        };
        string binary_35 = join(binParts_33, "");
        binary_35 = (binary_35.Substring((int)(0), (int)(size_32 - 0)) + repeat("0", (32 - size_32)));
        string[] canonParts_36 = new string[]{};
        long i_37 = 0;
        while ((Convert.ToDouble(i_37) < Convert.ToDouble(binary_35.Length))) {
            canonParts_36 = (Enumerable.ToArray(Enumerable.Append(canonParts_36, _fmt(binToInt(binary_35.Substring((int)(i_37), (int)((i_37 + 8) - i_37)))))));
            i_37 = (i_37 + 8);
        };
        return ((join(canonParts_36, ".") + "/") + parts_30[(int)(1)]);
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            foreach (var t_39 in tests_38) {
                Console.WriteLine(_fmtTop(((padRight(t_39, 18) + " -> ") + canonicalize(t_39))));
            }
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
