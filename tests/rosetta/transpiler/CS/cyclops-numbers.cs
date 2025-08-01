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
    static long[] digits(long n_0) {
        if ((n_0 == 0)) {
            return new long[]{0};
        };
        long[] rev_1 = new long[]{};
        long x_2 = n_0;
        while ((x_2 > 0)) {
            rev_1 = (Enumerable.ToArray(Enumerable.Append(rev_1, (x_2 % 10))));
            x_2 = Convert.ToInt64((x_2 / 10));
        };
        long[] out_3 = new long[]{};
        long i_4 = (((dynamic)rev_1.Length) - ((dynamic)1));
        while ((i_4 >= 0)) {
            out_3 = (Enumerable.ToArray(Enumerable.Append(out_3, rev_1[(int)(i_4)])));
            i_4 = (i_4 - 1);
        };
        return out_3;
    }

    static string commatize(long n_5) {
        string s_6 = (n_5).ToString();
        string out_7 = "";
        long i_8 = s_6.Length;
        while ((i_8 > 3)) {
            out_7 = (("," + s_6.Substring((int)((i_8 - 3)), (int)(i_8 - (i_8 - 3)))) + out_7);
            i_8 = (i_8 - 3);
        };
        out_7 = (s_6.Substring((int)(0), (int)(i_8 - 0)) + out_7);
        return out_7;
    }

    static bool isPrime(long n_9) {
        if ((n_9 < 2)) {
            return false;
        };
        if (((n_9 % 2) == 0)) {
            return (n_9 == 2);
        };
        if (((n_9 % 3) == 0)) {
            return (n_9 == 3);
        };
        long d_10 = 5;
        while (((d_10 * d_10) <= n_9)) {
            if (((n_9 % d_10) == 0)) {
                return false;
            }
            d_10 = (d_10 + 2);
            if (((n_9 % d_10) == 0)) {
                return false;
            }
            d_10 = (d_10 + 4);
        };
        return true;
    }

    static string[] split(string s_11, string sep_12) {
        string[] parts_13 = new string[]{};
        string cur_14 = "";
        long i_15 = 0;
        while ((i_15 < s_11.Length)) {
            if ((((((dynamic)i_15) + ((dynamic)sep_12.Length)) <= s_11.Length) && (s_11.Substring((int)(i_15), (int)((((dynamic)i_15) + ((dynamic)sep_12.Length)) - i_15)) == sep_12))) {
                parts_13 = (Enumerable.ToArray(Enumerable.Append(parts_13, cur_14)));
                cur_14 = "";
                i_15 = (((dynamic)i_15) + ((dynamic)sep_12.Length));
            } else {
                cur_14 = (cur_14 + s_11.Substring((int)(i_15), (int)((i_15 + 1) - i_15)));
                i_15 = (i_15 + 1);
            }
        };
        parts_13 = (Enumerable.ToArray(Enumerable.Append(parts_13, cur_14)));
        return parts_13;
    }

    static long parseIntStr(string str_16) {
        long i_17 = 0;
        bool neg_18 = false;
        if (((str_16.Length > 0) && (str_16.Substring((int)(0), (int)(1 - 0)) == "-"))) {
            neg_18 = true;
            i_17 = 1;
        };
        long n_19 = 0;
        Dictionary<string, long> digits_20 = new Dictionary<string, long>{{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9}};
        while ((i_17 < str_16.Length)) {
            n_19 = ((n_19 * 10) + (digits_20.ContainsKey(str_16.Substring((int)(i_17), (int)((i_17 + 1) - i_17))) ? digits_20[str_16.Substring((int)(i_17), (int)((i_17 + 1) - i_17))] : 0));
            i_17 = (i_17 + 1);
        };
        if (neg_18) {
            n_19 = -n_19;
        };
        return n_19;
    }

    static string reverseStr(string s_21) {
        string out_22 = "";
        long i_23 = (((dynamic)s_21.Length) - ((dynamic)1));
        while ((i_23 >= 0)) {
            out_22 = (out_22 + s_21.Substring((int)(i_23), (int)((i_23 + 1) - i_23)));
            i_23 = (i_23 - 1);
        };
        return out_22;
    }

    static string pad(string s_24, long w_25) {
        string out_26 = s_24;
        while ((out_26.Length < w_25)) {
            out_26 = (" " + out_26);
        };
        return out_26;
    }

    static long[] findFirst(long[] list_27) {
        long i_28 = 0;
        while ((i_28 < list_27.Length)) {
            if ((list_27[(int)(i_28)] > 10000000)) {
                return new long[]{list_27[(int)(i_28)], i_28};
            }
            i_28 = (i_28 + 1);
        };
        return new long[]{-1, -1};
    }

    static void main() {
        long[][] ranges_29 = new long[][]{new long[]{0, 0}, new long[]{101, 909}, new long[]{11011, 99099}, new long[]{1110111, 9990999}, new long[]{111101111, 119101111}};
        long[] cyclops_30 = new long[]{};
        foreach (var r_31 in ranges_29) {
            long start_32 = r_31[(int)(0)];
            long end_33 = r_31[(int)(1)];
            var numDigits_34 = (start_32).ToString().Length;
            long center_35 = (((dynamic)numDigits_34) / ((dynamic)2));
            long i_36 = start_32;
            while ((i_36 <= end_33)) {
                long[] ds_37 = digits(i_36);
                if ((ds_37[(int)(center_35)] == 0)) {
                    long count_38 = 0;
                    foreach (var d_39 in ds_37) {
                        if ((d_39 == 0)) {
                            count_38 = (count_38 + 1);
                        }
                    }
                    if ((count_38 == 1)) {
                        cyclops_30 = (Enumerable.ToArray(Enumerable.Append(cyclops_30, i_36)));
                    }
                }
                i_36 = (i_36 + 1);
            }
        };
        Console.WriteLine(_fmtTop("The first 50 cyclops numbers are:"));
        long idx_40 = 0;
        while ((idx_40 < 50)) {
            Console.WriteLine(_fmtTop((pad(commatize(cyclops_30[(int)(idx_40)]), 6) + " ")));
            idx_40 = (idx_40 + 1);
            if (((idx_40 % 10) == 0)) {
                Console.WriteLine(_fmtTop("\n"));
            }
        };
        long[] fi_41 = findFirst(cyclops_30);
        Console.WriteLine(_fmtTop(((("\nFirst such number > 10 million is " + commatize(fi_41[(int)(0)])) + " at zero-based index ") + commatize(fi_41[(int)(1)]))));
        long[] primes_42 = new long[]{};
        foreach (var n_43 in cyclops_30) {
            if (isPrime(n_43)) {
                primes_42 = (Enumerable.ToArray(Enumerable.Append(primes_42, n_43)));
            }
        };
        Console.WriteLine(_fmtTop("\n\nThe first 50 prime cyclops numbers are:"));
        idx_40 = 0;
        while ((idx_40 < 50)) {
            Console.WriteLine(_fmtTop((pad(commatize(primes_42[(int)(idx_40)]), 6) + " ")));
            idx_40 = (idx_40 + 1);
            if (((idx_40 % 10) == 0)) {
                Console.WriteLine(_fmtTop("\n"));
            }
        };
        long[] fp_44 = findFirst(primes_42);
        Console.WriteLine(_fmtTop(((("\nFirst such number > 10 million is " + commatize(fp_44[(int)(0)])) + " at zero-based index ") + commatize(fp_44[(int)(1)]))));
        long[] bpcyclops_45 = new long[]{};
        long[] ppcyclops_46 = new long[]{};
        foreach (var p_47 in primes_42) {
            string ps_48 = (p_47).ToString();
            string[] splitp_49 = split(ps_48, "0");
            long noMiddle_50 = parseIntStr((splitp_49[(int)(0)] + splitp_49[(int)(1)]));
            if (isPrime(noMiddle_50)) {
                bpcyclops_45 = (Enumerable.ToArray(Enumerable.Append(bpcyclops_45, p_47)));
            }
            if ((ps_48 == reverseStr(ps_48))) {
                ppcyclops_46 = (Enumerable.ToArray(Enumerable.Append(ppcyclops_46, p_47)));
            }
        };
        Console.WriteLine(_fmtTop("\n\nThe first 50 blind prime cyclops numbers are:"));
        idx_40 = 0;
        while ((idx_40 < 50)) {
            Console.WriteLine(_fmtTop((pad(commatize(bpcyclops_45[(int)(idx_40)]), 6) + " ")));
            idx_40 = (idx_40 + 1);
            if (((idx_40 % 10) == 0)) {
                Console.WriteLine(_fmtTop("\n"));
            }
        };
        long[] fb_51 = findFirst(bpcyclops_45);
        Console.WriteLine(_fmtTop(((("\nFirst such number > 10 million is " + commatize(fb_51[(int)(0)])) + " at zero-based index ") + commatize(fb_51[(int)(1)]))));
        Console.WriteLine(_fmtTop("\n\nThe first 50 palindromic prime cyclops numbers are:"));
        idx_40 = 0;
        while ((idx_40 < 50)) {
            Console.WriteLine(_fmtTop((pad(commatize(ppcyclops_46[(int)(idx_40)]), 9) + " ")));
            idx_40 = (idx_40 + 1);
            if (((idx_40 % 8) == 0)) {
                Console.WriteLine(_fmtTop("\n"));
            }
        };
        long[] fpp_52 = findFirst(ppcyclops_46);
        Console.WriteLine(_fmtTop(((("\n\nFirst such number > 10 million is " + commatize(fpp_52[(int)(0)])) + " at zero-based index ") + commatize(fpp_52[(int)(1)]))));
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
