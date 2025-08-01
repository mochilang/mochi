// Generated by Mochi 0.10.41 on 2025-07-27 07:48 UTC
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
    static long LIMIT_7 = 999999;
    static long[] primes_8 = primesUpTo(LIMIT_7);
    static long[] primesUpTo(long n_0) {
        bool[] sieve_1 = new bool[]{};
        long i_2 = 0;
        while ((i_2 <= n_0)) {
            sieve_1 = (Enumerable.ToArray(Enumerable.Append(sieve_1, true)));
            i_2 = (i_2 + 1);
        };
        long p_3 = 2;
        while (((p_3 * p_3) <= n_0)) {
            if (sieve_1[(int)(p_3)]) {
                long m_4 = (p_3 * p_3);
                while ((m_4 <= n_0)) {
                    sieve_1[m_4] = false;
                    m_4 = (m_4 + p_3);
                }
            }
            p_3 = (p_3 + 1);
        };
        long[] res_5 = new long[]{};
        long x_6 = 2;
        while ((x_6 <= n_0)) {
            if (sieve_1[(int)(x_6)]) {
                res_5 = (Enumerable.ToArray(Enumerable.Append(res_5, x_6)));
            }
            x_6 = (x_6 + 1);
        };
        return res_5;
    }

    static void longestSeq(string dir_9) {
        long pd_10 = 0;
        long[][] longSeqs_11 = new long[][]{new long[]{2}};
        long[] currSeq_12 = new long[]{2};
        long i_13 = 1;
        while ((i_13 < primes_8.Length)) {
            long d_14 = (primes_8[(int)(i_13)] - primes_8[(int)((i_13 - 1))]);
            if ((((dir_9 == "ascending") && (d_14 <= pd_10)) || ((dir_9 == "descending") && (d_14 >= pd_10)))) {
                if ((currSeq_12.Length > longSeqs_11[(int)(0)].Length)) {
                    longSeqs_11 = new long[][]{currSeq_12};
                } else {
                    if ((currSeq_12.Length == longSeqs_11[(int)(0)].Length)) {
                        longSeqs_11 = (Enumerable.ToArray(Enumerable.Append(longSeqs_11, currSeq_12)));
                    }
                }
                currSeq_12 = new long[]{primes_8[(int)((i_13 - 1))], primes_8[(int)(i_13)]};
            } else {
                currSeq_12 = (Enumerable.ToArray(Enumerable.Append(currSeq_12, primes_8[(int)(i_13)])));
            }
            pd_10 = d_14;
            i_13 = (i_13 + 1);
        };
        if ((currSeq_12.Length > longSeqs_11[(int)(0)].Length)) {
            longSeqs_11 = new long[][]{currSeq_12};
        } else {
            if ((currSeq_12.Length == longSeqs_11[(int)(0)].Length)) {
                longSeqs_11 = (Enumerable.ToArray(Enumerable.Append(longSeqs_11, currSeq_12)));
            }
        };
        Console.WriteLine(_fmtTop((((("Longest run(s) of primes with " + dir_9) + " differences is ") + (longSeqs_11[(int)(0)].Length).ToString()) + " :")));
        foreach (var ls_15 in longSeqs_11) {
            long[] diffs_16 = new long[]{};
            long j_17 = 1;
            while ((j_17 < ls_15.Length)) {
                diffs_16 = (Enumerable.ToArray(Enumerable.Append(diffs_16, (ls_15[(int)(j_17)] - ls_15[(int)((j_17 - 1))]))));
                j_17 = (j_17 + 1);
            }
            long k_18 = 0;
            while ((k_18 < (((dynamic)ls_15.Length) - ((dynamic)1)))) {
                Console.WriteLine(string.Join(" ", new string[]{_fmtTop(((((ls_15[(int)(k_18)]).ToString() + " (") + (diffs_16[(int)(k_18)]).ToString()) + ") ")), _fmtTop(false)}));
                k_18 = (k_18 + 1);
            }
            Console.WriteLine(_fmtTop((ls_15[(int)((((dynamic)ls_15.Length) - ((dynamic)1)))]).ToString()));
        };
        Console.WriteLine(_fmtTop(""));
    }

    static void main() {
        Console.WriteLine(_fmtTop("For primes < 1 million:\n"));
        foreach (var dir_19 in new string[]{"ascending", "descending"}) {
            longestSeq(dir_19);
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
