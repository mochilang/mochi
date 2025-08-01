// Generated by Mochi 0.10.50 on 2025-07-31 07:41 +0700
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
    static string commatize(long n_0) {
        string s_1 = (n_0).ToString();
        long i_2 = (((dynamic)s_1.Length) - ((dynamic)3));
        while ((i_2 >= 1)) {
            s_1 = ((s_1.Substring((int)(0), (int)(i_2 - 0)) + ",") + s_1.Substring((int)(i_2), (int)(s_1.Length - i_2)));
            i_2 = (i_2 - 3);
        };
        return s_1;
    }

    static bool[] primeSieve(long n_3) {
        bool[] sieve_4 = new bool[]{};
        long i_5 = 0;
        while ((i_5 <= n_3)) {
            sieve_4 = (Enumerable.ToArray(Enumerable.Append(sieve_4, false)));
            i_5 = (i_5 + 1);
        };
        sieve_4[0] = true;
        sieve_4[1] = true;
        long p_6 = 2;
        while (((p_6 * p_6) <= n_3)) {
            if ((!sieve_4[(int)(p_6)])) {
                long m_7 = (p_6 * p_6);
                while ((m_7 <= n_3)) {
                    sieve_4[m_7] = true;
                    m_7 = (m_7 + p_6);
                }
            }
            p_6 = (p_6 + 1);
        };
        return sieve_4;
    }

    static long search(long[] xs_8, long target_9) {
        long low_10 = 0;
        long high_11 = xs_8.Length;
        while ((low_10 < high_11)) {
            long mid_12 = ((low_10 + high_11) / 2);
            if ((xs_8[(int)(mid_12)] < target_9)) {
                low_10 = (mid_12 + 1);
            } else {
                high_11 = mid_12;
            }
        };
        return low_10;
    }

    static void main() {
        long limit_13 = 45000;
        bool[] compMap_14 = primeSieve(limit_13);
        long[] compSums_15 = new long[]{};
        long[] primeSums_16 = new long[]{};
        long csum_17 = 0;
        long psum_18 = 0;
        long i_19 = 2;
        while ((i_19 <= limit_13)) {
            if (compMap_14[(int)(i_19)]) {
                csum_17 = (csum_17 + i_19);
                compSums_15 = (Enumerable.ToArray(Enumerable.Append(compSums_15, csum_17)));
            } else {
                psum_18 = (psum_18 + i_19);
                primeSums_16 = (Enumerable.ToArray(Enumerable.Append(primeSums_16, psum_18)));
            }
            i_19 = (i_19 + 1);
        };
        Console.WriteLine(_fmtTop("Sum        | Prime Index | Composite Index"));
        Console.WriteLine(_fmtTop("------------------------------------------"));
        long idx_20 = 0;
        while ((idx_20 < primeSums_16.Length)) {
            long s_21 = primeSums_16[(int)(idx_20)];
            long j_22 = search(compSums_15, s_21);
            if (((j_22 < compSums_15.Length) && (compSums_15[(int)(j_22)] == s_21))) {
                var sumStr_23 = commatize(s_21).PadLeft(10, ' ');
                var piStr_24 = commatize((idx_20 + 1)).PadLeft(11, ' ');
                var ciStr_25 = commatize((j_22 + 1)).PadLeft(15, ' ');
                Console.WriteLine(_fmtTop((((dynamic)((((dynamic)(((dynamic)sumStr_23) + ((dynamic)" | "))) + ((dynamic)piStr_24)) + " | ")) + ((dynamic)ciStr_25))));
            }
            idx_20 = (idx_20 + 1);
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
