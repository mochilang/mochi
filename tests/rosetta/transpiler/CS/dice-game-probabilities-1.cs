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
    static long powInt(long base_0, long exp_1) {
        long r_2 = 1;
        long b_3 = base_0;
        long e_4 = exp_1;
        while ((e_4 > 0)) {
            if (((e_4 % 2) == 1)) {
                r_2 = (r_2 * b_3);
            }
            b_3 = (b_3 * b_3);
            e_4 = (((dynamic)e_4) / ((dynamic)Convert.ToInt64(2)));
        };
        return r_2;
    }

    static long minInt(long x_5, long y_6) {
        if ((x_5 < y_6)) {
            return x_5;
        };
        return y_6;
    }

    static void throwDie(long nSides_7, long nDice_8, long s_9, long[] counts_10) {
        if ((nDice_8 == 0)) {
            counts_10[s_9] = (counts_10[(int)(s_9)] + 1);
            return;
        };
        long i_11 = 1;
        while ((i_11 <= nSides_7)) {
            throwDie(nSides_7, (nDice_8 - 1), (s_9 + i_11), counts_10);
            i_11 = (i_11 + 1);
        };
    }

    static double beatingProbability(long nSides1_12, long nDice1_13, long nSides2_14, long nDice2_15) {
        long len1_16 = ((nSides1_12 + 1) * nDice1_13);
        long[] c1_17 = new long[]{};
        long i_18 = 0;
        while ((i_18 < len1_16)) {
            c1_17 = (Enumerable.ToArray(Enumerable.Append(c1_17, 0)));
            i_18 = (i_18 + 1);
        };
        throwDie(nSides1_12, nDice1_13, 0, c1_17);
        long len2_19 = ((nSides2_14 + 1) * nDice2_15);
        long[] c2_20 = new long[]{};
        long j_21 = 0;
        while ((j_21 < len2_19)) {
            c2_20 = (Enumerable.ToArray(Enumerable.Append(c2_20, 0)));
            j_21 = (j_21 + 1);
        };
        throwDie(nSides2_14, nDice2_15, 0, c2_20);
        var p12_22 = (((dynamic)Convert.ToDouble(powInt(nSides1_12, nDice1_13))) * ((dynamic)Convert.ToDouble(powInt(nSides2_14, nDice2_15))));
        double tot_23 = 0;
        i_18 = 0;
        while ((i_18 < len1_16)) {
            j_21 = 0;
            long m_24 = minInt(i_18, len2_19);
            while ((j_21 < m_24)) {
                tot_23 = (tot_23 + (((dynamic)(((dynamic)c1_17[(int)(i_18)]) * ((dynamic)Convert.ToDouble(c2_20[(int)(j_21)])))) / ((dynamic)p12_22)));
                j_21 = (j_21 + 1);
            }
            i_18 = (i_18 + 1);
        };
        return tot_23;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            Console.WriteLine(_fmtTop((beatingProbability(4, 9, 6, 6)).ToString()));
            Console.WriteLine(_fmtTop((beatingProbability(10, 5, 7, 6)).ToString()));
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
