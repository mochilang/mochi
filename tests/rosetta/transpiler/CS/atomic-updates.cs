// Generated by Mochi 0.10.40 on 2025-07-25 14:26 UTC
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
    static long[] randOrder(long seed, long n) {
        long next_0 = (((seed * 1664525) + 1013904223) % 2147483647);
        return new long[]{next_0, (next_0 % n)};
    }

    static long[] randChaos(long seed, long n) {
        long next_1 = (((seed * 1103515245) + 12345) % 2147483647);
        return new long[]{next_1, (next_1 % n)};
    }

    static void main() {
        long nBuckets_2 = 10;
        long initialSum_3 = 1000;
        long[] buckets_4 = new long[]{};
        for (var i_5 = 0; i_5 < nBuckets_2; i_5++) {
            buckets_4 = (Enumerable.ToArray(Enumerable.Append(buckets_4, 0)));
        };
        long i_6 = nBuckets_2;
        long dist_7 = initialSum_3;
        while ((i_6 > 0)) {
            long v_8 = (dist_7 / i_6);
            i_6 = (i_6 - 1);
            buckets_4[i_6] = v_8;
            dist_7 = (dist_7 - v_8);
        };
        long tc0_9 = 0;
        long tc1_10 = 0;
        long total_11 = 0;
        long nTicks_12 = 0;
        long seedOrder_13 = 1;
        long seedChaos_14 = 2;
        Console.WriteLine(_fmtTop("sum  ---updates---    mean  buckets"));
        long t_15 = 0;
        while ((t_15 < 5)) {
            long[] r_16 = randOrder(seedOrder_13, nBuckets_2);
            seedOrder_13 = r_16[(int)(0)];
            long b1_17 = r_16[(int)(1)];
            long b2_18 = ((b1_17 + 1) % nBuckets_2);
            long v1_19 = buckets_4[(int)(b1_17)];
            long v2_20 = buckets_4[(int)(b2_18)];
            if ((v1_19 > v2_20)) {
                long a_21 = Convert.ToInt64(((v1_19 - v2_20) / 2));
                if ((a_21 > buckets_4[(int)(b1_17)])) {
                    a_21 = buckets_4[(int)(b1_17)];
                }
                buckets_4[b1_17] = (buckets_4[(int)(b1_17)] - a_21);
                buckets_4[b2_18] = (buckets_4[(int)(b2_18)] + a_21);
            } else {
                long a_22 = Convert.ToInt64(((v2_20 - v1_19) / 2));
                if ((a_22 > buckets_4[(int)(b2_18)])) {
                    a_22 = buckets_4[(int)(b2_18)];
                }
                buckets_4[b2_18] = (buckets_4[(int)(b2_18)] - a_22);
                buckets_4[b1_17] = (buckets_4[(int)(b1_17)] + a_22);
            }
            tc0_9 = (tc0_9 + 1);
            r_16 = randChaos(seedChaos_14, nBuckets_2);
            seedChaos_14 = r_16[(int)(0)];
            b1_17 = r_16[(int)(1)];
            b2_18 = ((b1_17 + 1) % nBuckets_2);
            r_16 = randChaos(seedChaos_14, (buckets_4[(int)(b1_17)] + 1));
            seedChaos_14 = r_16[(int)(0)];
            long amt_23 = r_16[(int)(1)];
            if ((amt_23 > buckets_4[(int)(b1_17)])) {
                amt_23 = buckets_4[(int)(b1_17)];
            }
            buckets_4[b1_17] = (buckets_4[(int)(b1_17)] - amt_23);
            buckets_4[b2_18] = (buckets_4[(int)(b2_18)] + amt_23);
            tc1_10 = (tc1_10 + 1);
            long sum_24 = 0;
            long idx_25 = 0;
            while ((idx_25 < nBuckets_2)) {
                sum_24 = (sum_24 + buckets_4[(int)(idx_25)]);
                idx_25 = (idx_25 + 1);
            }
            total_11 = ((total_11 + tc0_9) + tc1_10);
            nTicks_12 = (nTicks_12 + 1);
            Console.WriteLine(_fmtTop((((((((((sum_24).ToString() + " ") + (tc0_9).ToString()) + " ") + (tc1_10).ToString()) + " ") + ((total_11 / nTicks_12)).ToString()) + "  ") + (buckets_4).ToString())));
            tc0_9 = 0;
            tc1_10 = 0;
            t_15 = (t_15 + 1);
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
