// Generated by Mochi 0.10.52 on 2025-08-01 15:22 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class FWResult {
    public long[][] dist;
    public long[][] next;
    public override string ToString() => $"FWResult {{dist = {dist}, next = {next}}}";
}
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
    static long INF_0 = 1000000;
    static long n_21 = 4;
    static long[][] g_22 = new long[][]{};
    static FWResult floydWarshall(long[][] graph_1) {
        var n_2 = graph_1.Length;
        long[][] dist_3 = new long[][]{};
        long[][] next_4 = new long[][]{};
        long i_5 = 0;
        while ((Convert.ToDouble(i_5) < Convert.ToDouble(n_2))) {
            long[] drow_6 = new long[]{};
            long[] nrow_7 = new long[]{};
            long j_8 = 0;
            while ((Convert.ToDouble(j_8) < Convert.ToDouble(n_2))) {
                drow_6 = (Enumerable.ToArray(Enumerable.Append(drow_6, graph_1[(int)(i_5)][(int)(j_8)])));
                if (((graph_1[(int)(i_5)][(int)(j_8)] < INF_0) && (i_5 != j_8))) {
                    nrow_7 = (Enumerable.ToArray(Enumerable.Append(nrow_7, j_8)));
                } else {
                    nrow_7 = (Enumerable.ToArray(Enumerable.Append(nrow_7, -1)));
                }
                j_8 = (j_8 + 1);
            }
            dist_3 = (Enumerable.ToArray(Enumerable.Append(dist_3, drow_6)));
            next_4 = (Enumerable.ToArray(Enumerable.Append(next_4, nrow_7)));
            i_5 = (i_5 + 1);
        };
        long k_9 = 0;
        while ((Convert.ToDouble(k_9) < Convert.ToDouble(n_2))) {
            long i_10 = 0;
            while ((Convert.ToDouble(i_10) < Convert.ToDouble(n_2))) {
                long j_11 = 0;
                while ((Convert.ToDouble(j_11) < Convert.ToDouble(n_2))) {
                    if (((dist_3[(int)(i_10)][(int)(k_9)] < INF_0) && (dist_3[(int)(k_9)][(int)(j_11)] < INF_0))) {
                        long alt_12 = (dist_3[(int)(i_10)][(int)(k_9)] + dist_3[(int)(k_9)][(int)(j_11)]);
                        if ((alt_12 < dist_3[(int)(i_10)][(int)(j_11)])) {
                            dist_3[(int)(i_10)][j_11] = alt_12;
                            next_4[(int)(i_10)][j_11] = next_4[(int)(i_10)][(int)(k_9)];
                        }
                    }
                    j_11 = (j_11 + 1);
                }
                i_10 = (i_10 + 1);
            }
            k_9 = (k_9 + 1);
        };
        return new FWResult{dist = dist_3, next = next_4};
    }

    static long[] path(long u_13, long v_14, long[][] next_15) {
        if ((next_15[(int)(u_13)][(int)(v_14)] < 0)) {
            return new long[]{};
        };
        long[] p_16 = new long[]{u_13};
        long x_17 = u_13;
        while ((x_17 != v_14)) {
            x_17 = next_15[(int)(x_17)][(int)(v_14)];
            p_16 = (Enumerable.ToArray(Enumerable.Append(p_16, x_17)));
        };
        return p_16;
    }

    static string pathStr(long[] p_18) {
        string s_19 = "";
        long i_20 = 0;
        while ((Convert.ToDouble(i_20) < Convert.ToDouble(p_18.Length))) {
            s_19 = (s_19 + _fmt((p_18[(int)(i_20)] + 1)));
            if ((i_20 < (((dynamic)(p_18.Length)) - ((dynamic)(1))))) {
                s_19 = (s_19 + " -> ");
            }
            i_20 = (i_20 + 1);
        };
        return s_19;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            for (var i_23 = 0; i_23 < n_21; i_23++) {
                long[] row_24 = new long[]{};
                for (var j_25 = 0; j_25 < n_21; j_25++) {
                    if ((i_23 == j_25)) {
                        row_24 = (Enumerable.ToArray(Enumerable.Append(row_24, 0)));
                    } else {
                        row_24 = (Enumerable.ToArray(Enumerable.Append(row_24, INF_0)));
                    }
                }
                g_22 = (Enumerable.ToArray(Enumerable.Append(g_22, row_24)));
            }
            g_22[(int)(0)][2] = -2;
            g_22[(int)(2)][3] = 2;
            g_22[(int)(3)][1] = -1;
            g_22[(int)(1)][0] = 4;
            g_22[(int)(1)][2] = 3;
            FWResult res_26 = floydWarshall(g_22);
            Console.WriteLine(_fmtTop("pair\tdist\tpath"));
            long i_27 = 0;
            while ((i_27 < n_21)) {
                long j_28 = 0;
                while ((j_28 < n_21)) {
                    if ((i_27 != j_28)) {
                        long[] p_29 = path(i_27, j_28, res_26.next);
                        Console.WriteLine(_fmtTop(((((((_fmt((i_27 + 1)) + " -> ") + _fmt((j_28 + 1))) + "\t") + _fmt(res_26.dist[(int)(i_27)][(int)(j_28)])) + "\t") + pathStr(p_29))));
                    }
                    j_28 = (j_28 + 1);
                }
                i_27 = (i_27 + 1);
            }
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
