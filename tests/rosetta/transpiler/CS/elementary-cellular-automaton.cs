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
    static long bitAt(long x_0, long idx_1) {
        long v_2 = x_0;
        long i_3 = 0;
        while ((i_3 < idx_1)) {
            v_2 = Convert.ToInt64((v_2 / 2));
            i_3 = (i_3 + 1);
        };
        return (v_2 % 2);
    }

    static void outputState(string state_4) {
        string line_5 = "";
        long i_6 = 0;
        while ((i_6 < state_4.Length)) {
            if ((state_4.Substring((int)(i_6), (int)((i_6 + 1) - i_6)) == "1")) {
                line_5 = (line_5 + "#");
            } else {
                line_5 = (line_5 + " ");
            }
            i_6 = (i_6 + 1);
        };
        Console.WriteLine(_fmtTop(line_5));
    }

    static string step(string state_7, long r_8) {
        var cells_9 = state_7.Length;
        string out_10 = "";
        long i_11 = 0;
        while ((i_11 < cells_9)) {
            string l_12 = state_7.Substring((int)((((dynamic)(((dynamic)(i_11 - 1)) + ((dynamic)cells_9))) % ((dynamic)cells_9))), (int)(((((dynamic)(((dynamic)(i_11 - 1)) + ((dynamic)cells_9))) % ((dynamic)cells_9)) + 1) - (((dynamic)(((dynamic)(i_11 - 1)) + ((dynamic)cells_9))) % ((dynamic)cells_9))));
            string c_13 = state_7.Substring((int)(i_11), (int)((i_11 + 1) - i_11));
            string rt_14 = state_7.Substring((int)((((dynamic)(i_11 + 1)) % ((dynamic)cells_9))), (int)(((((dynamic)(i_11 + 1)) % ((dynamic)cells_9)) + 1) - (((dynamic)(i_11 + 1)) % ((dynamic)cells_9))));
            long idx_15 = 0;
            if ((l_12 == "1")) {
                idx_15 = (idx_15 + 4);
            }
            if ((c_13 == "1")) {
                idx_15 = (idx_15 + 2);
            }
            if ((rt_14 == "1")) {
                idx_15 = (idx_15 + 1);
            }
            if ((bitAt(r_8, idx_15) == 1)) {
                out_10 = (out_10 + "1");
            } else {
                out_10 = (out_10 + "0");
            }
            i_11 = (i_11 + 1);
        };
        return out_10;
    }

    static void elem(long r_16, long cells_17, long generations_18, string state_19) {
        outputState(state_19);
        long g_20 = 0;
        string s_21 = state_19;
        while ((g_20 < generations_18)) {
            s_21 = step(s_21, r_16);
            outputState(s_21);
            g_20 = (g_20 + 1);
        };
    }

    static string randInit(long cells_22, long seed_23) {
        string s_24 = "";
        long val_25 = seed_23;
        long i_26 = 0;
        while ((i_26 < cells_22)) {
            val_25 = (((val_25 * 1664525) + 1013904223) % 2147483647);
            if (((val_25 % 2) == 0)) {
                s_24 = (s_24 + "0");
            } else {
                s_24 = (s_24 + "1");
            }
            i_26 = (i_26 + 1);
        };
        return s_24;
    }

    static string singleInit(long cells_27) {
        string s_28 = "";
        long i_29 = 0;
        while ((i_29 < cells_27)) {
            if ((i_29 == (cells_27 / 2))) {
                s_28 = (s_28 + "1");
            } else {
                s_28 = (s_28 + "0");
            }
            i_29 = (i_29 + 1);
        };
        return s_28;
    }

    static void main() {
        long cells_30 = 20;
        long generations_31 = 9;
        Console.WriteLine(_fmtTop("Single 1, rule 90:"));
        string state_32 = singleInit(cells_30);
        elem(90, cells_30, generations_31, state_32);
        Console.WriteLine(_fmtTop("Random intial state, rule 30:"));
        state_32 = randInit(cells_30, 3);
        elem(30, cells_30, generations_31, state_32);
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
