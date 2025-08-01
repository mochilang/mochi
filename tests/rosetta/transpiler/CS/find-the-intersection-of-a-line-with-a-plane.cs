// Generated by Mochi 0.10.50 on 2025-07-30 21:05 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class Vector {
    public double x;
    public double y;
    public double z;
    public override string ToString() => $"Vector {{x = {x.ToString("0.0")}, y = {y.ToString("0.0")}, z = {z.ToString("0.0")}}}";
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
    static Vector add(Vector a_0, Vector b_1) {
        return new Vector{x = (a_0.x + b_1.x), y = (a_0.y + b_1.y), z = (a_0.z + b_1.z)};
    }

    static Vector sub(Vector a_2, Vector b_3) {
        return new Vector{x = (a_2.x - b_3.x), y = (a_2.y - b_3.y), z = (a_2.z - b_3.z)};
    }

    static Vector mul(Vector v_4, double s_5) {
        return new Vector{x = (v_4.x * s_5), y = (v_4.y * s_5), z = (v_4.z * s_5)};
    }

    static double dot(Vector a_6, Vector b_7) {
        return (((a_6.x * b_7.x) + (a_6.y * b_7.y)) + (a_6.z * b_7.z));
    }

    static Vector intersectPoint(Vector rv_8, Vector rp_9, Vector pn_10, Vector pp_11) {
        Vector diff_12 = sub(rp_9, pp_11);
        double prod1_13 = dot(diff_12, pn_10);
        double prod2_14 = dot(rv_8, pn_10);
        double prod3_15 = (prod1_13 / prod2_14);
        return sub(rp_9, mul(rv_8, prod3_15));
    }

    static void main() {
        Vector rv_16 = new Vector{x = 0, y = -1, z = -1};
        Vector rp_17 = new Vector{x = 0, y = 0, z = 10};
        Vector pn_18 = new Vector{x = 0, y = 0, z = 1};
        Vector pp_19 = new Vector{x = 0, y = 0, z = 5};
        Vector ip_20 = intersectPoint(rv_16, rp_17, pn_18, pp_19);
        Console.WriteLine(_fmtTop((((((("The ray intersects the plane at (" + (ip_20.x).ToString()) + ", ") + (ip_20.y).ToString()) + ", ") + (ip_20.z).ToString()) + ")")));
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
