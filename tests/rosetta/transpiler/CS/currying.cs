// Generated by Mochi 0.10.47 on 2025-07-28 05:10 UTC
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class Foo {
    public long value;
    public long Method(long b) {
        return (((dynamic)value) + ((dynamic)b));
    }

    public override string ToString() => $"Foo {{value = {value}}}";
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
    static double pow(double base_0, double exp_1) {
        double result_2 = 1;
        long i_3 = 0;
        while ((i_3 < Convert.ToInt64(exp_1))) {
            result_2 = (result_2 * base_0);
            i_3 = (i_3 + 1);
        };
        return result_2;
    }

    static Func<double, double> PowN(double b_4) {
        return (double e) => pow(b_4, e);
    }

    static Func<double, double> PowE(double e_5) {
        return (double b) => pow(b, e_5);
    }

    static void main() {
        Func<double, double> pow2_6 = PowN(2);
        Func<double, double> cube_7 = PowE(3);
        Console.WriteLine(_fmtTop(("2^8 = " + (pow2_6(8)).ToString())));
        Console.WriteLine(_fmtTop(("4³ = " + (cube_7(4)).ToString())));
        Foo a_8 = new Foo{value = 2};
        Func<long, long> fn1_9 = null;
        fn1_9 = (long b) => a_8.Method(b);
        Func<Foo, long, long> fn2_10 = null;
        fn2_10 = (Foo f, long b) => f.Method(b);
        Console.WriteLine(_fmtTop(("2 + 2 = " + (a_8.Method(2)).ToString())));
        Console.WriteLine(_fmtTop(("2 + 3 = " + (fn1_9(3)).ToString())));
        Console.WriteLine(_fmtTop(("2 + 4 = " + (fn2_10(a_8, 4)).ToString())));
        Console.WriteLine(_fmtTop(("3 + 5 = " + (fn2_10(new Foo{value = 3}, 5)).ToString())));
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
