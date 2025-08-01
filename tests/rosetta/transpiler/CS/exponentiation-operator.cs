// Generated by Mochi 0.10.50 on 2025-07-31 08:23 +0700
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
    static void printExpI(long b_0, long p_1) {
        if ((p_1 < 0)) {
            Console.WriteLine(_fmtTop(((((b_0).ToString() + "^") + (p_1).ToString()) + ": negative power not allowed")));
            return;
        };
        long r_2 = 1;
        long i_3 = 1;
        while ((i_3 <= p_1)) {
            r_2 = (r_2 * b_0);
            i_3 = (i_3 + 1);
        };
        Console.WriteLine(_fmtTop((((((b_0).ToString() + "^") + (p_1).ToString()) + ": ") + (r_2).ToString())));
    }

    static double expF(double b_4, long p_5) {
        bool neg_6 = false;
        if ((p_5 < 0)) {
            neg_6 = true;
            p_5 = -p_5;
        };
        double r_7 = 1;
        double pow_8 = b_4;
        while ((p_5 > 0)) {
            if (((p_5 % 2) == 1)) {
                r_7 = (r_7 * pow_8);
            }
            pow_8 = (pow_8 * pow_8);
            p_5 = (p_5 / 2);
        };
        if (neg_6) {
            r_7 = (1 / r_7);
        };
        return r_7;
    }

    static void printExpF(double b_9, long p_10) {
        if (((b_9 == 0) && (p_10 < 0))) {
            Console.WriteLine(_fmtTop(((((b_9).ToString() + "^") + (p_10).ToString()) + ": +Inf")));
            return;
        };
        Console.WriteLine(_fmtTop((((((b_9).ToString() + "^") + (p_10).ToString()) + ": ") + (expF(b_9, p_10)).ToString())));
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            Console.WriteLine(_fmtTop("expI tests"));
            printExpI(2, 10);
            printExpI(2, -10);
            printExpI(-2, 10);
            printExpI(-2, 11);
            printExpI(11, 0);
            Console.WriteLine(_fmtTop("overflow undetected"));
            printExpI(10, 10);
            Console.WriteLine(_fmtTop("\nexpF tests:"));
            printExpF(2, 10);
            printExpF(2, -10);
            printExpF(-2, 10);
            printExpF(-2, 11);
            printExpF(11, 0);
            Console.WriteLine(_fmtTop("disallowed in expI, allowed here"));
            printExpF(0, -1);
            Console.WriteLine(_fmtTop("other interesting cases for 32 bit float type"));
            printExpF(10, 39);
            printExpF(10, -39);
            printExpF(-10, 39);
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
