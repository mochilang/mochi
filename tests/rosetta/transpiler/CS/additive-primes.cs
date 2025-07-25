// Generated by Mochi 0.10.40 on 2025-07-25 19:53 +0700
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
    static bool isPrime(long n) {
        if ((n < 2)) {
            return false;
        };
        if (((n % 2) == 0)) {
            return (n == 2);
        };
        if (((n % 3) == 0)) {
            return (n == 3);
        };
        long d_0 = 5;
        while (((d_0 * d_0) <= n)) {
            if (((n % d_0) == 0)) {
                return false;
            }
            d_0 = (d_0 + 2);
            if (((n % d_0) == 0)) {
                return false;
            }
            d_0 = (d_0 + 4);
        };
        return true;
    }

    static long sumDigits(long n) {
        long s_1 = 0;
        long x_2 = n;
        while ((x_2 > 0)) {
            s_1 = (s_1 + (x_2 % 10));
            x_2 = Convert.ToInt32((x_2 / 10));
        };
        return s_1;
    }

    static string pad(long n) {
        if ((n < 10)) {
            return ("  " + (n).ToString());
        };
        if ((n < 100)) {
            return (" " + (n).ToString());
        };
        return (n).ToString();
    }

    static void main() {
        Console.WriteLine(_fmtTop("Additive primes less than 500:"));
        long count_3 = 0;
        string line_4 = "";
        long lineCount_5 = 0;
        long i_6 = 2;
        while ((i_6 < 500)) {
            if ((isPrime(i_6) && isPrime(sumDigits(i_6)))) {
                count_3 = (count_3 + 1);
                line_4 = ((line_4 + pad(i_6)) + "  ");
                lineCount_5 = (lineCount_5 + 1);
                if ((lineCount_5 == 10)) {
                    Console.WriteLine(_fmtTop(line_4.Substring((int)(0), (int)((((dynamic)line_4.Length) - ((dynamic)2)) - 0))));
                    line_4 = "";
                    lineCount_5 = 0;
                }
            }
            if ((i_6 > 2)) {
                i_6 = (i_6 + 2);
            } else {
                i_6 = (i_6 + 1);
            }
        };
        if ((lineCount_5 > 0)) {
            Console.WriteLine(_fmtTop(line_4.Substring((int)(0), (int)((((dynamic)line_4.Length) - ((dynamic)2)) - 0))));
        };
        Console.WriteLine(_fmtTop(((count_3).ToString() + " additive primes found.")));
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
