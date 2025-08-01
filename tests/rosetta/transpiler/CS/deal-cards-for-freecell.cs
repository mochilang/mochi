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
    static long seed_0 = 1;
    static string suits_6 = "CDHS";
    static string nums_7 = "A23456789TJQK";
    static long rnd() {
        seed_0 = (((seed_0 * 214013) + 2531011) % 2147483648L);
        return (seed_0 / 65536);
    }

    static long[] deal(long game_1) {
        seed_0 = game_1;
        long[] deck_2 = new long[]{};
        long i_3 = 0;
        while ((i_3 < 52)) {
            deck_2 = (Enumerable.ToArray(Enumerable.Append(deck_2, (51 - i_3))));
            i_3 = (i_3 + 1);
        };
        i_3 = 0;
        while ((i_3 < 51)) {
            long j_4 = (51 - (rnd() % (52 - i_3)));
            long tmp_5 = deck_2[(int)(i_3)];
            deck_2[i_3] = deck_2[(int)(j_4)];
            deck_2[j_4] = tmp_5;
            i_3 = (i_3 + 1);
        };
        return deck_2;
    }

    static void show(long[] cards_8) {
        long i_9 = 0;
        while ((i_9 < cards_8.Length)) {
            long c_10 = cards_8[(int)(i_9)];
            Console.Write(((" " + nums_7.Substring((int)((c_10 / 4)), (int)(((c_10 / 4) + 1) - (c_10 / 4)))) + suits_6.Substring((int)((c_10 % 4)), (int)(((c_10 % 4) + 1) - (c_10 % 4)))));
            if (((((i_9 + 1) % 8) == 0) || ((i_9 + 1) == cards_8.Length))) {
                Console.WriteLine(_fmtTop(""));
            }
            i_9 = (i_9 + 1);
        };
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            Console.WriteLine(_fmtTop(""));
            Console.WriteLine(_fmtTop("Game #1"));
            show(deal(1));
            Console.WriteLine(_fmtTop(""));
            Console.WriteLine(_fmtTop("Game #617"));
            show(deal(617));
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
