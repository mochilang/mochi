// Generated by Mochi 0.10.50 on 2025-07-30 21:05 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.IO;
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
    static string[] inputLines;
    static int inputIndex = 0;
    static string _input() {
        if (inputLines == null) {
            var path = Environment.GetEnvironmentVariable("MOCHI_INPUT_FILE");
            if (!string.IsNullOrEmpty(path) && File.Exists(path)) {
                inputLines = File.ReadAllLines(path);
            } else {
                inputLines = new string[]{};
            }
        }
        if (inputIndex < inputLines.Length) {
            return inputLines[inputIndex++];
        }
        var line = Console.ReadLine();
        return line == null ? "" : line;
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
    static bool leapYear(long y_0) {
        return ((((y_0 % 4) == 0) && ((y_0 % 100) != 0)) || ((y_0 % 400) == 0));
    }

    static long monthDays(long y_1, long m_2) {
        long[] days_3 = new long[]{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
        if (((m_2 == 2) && leapYear(y_1))) {
            return 29;
        };
        return days_3[(int)(m_2)];
    }

    static long zeller(long y_4, long m_5, long d_6) {
        long mm_7 = m_5;
        long yy_8 = y_4;
        if ((mm_7 < 3)) {
            mm_7 = (mm_7 + 12);
            yy_8 = (yy_8 - 1);
        };
        long K_9 = (yy_8 % 100);
        long J_10 = (yy_8 / 100);
        long h_11 = ((((((d_6 + ((13 * (mm_7 + 1)) / 5)) + K_9) + (K_9 / 4)) + (J_10 / 4)) + (5 * J_10)) % 7);
        return ((h_11 + 6) % 7);
    }

    static long lastSunday(long y_12, long m_13) {
        long day_14 = monthDays(y_12, m_13);
        while (((day_14 > 0) && (zeller(y_12, m_13, day_14) != 0))) {
            day_14 = (day_14 - 1);
        };
        return day_14;
    }

    static string monthName(long m_15) {
        string[] names_16 = new string[]{"", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};
        return names_16[(int)(m_15)];
    }

    static void main() {
        var year_17 = Convert.ToInt64(_input());
        Console.WriteLine(_fmtTop(("Last Sundays of each month of " + (year_17).ToString())));
        Console.WriteLine(_fmtTop("=================================="));
        long m_18 = 1;
        while ((m_18 <= 12)) {
            long day_19 = lastSunday(year_17, m_18);
            Console.WriteLine(_fmtTop(((monthName(m_18) + ": ") + (day_19).ToString())));
            m_18 = (m_18 + 1);
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
