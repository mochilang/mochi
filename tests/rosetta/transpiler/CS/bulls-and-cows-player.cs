// Generated by Mochi 0.10.52 on 2025-07-31 14:19 +0700
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
    static long indexOf(string s_0, string ch_1) {
        long i_2 = 0;
        while ((Convert.ToDouble(i_2) < Convert.ToDouble(s_0.Length))) {
            if ((s_0.Substring((int)(i_2), (int)((i_2 + 1) - i_2)) == ch_1)) {
                return i_2;
            }
            i_2 = (i_2 + 1);
        };
        return -1;
    }

    static string[] fields(string s_3) {
        string[] words_4 = new string[]{};
        string cur_5 = "";
        long i_6 = 0;
        while ((Convert.ToDouble(i_6) < Convert.ToDouble(s_3.Length))) {
            string ch_7 = s_3.Substring((int)(i_6), (int)((i_6 + 1) - i_6));
            if ((((ch_7 == " ") || (ch_7 == "\t")) || (ch_7 == "\n"))) {
                if ((Convert.ToDouble(cur_5.Length) > Convert.ToDouble(0))) {
                    words_4 = (Enumerable.ToArray(Enumerable.Append(words_4, cur_5)));
                    cur_5 = "";
                }
            } else {
                cur_5 = (cur_5 + ch_7);
            }
            i_6 = (i_6 + 1);
        };
        if ((Convert.ToDouble(cur_5.Length) > Convert.ToDouble(0))) {
            words_4 = (Enumerable.ToArray(Enumerable.Append(words_4, cur_5)));
        };
        return words_4;
    }

    static string[] makePatterns() {
        string[] digits_8 = new string[]{"1", "2", "3", "4", "5", "6", "7", "8", "9"};
        string[] pats_9 = new string[]{};
        long i_10 = 0;
        while ((Convert.ToDouble(i_10) < Convert.ToDouble(digits_8.Length))) {
            long j_11 = 0;
            while ((Convert.ToDouble(j_11) < Convert.ToDouble(digits_8.Length))) {
                if ((j_11 != i_10)) {
                    long k_12 = 0;
                    while ((Convert.ToDouble(k_12) < Convert.ToDouble(digits_8.Length))) {
                        if (((k_12 != i_10) && (k_12 != j_11))) {
                            long l_13 = 0;
                            while ((Convert.ToDouble(l_13) < Convert.ToDouble(digits_8.Length))) {
                                if ((((l_13 != i_10) && (l_13 != j_11)) && (l_13 != k_12))) {
                                    pats_9 = (Enumerable.ToArray(Enumerable.Append(pats_9, (((digits_8[(int)(i_10)] + digits_8[(int)(j_11)]) + digits_8[(int)(k_12)]) + digits_8[(int)(l_13)]))));
                                }
                                l_13 = (l_13 + 1);
                            }
                        }
                        k_12 = (k_12 + 1);
                    }
                }
                j_11 = (j_11 + 1);
            }
            i_10 = (i_10 + 1);
        };
        return pats_9;
    }

    static void main() {
        Console.WriteLine(_fmtTop(((((("Cows and bulls/player\n" + "You think of four digit number of unique digits in the range 1 to 9.\n") + "I guess.  You score my guess:\n") + "    A correct digit but not in the correct place is a cow.\n") + "    A correct digit in the correct place is a bull.\n") + "You give my score as two numbers separated with a space.")));
        string[] patterns_14 = makePatterns();
        while (true) {
            if ((patterns_14.Length == 0)) {
                Console.WriteLine(_fmtTop("Oops, check scoring."));
                return;
            }
            string guess_15 = patterns_14[(int)(0)];
            patterns_14 = patterns_14.Skip((int)(1)).Take((int)((patterns_14.Length - 1))).ToArray();
            long cows_16 = 0;
            long bulls_17 = 0;
            while (true) {
                Console.WriteLine(_fmtTop((("My guess: " + guess_15) + ".  Score? (c b) ")));
                string line_18 = _input();
                string[] toks_19 = fields(line_18);
                if ((toks_19.Length == 2)) {
                    long c_20 = Convert.ToInt64(toks_19[(int)(0)]);
                    long b_21 = Convert.ToInt64(toks_19[(int)(1)]);
                    if ((((((c_20 >= 0) && (c_20 <= 4)) && (b_21 >= 0)) && (b_21 <= 4)) && ((c_20 + b_21) <= 4))) {
                        cows_16 = c_20;
                        bulls_17 = b_21;
                        break;
                    }
                }
                Console.WriteLine(_fmtTop("Score guess as two numbers: cows bulls"));
            }
            if ((bulls_17 == 4)) {
                Console.WriteLine(_fmtTop("I did it. :)"));
                return;
            }
            string[] next_22 = new string[]{};
            long idx_23 = 0;
            while ((Convert.ToDouble(idx_23) < Convert.ToDouble(patterns_14.Length))) {
                string pat_24 = patterns_14[(int)(idx_23)];
                long c_25 = 0;
                long b_26 = 0;
                long i_27 = 0;
                while ((i_27 < 4)) {
                    string cg_28 = guess_15.Substring((int)(i_27), (int)((i_27 + 1) - i_27));
                    string cp_29 = pat_24.Substring((int)(i_27), (int)((i_27 + 1) - i_27));
                    if ((cg_28 == cp_29)) {
                        b_26 = (b_26 + 1);
                    } else {
                        if ((indexOf(pat_24, cg_28) >= 0)) {
                            c_25 = (c_25 + 1);
                        }
                    }
                    i_27 = (i_27 + 1);
                }
                if (((c_25 == cows_16) && (b_26 == bulls_17))) {
                    next_22 = (Enumerable.ToArray(Enumerable.Append(next_22, pat_24)));
                }
                idx_23 = (idx_23 + 1);
            }
            patterns_14 = next_22;
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
