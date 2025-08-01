// Generated by Mochi 0.10.47 on 2025-07-28 05:05 UTC
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
    static string c_10 = ((((("Character,Speech\n" + "The multitude,The messiah! Show us the messiah!\n") + "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>\n") + "The multitude,Who are you?\n") + "Brians mother,I'm his mother; that's who!\n") + "The multitude,Behold his mother! Behold his mother!");
    static string[][] rows_11 = new string[][]{};
    static string[] split(string s_0, string sep_1) {
        string[] out_2 = new string[]{};
        long start_3 = 0;
        long i_4 = 0;
        var n_5 = sep_1.Length;
        while ((i_4 <= (((dynamic)s_0.Length) - ((dynamic)n_5)))) {
            if ((s_0.Substring((int)(i_4), (int)((((dynamic)i_4) + ((dynamic)n_5)) - i_4)) == sep_1)) {
                out_2 = (Enumerable.ToArray(Enumerable.Append(out_2, s_0.Substring((int)(start_3), (int)(i_4 - start_3)))));
                i_4 = (((dynamic)i_4) + ((dynamic)n_5));
                start_3 = i_4;
            } else {
                i_4 = (i_4 + 1);
            }
        };
        out_2 = (Enumerable.ToArray(Enumerable.Append(out_2, s_0.Substring((int)(start_3), (int)(s_0.Length - start_3)))));
        return out_2;
    }

    static string htmlEscape(string s_6) {
        string out_7 = "";
        long i_8 = 0;
        while ((i_8 < s_6.Length)) {
            string ch_9 = s_6.Substring((int)(i_8), (int)((i_8 + 1) - i_8));
            if ((ch_9 == "&")) {
                out_7 = (out_7 + "&amp;");
            } else {
                if ((ch_9 == "<")) {
                    out_7 = (out_7 + "&lt;");
                } else {
                    if ((ch_9 == ">")) {
                        out_7 = (out_7 + "&gt;");
                    } else {
                        out_7 = (out_7 + ch_9);
                    }
                }
            }
            i_8 = (i_8 + 1);
        };
        return out_7;
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            foreach (var line_12 in split(c_10, "\n")) {
                rows_11 = (Enumerable.ToArray(Enumerable.Append(rows_11, split(line_12, ","))));
            }
            Console.WriteLine(_fmtTop("<table>"));
            foreach (var row_13 in rows_11) {
                string cells_14 = "";
                foreach (var cell_15 in row_13) {
                    cells_14 = (((cells_14 + "<td>") + htmlEscape(cell_15)) + "</td>");
                }
                Console.WriteLine(_fmtTop((("    <tr>" + cells_14) + "</tr>")));
            }
            Console.WriteLine(_fmtTop("</table>"));
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
