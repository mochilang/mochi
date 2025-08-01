// Generated by Mochi 0.10.50 on 2025-07-30 21:05 +0700
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
    static string[] splitPath(string p_0) {
        string[] parts_1 = new string[]{};
        string cur_2 = "";
        long i_3 = 0;
        while ((i_3 < p_0.Length)) {
            if ((p_0.Substring((int)(i_3), (int)((i_3 + 1) - i_3)) == "/")) {
                if ((cur_2 != "")) {
                    parts_1 = (Enumerable.ToArray(Enumerable.Append(parts_1, cur_2)));
                    cur_2 = "";
                }
            } else {
                cur_2 = (cur_2 + p_0.Substring((int)(i_3), (int)((i_3 + 1) - i_3)));
            }
            i_3 = (i_3 + 1);
        };
        if ((cur_2 != "")) {
            parts_1 = (Enumerable.ToArray(Enumerable.Append(parts_1, cur_2)));
        };
        return parts_1;
    }

    static string joinPath(string[] parts_4) {
        string s_5 = "";
        long i_6 = 0;
        while ((i_6 < parts_4.Length)) {
            s_5 = ((s_5 + "/") + parts_4[(int)(i_6)]);
            i_6 = (i_6 + 1);
        };
        return s_5;
    }

    static string commonPrefix(string[] paths_7) {
        if ((paths_7.Length == 0)) {
            return "";
        };
        string[] base_8 = splitPath(paths_7[(int)(0)]);
        long i_9 = 0;
        string[] prefix_10 = new string[]{};
        while ((i_9 < base_8.Length)) {
            string comp_11 = base_8[(int)(i_9)];
            bool ok_12 = true;
            foreach (var p_13 in paths_7) {
                string[] parts_14 = splitPath(p_13);
                if (((i_9 >= parts_14.Length) || (parts_14[(int)(i_9)] != comp_11))) {
                    ok_12 = false;
                    break;
                }
            }
            if (ok_12) {
                prefix_10 = (Enumerable.ToArray(Enumerable.Append(prefix_10, comp_11)));
            } else {
                break;
            }
            i_9 = (i_9 + 1);
        };
        return joinPath(prefix_10);
    }

    static void main() {
        string[] paths_15 = new string[]{"/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members", "/home//user1/tmp/coventry", "/home/user1/././tmp/covertly/foo", "/home/bob/../user1/tmp/coved/bar"};
        string c_16 = commonPrefix(paths_15);
        if ((c_16 == "")) {
            Console.WriteLine(_fmtTop("No common path"));
        } else {
            Console.WriteLine(_fmtTop(("Common path: " + c_16)));
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
