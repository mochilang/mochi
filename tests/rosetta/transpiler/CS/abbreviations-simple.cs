// Generated by Mochi 0.10.40 on 2025-07-25 19:02 +0700
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
    static string[] fields(string s) {
        string[] words_0 = new string[]{};
        string cur_1 = "";
        long i_2 = 0;
        while ((i_2 < s.Length)) {
            string ch_3 = s.Substring((int)(i_2), (int)((i_2 + 1) - i_2));
            if ((((ch_3 == " ") || (ch_3 == "\n")) || (ch_3 == "\t"))) {
                if ((cur_1.Length > 0)) {
                    words_0 = (Enumerable.ToArray(Enumerable.Append(words_0, cur_1)));
                    cur_1 = "";
                }
            } else {
                cur_1 = (cur_1 + ch_3);
            }
            i_2 = (i_2 + 1);
        };
        if ((cur_1.Length > 0)) {
            words_0 = (Enumerable.ToArray(Enumerable.Append(words_0, cur_1)));
        };
        return words_0;
    }

    static string padRight(string s, long width) {
        string out_4 = s;
        long i_5 = s.Length;
        while ((i_5 < width)) {
            out_4 = (out_4 + " ");
            i_5 = (i_5 + 1);
        };
        return out_4;
    }

    static string join(string[] xs, string sep) {
        string res_6 = "";
        long i_7 = 0;
        while ((i_7 < xs.Length)) {
            if ((i_7 > 0)) {
                res_6 = (res_6 + sep);
            }
            res_6 = (res_6 + xs[i_7]);
            i_7 = (i_7 + 1);
        };
        return res_6;
    }

    static long parseIntStr(string str) {
        long i_8 = 0;
        bool neg_9 = false;
        if (((str.Length > 0) && (str.Substring((int)(0), (int)(1 - 0)) == "-"))) {
            neg_9 = true;
            i_8 = 1;
        };
        long n_10 = 0;
        Dictionary<string, long> digits_11 = new Dictionary<string, long>{{"0", 0}, {"1", 1}, {"2", 2}, {"3", 3}, {"4", 4}, {"5", 5}, {"6", 6}, {"7", 7}, {"8", 8}, {"9", 9}};
        while ((i_8 < str.Length)) {
            n_10 = ((n_10 * 10) + digits_11[str.Substring((int)(i_8), (int)((i_8 + 1) - i_8))]);
            i_8 = (i_8 + 1);
        };
        if (neg_9) {
            n_10 = -n_10;
        };
        return n_10;
    }

    static bool isDigits(string s) {
        if ((s.Length == 0)) {
            return false;
        };
        long i_12 = 0;
        while ((i_12 < s.Length)) {
            string ch_13 = s.Substring((int)(i_12), (int)((i_12 + 1) - i_12));
            if (((string.Compare(ch_13, "0") < 0) || (string.Compare(ch_13, "9") > 0))) {
                return false;
            }
            i_12 = (i_12 + 1);
        };
        return true;
    }

    static Dictionary<string, object> readTable(string table) {
        string[] toks_14 = fields(table);
        string[] cmds_15 = new string[]{};
        long[] mins_16 = new long[]{};
        long i_17 = 0;
        while ((i_17 < toks_14.Length)) {
            string cmd_18 = toks_14[i_17];
            long minlen_19 = cmd_18.Length;
            i_17 = (i_17 + 1);
            if (((i_17 < toks_14.Length) && isDigits(toks_14[i_17]))) {
                long num_20 = parseIntStr(toks_14[i_17]);
                if (((num_20 >= 1) && (num_20 < cmd_18.Length))) {
                    minlen_19 = num_20;
                    i_17 = (i_17 + 1);
                }
            }
            cmds_15 = (Enumerable.ToArray(Enumerable.Append(cmds_15, cmd_18)));
            mins_16 = (Enumerable.ToArray(Enumerable.Append(mins_16, minlen_19)));
        };
        return new Dictionary<string, object>{{"commands", cmds_15}, {"mins", mins_16}};
    }

    static string[] validate(string[] commands, long[] mins, string[] words) {
        string[] results_21 = new string[]{};
        long wi_22 = 0;
        while ((wi_22 < words.Length)) {
            string w_23 = words[wi_22];
            bool found_24 = false;
            var wlen_25 = w_23.Length;
            long ci_26 = 0;
            while ((ci_26 < commands.Length)) {
                string cmd_27 = commands[ci_26];
                if ((((mins[ci_26] != 0) && (wlen_25 >= mins[ci_26])) && (wlen_25 <= cmd_27.Length))) {
                    var c_28 = cmd_27.ToUpper();
                    var ww_29 = w_23.ToUpper();
                    if ((c_28.Substring((int)(0), (int)(wlen_25 - 0)) == ww_29)) {
                        results_21 = (Enumerable.ToArray(Enumerable.Append(results_21, c_28)));
                        found_24 = true;
                        break;
                    }
                }
                ci_26 = (ci_26 + 1);
            }
            if ((!found_24)) {
                results_21 = (Enumerable.ToArray(Enumerable.Append(results_21, "*error*")));
            }
            wi_22 = (wi_22 + 1);
        };
        return results_21;
    }

    static void main() {
        string table_30 = (((((((("" + "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 ") + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate ") + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 ") + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load ") + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 ") + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 ") + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left ") + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 ");
        string sentence_31 = "riG   rePEAT copies  put mo   rest    types   fup.    6\npoweRin";
        Dictionary<string, object> tbl_32 = readTable(table_30);
        string[] commands_33 = (string[])((dynamic)tbl_32)["commands"];
        long[] mins_34 = (long[])((dynamic)tbl_32)["mins"];
        string[] words_35 = fields(sentence_31);
        string[] results_36 = validate(commands_33, mins_34, words_35);
        string out1_37 = "user words:";
        long k_38 = 0;
        while ((k_38 < words_35.Length)) {
            out1_37 = (out1_37 + " ");
            if ((k_38 < (words_35.Length - 1))) {
                out1_37 = (out1_37 + padRight(words_35[k_38], results_36[k_38].Length));
            } else {
                out1_37 = (out1_37 + words_35[k_38]);
            }
            k_38 = (k_38 + 1);
        };
        Console.WriteLine(_fmtTop(out1_37));
        Console.WriteLine(_fmtTop(("full words: " + join(results_36, " "))));
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
