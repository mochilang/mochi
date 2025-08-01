// Generated by Mochi 0.10.42 on 2025-07-28 10:03 +0700
using System;
using System.Linq;
using System.Numerics;
using System.Collections;
using System.Collections.Generic;

class Program {
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
    static string dna_4 = (((((((((("" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT");
    static dynamic le_5 = dna_4.Length;
    static long i_6 = 0;
    static long a_8 = 0;
    static long c_9 = 0;
    static long g_10 = 0;
    static long t_11 = 0;
    static long idx_12 = 0;
    static string padLeft(string s_0, long w_1) {
        string res_2 = "";
        long n_3 = (((dynamic)w_1) - ((dynamic)s_0.Length));
        while ((n_3 > 0)) {
            res_2 = (res_2 + " ");
            n_3 = (n_3 - 1);
        };
        return (res_2 + s_0);
    }

    static void Main() {
        Console.WriteLine(_fmtTop("SEQUENCE:"));
        while ((i_6 < le_5)) {
            long k_7 = (i_6 + 50);
            if ((k_7 > le_5)) {
                k_7 = le_5;
            }
            Console.WriteLine(_fmtTop(((padLeft((i_6).ToString(), 5) + ": ") + dna_4.Substring((int)(i_6), (int)(k_7 - i_6)))));
            i_6 = (i_6 + 50);
        }
        while ((idx_12 < le_5)) {
            string ch_13 = dna_4.Substring((int)(idx_12), (int)((idx_12 + 1) - idx_12));
            if ((ch_13 == "A")) {
                a_8 = (a_8 + 1);
            } else {
                if ((ch_13 == "C")) {
                    c_9 = (c_9 + 1);
                } else {
                    if ((ch_13 == "G")) {
                        g_10 = (g_10 + 1);
                    } else {
                        if ((ch_13 == "T")) {
                            t_11 = (t_11 + 1);
                        }
                    }
                }
            }
            idx_12 = (idx_12 + 1);
        }
        Console.WriteLine(_fmtTop(""));
        Console.WriteLine(_fmtTop("BASE COUNT:"));
        Console.WriteLine(_fmtTop(("    A: " + padLeft((a_8).ToString(), 3))));
        Console.WriteLine(_fmtTop(("    C: " + padLeft((c_9).ToString(), 3))));
        Console.WriteLine(_fmtTop(("    G: " + padLeft((g_10).ToString(), 3))));
        Console.WriteLine(_fmtTop(("    T: " + padLeft((t_11).ToString(), 3))));
        Console.WriteLine(_fmtTop("    ------"));
        Console.WriteLine(_fmtTop(("    Σ: " + (le_5).ToString())));
        Console.WriteLine(_fmtTop("    ======"));
    }
}
