// Generated by Mochi 0.10.52 on 2025-07-31 08:37 UTC
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
    static double sqrtApprox(double x_0) {
        double guess_1 = x_0;
        long i_2 = 0;
        while ((i_2 < 20)) {
            guess_1 = ((guess_1 + (x_0 / guess_1)) / 2.0);
            i_2 = (i_2 + 1);
        };
        return guess_1;
    }

    static Dictionary<string, object> makeSym(long order_3, double[] elements_4) {
        return new Dictionary<string, object>{{"order", order_3}, {"ele", elements_4}};
    }

    static double[][] unpackSym(Dictionary<string, object> m_5) {
        object n_6 = ((dynamic)m_5)["order"];
        object ele_7 = ((dynamic)m_5)["ele"];
        double[][] mat_8 = new double[][]{};
        long idx_9 = 0;
        long r_10 = 0;
        while ((Convert.ToDouble(r_10) < Convert.ToDouble(n_6))) {
            double[] row_11 = new double[]{};
            long c_12 = 0;
            while ((c_12 <= r_10)) {
                row_11 = (Enumerable.ToArray(Enumerable.Append(row_11, ((dynamic)ele_7)[idx_9])));
                idx_9 = (idx_9 + 1);
                c_12 = (c_12 + 1);
            }
            while ((Convert.ToDouble(c_12) < Convert.ToDouble(n_6))) {
                row_11 = (Enumerable.ToArray(Enumerable.Append(row_11, 0.0)));
                c_12 = (c_12 + 1);
            }
            mat_8 = (Enumerable.ToArray(Enumerable.Append(mat_8, row_11)));
            r_10 = (r_10 + 1);
        };
        r_10 = 0;
        while ((Convert.ToDouble(r_10) < Convert.ToDouble(n_6))) {
            long c_13 = (r_10 + 1);
            while ((Convert.ToDouble(c_13) < Convert.ToDouble(n_6))) {
                mat_8[(int)(r_10)][c_13] = mat_8[(int)(c_13)][(int)(r_10)];
                c_13 = (c_13 + 1);
            }
            r_10 = (r_10 + 1);
        };
        return mat_8;
    }

    static void printMat(double[][] m_14) {
        long i_15 = 0;
        while ((Convert.ToDouble(i_15) < Convert.ToDouble(m_14.Length))) {
            string line_16 = "";
            long j_17 = 0;
            while ((Convert.ToDouble(j_17) < Convert.ToDouble(m_14[(int)(i_15)].Length))) {
                line_16 = (line_16 + _fmt(m_14[(int)(i_15)][(int)(j_17)]));
                if ((j_17 < (((dynamic)(m_14[(int)(i_15)].Length)) - ((dynamic)(1))))) {
                    line_16 = (line_16 + " ");
                }
                j_17 = (j_17 + 1);
            }
            Console.WriteLine(_fmtTop(line_16));
            i_15 = (i_15 + 1);
        };
    }

    static void printSym(Dictionary<string, object> m_18) {
        printMat(unpackSym(m_18));
    }

    static void printLower(Dictionary<string, object> m_19) {
        object n_20 = ((dynamic)m_19)["order"];
        object ele_21 = ((dynamic)m_19)["ele"];
        double[][] mat_22 = new double[][]{};
        long idx_23 = 0;
        long r_24 = 0;
        while ((Convert.ToDouble(r_24) < Convert.ToDouble(n_20))) {
            double[] row_25 = new double[]{};
            long c_26 = 0;
            while ((c_26 <= r_24)) {
                row_25 = (Enumerable.ToArray(Enumerable.Append(row_25, ((dynamic)ele_21)[idx_23])));
                idx_23 = (idx_23 + 1);
                c_26 = (c_26 + 1);
            }
            while ((Convert.ToDouble(c_26) < Convert.ToDouble(n_20))) {
                row_25 = (Enumerable.ToArray(Enumerable.Append(row_25, 0.0)));
                c_26 = (c_26 + 1);
            }
            mat_22 = (Enumerable.ToArray(Enumerable.Append(mat_22, row_25)));
            r_24 = (r_24 + 1);
        };
        printMat(mat_22);
    }

    static Dictionary<string, object> choleskyLower(Dictionary<string, object> a_27) {
        object n_28 = ((dynamic)a_27)["order"];
        object ae_29 = ((dynamic)a_27)["ele"];
        double[] le_30 = new double[]{};
        long idx_31 = 0;
        while ((Convert.ToDouble(idx_31) < Convert.ToDouble(Convert.ToString(ae_29).Length))) {
            le_30 = (Enumerable.ToArray(Enumerable.Append(le_30, 0.0)));
            idx_31 = (idx_31 + 1);
        };
        long row_32 = 1;
        long col_33 = 1;
        long dr_34 = 0;
        long dc_35 = 0;
        long i_36 = 0;
        while ((Convert.ToDouble(i_36) < Convert.ToDouble(Convert.ToString(ae_29).Length))) {
            var e_37 = ((dynamic)ae_29)[i_36];
            if ((i_36 < dr_34)) {
                double d_38 = ((((dynamic)(e_37)) - ((dynamic)(le_30[(int)(i_36)]))) / le_30[(int)(dc_35)]);
                le_30[i_36] = d_38;
                long ci_39 = col_33;
                long cx_40 = dc_35;
                long j_41 = (i_36 + 1);
                while ((j_41 <= dr_34)) {
                    cx_40 = (cx_40 + ci_39);
                    ci_39 = (ci_39 + 1);
                    le_30[j_41] = (le_30[(int)(j_41)] + (d_38 * le_30[(int)(cx_40)]));
                    j_41 = (j_41 + 1);
                }
                col_33 = (col_33 + 1);
                dc_35 = (dc_35 + col_33);
            } else {
                le_30[i_36] = sqrtApprox((((dynamic)(e_37)) - ((dynamic)(le_30[(int)(i_36)]))));
                row_32 = (row_32 + 1);
                dr_34 = (dr_34 + row_32);
                col_33 = 1;
                dc_35 = 0;
            }
            i_36 = (i_36 + 1);
        };
        return new Dictionary<string, object>{{"order", n_28}, {"ele", le_30}};
    }

    static void demo(Dictionary<string, object> a_42) {
        Console.WriteLine(_fmtTop("A:"));
        printSym(a_42);
        Console.WriteLine(_fmtTop("L:"));
        Dictionary<string, object> l_43 = choleskyLower(a_42);
        printLower(l_43);
    }

    static void Main() {
        {
            var __memStart = _mem();
            var __start = _now();
            demo(makeSym(3, new double[]{25.0, 15.0, 18.0, -5.0, 0.0, 11.0}));
            demo(makeSym(4, new double[]{18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0}));
            var __end = _now();
            var __memEnd = _mem();
            var __dur = (__end - __start);
            var __memDiff = __memEnd - __memStart;
            Console.WriteLine(JsonSerializer.Serialize(new SortedDictionary<string, object>{{"name", "main"}, {"duration_us", __dur}, {"memory_bytes", __memDiff}}, new JsonSerializerOptions{ WriteIndented = true }));
        }
    }
}
