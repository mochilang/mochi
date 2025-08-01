// Generated by Mochi 0.10.52 on 2025-07-31 10:42 +0700
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Numerics;
using System.Collections;

class Point {
    public double x;
    public double y;
    public override string ToString() => $"Point {{x = {x.ToString("0.0")}, y = {y.ToString("0.0")}}}";
}
class QuadSpline {
    public double c0;
    public double c1;
    public double c2;
    public override string ToString() => $"QuadSpline {{c0 = {c0.ToString("0.0")}, c1 = {c1.ToString("0.0")}, c2 = {c2.ToString("0.0")}}}";
}
class QuadCurve {
    public QuadSpline x;
    public QuadSpline y;
    public override string ToString() => $"QuadCurve {{x = {x}, y = {y}}}";
}
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
    static double absf(double x_0) {
        if ((x_0 < 0.0)) {
            return -x_0;
        };
        return x_0;
    }

    static double maxf(double a_1, double b_2) {
        if ((a_1 > b_2)) {
            return a_1;
        };
        return b_2;
    }

    static double minf(double a_3, double b_4) {
        if ((a_3 < b_4)) {
            return a_3;
        };
        return b_4;
    }

    static double max3(double a_5, double b_6, double c_7) {
        double m_8 = a_5;
        if ((b_6 > m_8)) {
            m_8 = b_6;
        };
        if ((c_7 > m_8)) {
            m_8 = c_7;
        };
        return m_8;
    }

    static double min3(double a_9, double b_10, double c_11) {
        double m_12 = a_9;
        if ((b_10 < m_12)) {
            m_12 = b_10;
        };
        if ((c_11 < m_12)) {
            m_12 = c_11;
        };
        return m_12;
    }

    static QuadSpline[] subdivideQuadSpline(QuadSpline q_13, double t_14) {
        double s_15 = (1.0 - t_14);
        QuadSpline u_16 = new QuadSpline{c0 = q_13.c0, c1 = 0.0, c2 = 0.0};
        QuadSpline v_17 = new QuadSpline{c0 = 0.0, c1 = 0.0, c2 = q_13.c2};
        u_16.c1 = ((s_15 * q_13.c0) + (t_14 * q_13.c1));
        v_17.c1 = ((s_15 * q_13.c1) + (t_14 * q_13.c2));
        u_16.c2 = ((s_15 * u_16.c1) + (t_14 * v_17.c1));
        v_17.c0 = u_16.c2;
        return new QuadSpline[]{u_16, v_17};
    }

    static QuadCurve[] subdivideQuadCurve(QuadCurve q_18, double t_19) {
        QuadSpline[] xs_20 = subdivideQuadSpline(q_18.x, t_19);
        QuadSpline[] ys_21 = subdivideQuadSpline(q_18.y, t_19);
        QuadCurve u_22 = new QuadCurve{x = xs_20[(int)(0)], y = ys_21[(int)(0)]};
        QuadCurve v_23 = new QuadCurve{x = xs_20[(int)(1)], y = ys_21[(int)(1)]};
        return new QuadCurve[]{u_22, v_23};
    }

    static bool rectsOverlap(double xa0_24, double ya0_25, double xa1_26, double ya1_27, double xb0_28, double yb0_29, double xb1_30, double yb1_31) {
        return ((((xb0_28 <= xa1_26) && (xa0_24 <= xb1_30)) && (yb0_29 <= ya1_27)) && (ya0_25 <= yb1_31));
    }

    static Dictionary<string, object> testIntersect(QuadCurve p_32, QuadCurve q_33, double tol_34) {
        double pxmin_35 = min3(p_32.x.c0, p_32.x.c1, p_32.x.c2);
        double pymin_36 = min3(p_32.y.c0, p_32.y.c1, p_32.y.c2);
        double pxmax_37 = max3(p_32.x.c0, p_32.x.c1, p_32.x.c2);
        double pymax_38 = max3(p_32.y.c0, p_32.y.c1, p_32.y.c2);
        double qxmin_39 = min3(q_33.x.c0, q_33.x.c1, q_33.x.c2);
        double qymin_40 = min3(q_33.y.c0, q_33.y.c1, q_33.y.c2);
        double qxmax_41 = max3(q_33.x.c0, q_33.x.c1, q_33.x.c2);
        double qymax_42 = max3(q_33.y.c0, q_33.y.c1, q_33.y.c2);
        bool exclude_43 = true;
        bool accept_44 = false;
        Point inter_45 = new Point{x = 0.0, y = 0.0};
        if (rectsOverlap(pxmin_35, pymin_36, pxmax_37, pymax_38, qxmin_39, qymin_40, qxmax_41, qymax_42)) {
            exclude_43 = false;
            double xmin_46 = maxf(pxmin_35, qxmin_39);
            double xmax_47 = minf(pxmax_37, qxmax_41);
            if (((xmax_47 - xmin_46) <= tol_34)) {
                double ymin_48 = maxf(pymin_36, qymin_40);
                double ymax_49 = minf(pymax_38, qymax_42);
                if (((ymax_49 - ymin_48) <= tol_34)) {
                    accept_44 = true;
                    inter_45.x = (0.5 * (xmin_46 + xmax_47));
                    inter_45.y = (0.5 * (ymin_48 + ymax_49));
                }
            }
        };
        return new Dictionary<string, object>{{"exclude", exclude_43}, {"accept", accept_44}, {"intersect", inter_45}};
    }

    static bool seemsToBeDuplicate(Point[] pts_50, Point xy_51, double spacing_52) {
        long i_53 = 0;
        while ((string.Compare(Convert.ToString(i_53), Convert.ToString(pts_50.Length)) < 0)) {
            Point pt_54 = pts_50[(int)(i_53)];
            if (((absf((pt_54.x - xy_51.x)) < spacing_52) && (absf((pt_54.y - xy_51.y)) < spacing_52))) {
                return true;
            }
            i_53 = (i_53 + 1);
        };
        return false;
    }

    static Point[] findIntersects(QuadCurve p_55, QuadCurve q_56, double tol_57, double spacing_58) {
        Point[] inters_59 = new Point[]{};
        Dictionary<string, QuadCurve>[] workload_60 = new Dictionary<string, QuadCurve>[]{new Dictionary<string, QuadCurve>{{"p", p_55}, {"q", q_56}}};
        while ((string.Compare(Convert.ToString(workload_60.Length), Convert.ToString(0)) > 0)) {
            long idx_61 = (((dynamic)(workload_60.Length)) - ((dynamic)(1)));
            Dictionary<string, QuadCurve> work_62 = workload_60[(int)(idx_61)];
            workload_60 = workload_60.Skip((int)(0)).Take((int)((idx_61 - 0))).ToArray();
            Dictionary<string, object> res_63 = testIntersect((work_62.ContainsKey("p") ? work_62["p"] : null), (work_62.ContainsKey("q") ? work_62["q"] : null), tol_57);
            object excl_64 = ((dynamic)res_63)["exclude"];
            object acc_65 = ((dynamic)res_63)["accept"];
            object inter_66 = ((dynamic)res_63)["intersect"];
            if (acc_65) {
                if ((!seemsToBeDuplicate(inters_59, inter_66, spacing_58))) {
                    inters_59 = (Enumerable.ToArray(Enumerable.Append(inters_59, inter_66)));
                }
            } else {
                if ((!Convert.ToBoolean(excl_64))) {
                    QuadCurve[] ps_67 = subdivideQuadCurve((work_62.ContainsKey("p") ? work_62["p"] : null), 0.5);
                    QuadCurve[] qs_68 = subdivideQuadCurve((work_62.ContainsKey("q") ? work_62["q"] : null), 0.5);
                    QuadCurve p0_69 = ps_67[(int)(0)];
                    QuadCurve p1_70 = ps_67[(int)(1)];
                    QuadCurve q0_71 = qs_68[(int)(0)];
                    QuadCurve q1_72 = qs_68[(int)(1)];
                    workload_60 = (Enumerable.ToArray(Enumerable.Append(workload_60, new Dictionary<string, QuadCurve>{{"p", p0_69}, {"q", q0_71}})));
                    workload_60 = (Enumerable.ToArray(Enumerable.Append(workload_60, new Dictionary<string, QuadCurve>{{"p", p0_69}, {"q", q1_72}})));
                    workload_60 = (Enumerable.ToArray(Enumerable.Append(workload_60, new Dictionary<string, QuadCurve>{{"p", p1_70}, {"q", q0_71}})));
                    workload_60 = (Enumerable.ToArray(Enumerable.Append(workload_60, new Dictionary<string, QuadCurve>{{"p", p1_70}, {"q", q1_72}})));
                }
            }
        };
        return inters_59;
    }

    static void main() {
        QuadCurve p_73 = new QuadCurve{x = new QuadSpline{c0 = -1.0, c1 = 0.0, c2 = 1.0}, y = new QuadSpline{c0 = 0.0, c1 = 10.0, c2 = 0.0}};
        QuadCurve q_74 = new QuadCurve{x = new QuadSpline{c0 = 2.0, c1 = -8.0, c2 = 2.0}, y = new QuadSpline{c0 = 1.0, c1 = 2.0, c2 = 3.0}};
        double tol_75 = 1e-07;
        double spacing_76 = (tol_75 * 10.0);
        Point[] inters_77 = findIntersects(p_73, q_74, tol_75, spacing_76);
        long i_78 = 0;
        while ((string.Compare(Convert.ToString(i_78), Convert.ToString(inters_77.Length)) < 0)) {
            Point pt_79 = inters_77[(int)(i_78)];
            Console.WriteLine(_fmtTop((((("(" + (pt_79.x).ToString()) + ", ") + (pt_79.y).ToString()) + ")")));
            i_78 = (i_78 + 1);
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
