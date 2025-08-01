// Generated by Mochi 0.10.52 on 2025-07-31 14:19 +0700
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
    class BigRat {
        public BigInteger num;
        public BigInteger den;
        public BigRat(BigInteger n, BigInteger d) {
            if (d.Sign < 0) { n = BigInteger.Negate(n); d = BigInteger.Negate(d); }
            var g = BigInteger.GreatestCommonDivisor(n, d);
            num = n / g; den = d / g;
        }
        public override string ToString() => den.Equals(BigInteger.One) ? num.ToString() : num.ToString()+"/"+den.ToString();
    }
    static BigInteger _toBigInt(object x) {
        if (x is BigInteger bi) return bi;
        if (x is BigRat br) return br.num;
        if (x is int ii) return new BigInteger(ii);
        if (x is long ll) return new BigInteger(ll);
        if (x is double dd) return new BigInteger((long)dd);
        if (x is string ss) return BigInteger.Parse(ss);
        return BigInteger.Zero;
    }
    static BigRat _bigrat(object n, object d = null) {
        var nn = _toBigInt(n);
        var dd = d == null ? BigInteger.One : _toBigInt(d);
        return new BigRat(nn, dd);
    }
    static BigRat _add(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den + y.num*x.den, x.den*y.den); }
    static BigRat _sub(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den - y.num*x.den, x.den*y.den); }
    static BigRat _mul(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.num, x.den*y.den); }
    static BigRat _div(object a, object b) { var x=_bigrat(a, null); var y=_bigrat(b, null); return new BigRat(x.num*y.den, x.den*y.num); }
    static BigRat _neg(object a) { var x=_bigrat(a, null); return new BigRat(BigInteger.Negate(x.num), x.den); }
    static BigInteger _num(object x) { return x is BigRat br ? br.num : _toBigInt(x); }
    static BigInteger _denom(object x) { return x is BigRat br ? br.den : BigInteger.One; }
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
    static BigRat bigrat(long a_0, long b_1) {
        return _div(_bigrat(a_0), _bigrat(b_1));
    }

    static BigRat[] calkinWilf(long n_2) {
        BigRat[] seq_3 = new BigRat[]{};
        seq_3 = (Enumerable.ToArray(Enumerable.Append(seq_3, bigrat(1, 1))));
        long i_4 = 1;
        while ((i_4 < n_2)) {
            BigRat prev_5 = seq_3[(int)((i_4 - 1))];
            BigInteger a_6 = _num(prev_5);
            BigInteger b_7 = _denom(prev_5);
            BigInteger f_8 = (a_6 / b_7);
            BigRat t_9 = bigrat(f_8, 1);
            t_9 = _mul(t_9, _bigrat(2));
            t_9 = _sub(t_9, prev_5);
            t_9 = _add(t_9, _bigrat(1));
            t_9 = _div(_bigrat(1), t_9);
            seq_3 = (Enumerable.ToArray(Enumerable.Append(seq_3, t_9)));
            i_4 = (i_4 + 1);
        };
        return seq_3;
    }

    static long[] toContinued(BigRat r_10) {
        BigInteger a_11 = _num(r_10);
        BigInteger b_12 = _denom(r_10);
        long[] res_13 = new long[]{};
        while (true) {
            res_13 = (Enumerable.ToArray(Enumerable.Append(res_13, (int)((a_11 / b_12)))));
            BigInteger t_14 = (a_11 % b_12);
            a_11 = b_12;
            b_12 = t_14;
            if ((a_11 == 1)) {
                break;
            }
        };
        if (((((dynamic)(res_13.Length)) % ((dynamic)(2))) == 0)) {
            res_13[(((dynamic)(res_13.Length)) - ((dynamic)(1)))] = (res_13[(int)((((dynamic)(res_13.Length)) - ((dynamic)(1))))] - 1);
            res_13 = (Enumerable.ToArray(Enumerable.Append(res_13, 1)));
        };
        return res_13;
    }

    static long termNumber(long[] cf_15) {
        string b_16 = "";
        string d_17 = "1";
        foreach (var n_18 in cf_15) {
            b_16 = (repeat(d_17, n_18) + b_16);
            if ((d_17 == "1")) {
                d_17 = "0";
            } else {
                d_17 = "1";
            }
        };
        return Convert.ToInt64(b_16, 2);
    }

    static string commatize(long n_19) {
        string s_20 = _fmt(n_19);
        string out_21 = "";
        long i_22 = 0;
        long cnt_23 = 0;
        bool neg_24 = false;
        if ((s_20.Substring((int)(0), (int)(1 - 0)) == "-")) {
            neg_24 = true;
            s_20 = s_20.Substring((int)(1), (int)(s_20.Length - 1));
        };
        i_22 = (((dynamic)(s_20.Length)) - ((dynamic)(1)));
        while ((i_22 >= 0)) {
            out_21 = (s_20.Substring((int)(i_22), (int)((i_22 + 1) - i_22)) + out_21);
            cnt_23 = (cnt_23 + 1);
            if (((cnt_23 == 3) && (i_22 != 0))) {
                out_21 = ("," + out_21);
                cnt_23 = 0;
            }
            i_22 = (i_22 - 1);
        };
        if (neg_24) {
            out_21 = ("-" + out_21);
        };
        return out_21;
    }

    static void main() {
        BigRat[] cw_25 = calkinWilf(20);
        Console.WriteLine(_fmtTop("The first 20 terms of the Calkin-Wilf sequnence are:"));
        long i_26 = 0;
        while ((i_26 < 20)) {
            BigRat r_27 = cw_25[(int)(i_26)];
            string s_28 = _fmt(_num(r_27));
            if ((_denom(r_27) != 1)) {
                s_28 = ((s_28 + "/") + _fmt(_denom(r_27)));
            }
            Console.WriteLine(_fmtTop(((((dynamic)((i_26 + Convert.ToInt64(1)).PadLeft(2, ' '))) + ((dynamic)(": "))) + s_28)));
            i_26 = (i_26 + 1);
        };
        BigRat r_29 = bigrat(83116, 51639);
        long[] cf_30 = toContinued(r_29);
        long tn_31 = termNumber(cf_30);
        Console.WriteLine(_fmtTop((((((("" + _fmt(_num(r_29))) + "/") + _fmt(_denom(r_29))) + " is the ") + commatize(tn_31)) + "th term of the sequence.")));
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
