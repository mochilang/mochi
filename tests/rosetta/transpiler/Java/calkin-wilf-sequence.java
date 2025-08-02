public class Main {

    static BigRat bigrat(int a, int b) {
        return _div((_bigrat(a, null)), (_bigrat(b, null)));
    }

    static BigRat[] calkinWilf(int n) {
        BigRat[] seq = new BigRat[]{};
        seq = java.util.stream.Stream.concat(java.util.Arrays.stream(seq), java.util.stream.Stream.of(bigrat(1, 1))).toArray(BigRat[]::new);
        int i = 1;
        while (i < n) {
            BigRat prev = seq[i - 1];
            java.math.BigInteger a = new java.math.BigInteger(String.valueOf(_num(prev)));
            java.math.BigInteger b = new java.math.BigInteger(String.valueOf(_denom(prev)));
            java.math.BigInteger f = new java.math.BigInteger(String.valueOf(a / b));
            BigRat t = bigrat(f, 1);
            t = _mul(t, (_bigrat(2, null)));
            t = _sub(t, prev);
            t = _add(t, (_bigrat(1, null)));
            t = _div((_bigrat(1, null)), t);
            seq = java.util.stream.Stream.concat(java.util.Arrays.stream(seq), java.util.stream.Stream.of(t)).toArray(BigRat[]::new);
            i = i + 1;
        }
        return seq;
    }

    static int[] toContinued(BigRat r) {
        java.math.BigInteger a_1 = new java.math.BigInteger(String.valueOf(_num(r)));
        java.math.BigInteger b_1 = new java.math.BigInteger(String.valueOf(_denom(r)));
        int[] res = new int[]{};
        while (true) {
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(((Number)((a_1 / b_1))).intValue())).toArray();
            int t_1 = Math.floorMod(a_1, b_1);
            a_1 = new java.math.BigInteger(String.valueOf(b_1));
            b_1 = new java.math.BigInteger(String.valueOf(t_1));
            if (a_1 == 1) {
                break;
            }
        }
        if (Math.floorMod(res.length, 2) == 0) {
res[res.length - 1] = res[res.length - 1] - 1;
            res = java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(1)).toArray();
        }
        return res;
    }

    static int termNumber(int[] cf) {
        String b_2 = "";
        String d = "1";
        for (int n : cf) {
            b_2 = (String)(repeat(d, n)) + b_2;
            if ((d.equals("1"))) {
                d = "0";
            } else {
                d = "1";
            }
        }
        return Integer.parseInt(b_2, 2);
    }

    static String commatize(int n) {
        String s = _p(n);
        String out = "";
        int i_1 = 0;
        int cnt = 0;
        boolean neg = false;
        if ((s.substring(0, 1).equals("-"))) {
            neg = true;
            s = s.substring(1, _runeLen(s));
        }
        i_1 = _runeLen(s) - 1;
        while (i_1 >= 0) {
            out = s.substring(i_1, i_1 + 1) + out;
            cnt = cnt + 1;
            if (cnt == 3 && i_1 != 0) {
                out = "," + out;
                cnt = 0;
            }
            i_1 = i_1 - 1;
        }
        if (neg) {
            out = "-" + out;
        }
        return out;
    }

    static void main() {
        BigRat[] cw = calkinWilf(20);
        System.out.println("The first 20 terms of the Calkin-Wilf sequnence are:");
        int i_2 = 0;
        while (i_2 < 20) {
            BigRat r = cw[i_2];
            String s_1 = _p(_num(r));
            if (((Number)(_denom(r))).intValue() != 1) {
                s_1 = s_1 + "/" + _p(_denom(r));
            }
            System.out.println((String)(_padStart((i_2 + ((Number)(1)).intValue()), 2, " ")) + ": " + s_1);
            i_2 = i_2 + 1;
        }
        BigRat r_1 = bigrat(83116, 51639);
        int[] cf = toContinued(r_1);
        int tn = termNumber(cf);
        System.out.println("" + _p(_num(r_1)) + "/" + _p(_denom(r_1)) + " is the " + String.valueOf(commatize(tn)) + "th term of the sequence.");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
    }

    static class BigRat {
        java.math.BigInteger num;
        java.math.BigInteger den;
        BigRat(java.math.BigInteger n, java.math.BigInteger d) {
            if (d.signum() < 0) { n = n.negate(); d = d.negate(); }
            java.math.BigInteger g = n.gcd(d);
            num = n.divide(g); den = d.divide(g);
        }
        public String toString() { return den.equals(java.math.BigInteger.ONE) ? num.toString() : num.toString()+"/"+den.toString(); }
    }
    static java.math.BigInteger _toBigInt(Object x) {
        if (x instanceof java.math.BigInteger) return (java.math.BigInteger)x;
        if (x instanceof BigRat) return ((BigRat)x).num;
        if (x instanceof Integer) return java.math.BigInteger.valueOf((Integer)x);
        if (x instanceof Long) return java.math.BigInteger.valueOf((Long)x);
        if (x instanceof Double) return java.math.BigInteger.valueOf(((Double)x).longValue());
        if (x instanceof String) return new java.math.BigInteger((String)x);
        return java.math.BigInteger.ZERO;
    }
    static BigRat _bigrat(Object n, Object d) {
        java.math.BigInteger nn = _toBigInt(n);
        java.math.BigInteger dd = d == null ? java.math.BigInteger.ONE : _toBigInt(d);
        return new BigRat(nn, dd);
    }
    static BigRat _bigrat(Object n) { return _bigrat(n, null); }
    static BigRat _add(Object a, Object b) {
        BigRat x = _bigrat(a); BigRat y = _bigrat(b);
        java.math.BigInteger n = x.num.multiply(y.den).add(y.num.multiply(x.den));
        java.math.BigInteger d = x.den.multiply(y.den);
        return _bigrat(n, d);
    }
    static BigRat _sub(Object a, Object b) {
        BigRat x = _bigrat(a); BigRat y = _bigrat(b);
        java.math.BigInteger n = x.num.multiply(y.den).subtract(y.num.multiply(x.den));
        java.math.BigInteger d = x.den.multiply(y.den);
        return _bigrat(n, d);
    }
    static BigRat _mul(Object a, Object b) {
        BigRat x = _bigrat(a); BigRat y = _bigrat(b);
        return _bigrat(x.num.multiply(y.num), x.den.multiply(y.den));
    }
    static BigRat _div(Object a, Object b) {
        BigRat x = _bigrat(a); BigRat y = _bigrat(b);
        return _bigrat(x.num.multiply(y.den), x.den.multiply(y.num));
    }
    static java.math.BigInteger _num(Object x) { return (x instanceof BigRat) ? ((BigRat)x).num : _toBigInt(x); }
    static java.math.BigInteger _denom(Object x) { return (x instanceof BigRat) ? ((BigRat)x).den : java.math.BigInteger.ONE; }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
