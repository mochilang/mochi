public class Main {

    static BigRat bernoulli(int n) {
        BigRat[] a = new BigRat[]{};
        int m = 0;
        while (m <= n) {
            a = java.util.stream.Stream.concat(java.util.Arrays.stream(a), java.util.stream.Stream.of(_div(_bigrat(1, null), (_bigrat((m + 1), null))))).toArray(BigRat[]::new);
            int j = m;
            while (j >= 1) {
a[j - 1] = _mul((_bigrat(j, null)), (_sub(a[j - 1], a[j])));
                j = j - 1;
            }
            m = m + 1;
        }
        return a[0];
    }

    static java.math.BigInteger binom(int n, int k) {
        if (k < 0 || k > n) {
            return java.math.BigInteger.valueOf(0);
        }
        int kk = k;
        if (kk > n - kk) {
            kk = n - kk;
        }
        java.math.BigInteger res = java.math.BigInteger.valueOf(1);
        int i = 0;
        while (i < kk) {
            res = res.multiply((new java.math.BigInteger(String.valueOf((n - i)))));
            i = i + 1;
            res = res.divide((new java.math.BigInteger(String.valueOf(i))));
        }
        return res;
    }

    static BigRat coeff(int p, int j) {
        BigRat base = _div(_bigrat(1, null), (_bigrat((p + 1), null)));
        BigRat c = base;
        if (Math.floorMod(j, 2) == 1) {
            c = _sub(_bigrat(0, null), c);
        }
        c = _mul(c, (_bigrat(binom(p + 1, j), null)));
        c = _mul(c, bernoulli(j));
        return c;
    }

    static void main() {
        int p = 0;
        while (p < 10) {
            String line = _p(p) + " :";
            int j_1 = 0;
            while (j_1 <= p) {
                BigRat c_1 = coeff(p, j_1);
                if (!(_p(c_1).equals("0/1"))) {
                    line = line + " " + _p(c_1) + "×n";
                    int exp = p + 1 - j_1;
                    if (exp > 1) {
                        line = line + "^" + _p(exp);
                    }
                }
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            p = p + 1;
        }
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
