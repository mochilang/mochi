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
        if (n != 1) {
            return a[0];
        }
        return _sub(_bigrat(0, null), a[0]);
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

    static BigRat[] faulhaberRow(int p) {
        BigRat[] coeffs = new BigRat[]{};
        int i = 0;
        while (i <= p) {
            coeffs = java.util.stream.Stream.concat(java.util.Arrays.stream(coeffs), java.util.stream.Stream.of(_bigrat(0, null))).toArray(BigRat[]::new);
            i = i + 1;
        }
        int j = 0;
        int sign = -1;
        while (j <= p) {
            sign = -sign;
            BigRat c = _div(_bigrat(1, null), (_bigrat((p + 1), null)));
            if (sign < 0) {
                c = _sub(_bigrat(0, null), c);
            }
            c = _mul(c, (_bigrat(binom(p + 1, j), null)));
            c = _mul(c, bernoulli(j));
coeffs[p - j] = c;
            j = j + 1;
        }
        return coeffs;
    }

    static String ratStr(BigRat r) {
        String s = String.valueOf(r);
        if (endsWith(s, "/1")) {
            return _substr(s, 0, _runeLen(s) - 2);
        }
        return s;
    }

    static boolean endsWith(String s, String suf) {
        if (_runeLen(s) < _runeLen(suf)) {
            return false;
        }
        return (_substr(s, _runeLen(s) - _runeLen(suf), _runeLen(s)).equals(suf));
    }

    static void main() {
        int p = 0;
        while (p < 10) {
            BigRat[] row = faulhaberRow(p);
            String line = "";
            int idx = 0;
            while (idx < row.length) {
                line = line + String.valueOf(_padStart(ratStr(row[idx]), 5, " "));
                if (idx < row.length - 1) {
                    line = line + "  ";
                }
                idx = idx + 1;
            }
            System.out.println(line);
            p = p + 1;
        }
        System.out.println("");
        int k = 17;
        BigRat[] coeffs = faulhaberRow(k);
        BigRat nn = _bigrat(1000, null);
        BigRat np = _bigrat(1, null);
        BigRat sum = _bigrat(0, null);
        int i = 0;
        while (i < coeffs.length) {
            np = _mul(np, nn);
            sum = _add(sum, _mul(coeffs[i], np));
            i = i + 1;
        }
        System.out.println(ratStr(sum));
    }
    public static void main(String[] args) {
        main();
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

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }
}
