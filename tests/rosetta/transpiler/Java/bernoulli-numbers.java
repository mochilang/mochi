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

    static String padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) {
            out = String.valueOf(pad + out);
        }
        return out;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (int i = 0; i < 61; i++) {
                BigRat b = bernoulli(i);
                if (((Number)(b.num)).intValue() != 0) {
                    String numStr = String.valueOf(b.num);
                    String denStr = String.valueOf(b.den);
                    System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf("B(" + _padStart(String.valueOf(i), 2, " ")) + ") =") + _padStart(numStr, 45, " ")) + "/") + denStr);
                }
            }
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
            num = n.divide(g);
            den = d.divide(g);
        }
        BigRat add(BigRat o) { return new BigRat(num.multiply(o.den).add(o.num.multiply(den)), den.multiply(o.den)); }
        BigRat sub(BigRat o) { return new BigRat(num.multiply(o.den).subtract(o.num.multiply(den)), den.multiply(o.den)); }
        BigRat mul(BigRat o) { return new BigRat(num.multiply(o.num), den.multiply(o.den)); }
        BigRat div(BigRat o) { return new BigRat(num.multiply(o.den), den.multiply(o.num)); }
    }
    static java.math.BigInteger toBigInt(Object v) {
        if (v instanceof java.math.BigInteger) return (java.math.BigInteger)v;
        if (v instanceof Integer) return java.math.BigInteger.valueOf(((Integer)v).longValue());
        if (v instanceof Long) return java.math.BigInteger.valueOf(((Long)v).longValue());
        if (v instanceof BigRat) return ((BigRat)v).num;
        if (v instanceof String) return new java.math.BigInteger(v.toString());
        if (v instanceof Number) return java.math.BigInteger.valueOf(((Number)v).longValue());
        return java.math.BigInteger.ZERO;
    }
    static BigRat _bigrat(Object n, Object d) {
        java.math.BigInteger nn = toBigInt(n);
        java.math.BigInteger dd = d == null ? java.math.BigInteger.ONE : toBigInt(d);
        return new BigRat(nn, dd);
    }
    static java.math.BigInteger num(BigRat r) { return r.num; }
    static java.math.BigInteger denom(BigRat r) { return r.den; }
    static BigRat _add(BigRat a, BigRat b) { return a.add(b); }
    static BigRat _sub(BigRat a, BigRat b) { return a.sub(b); }
    static BigRat _mul(BigRat a, BigRat b) { return a.mul(b); }
    static BigRat _div(BigRat a, BigRat b) { return a.div(b); }

    static String _padStart(String s, int width, String pad) {
        String out = s;
        while (out.length() < width) { out = pad + out; }
        return out;
    }
}
