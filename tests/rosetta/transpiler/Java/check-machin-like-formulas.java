public class Main {
    static java.util.Map[][] testCases;

    static BigRat br(int n, int d) {
        return _div((_bigrat(n, null)), _bigrat((_bigrat(d, null)), null));
    }

    static String format(java.util.Map<String,Integer>[] ts) {
        String s = "[";
        int i = 0;
        while (i < ts.length) {
            java.util.Map<String,Integer> t = ts[i];
            s = s + "{" + _p(((int)(t).getOrDefault("a", 0))) + " " + _p(((int)(t).getOrDefault("n", 0))) + " " + _p(((int)(t).getOrDefault("d", 0))) + "}";
            if (i < ts.length - 1) {
                s = s + " ";
            }
            i = i + 1;
        }
        return s + "]";
    }

    static BigRat tanEval(int coef, BigRat f) {
        if (coef == 1) {
            return f;
        }
        if (coef < 0) {
            return _sub(_bigrat(0, null), _bigrat((tanEval(-coef, f)), null));
        }
        int ca = coef / 2;
        int cb = coef - ca;
        BigRat a = tanEval(ca, f);
        BigRat b = tanEval(cb, f);
        return _div(_bigrat((_add(a, b)), null), _bigrat((_sub(_bigrat(1, null), _mul(a, b))), null));
    }

    static BigRat tans(java.util.Map<String,Integer>[] m) {
        if (m.length == 1) {
            java.util.Map<String,Integer> t_1 = m[0];
            return tanEval((int)(((int)(t_1).getOrDefault("a", 0))), br((int)(((int)(t_1).getOrDefault("n", 0))), (int)(((int)(t_1).getOrDefault("d", 0)))));
        }
        int half = m.length / 2;
        BigRat a_1 = tans(((java.util.Map<String,Integer>[])(java.util.Arrays.copyOfRange(m, 0, half))));
        BigRat b_1 = tans(((java.util.Map<String,Integer>[])(java.util.Arrays.copyOfRange(m, half, m.length))));
        return _div(_bigrat((_add(a_1, b_1)), null), _bigrat((_sub(_bigrat(1, null), _mul(a_1, b_1))), null));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            testCases = ((java.util.Map[][])(new java.util.Map[][]{new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 2))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 3)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 2), java.util.Map.entry("n", 1), java.util.Map.entry("d", 3))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 7)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 4), java.util.Map.entry("n", 1), java.util.Map.entry("d", 5))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 5), java.util.Map.entry("n", 1), java.util.Map.entry("d", 7))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 2), java.util.Map.entry("n", 3), java.util.Map.entry("d", 79)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 2))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 5))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 8)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 4), java.util.Map.entry("n", 1), java.util.Map.entry("d", 5))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 70))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 99)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 5), java.util.Map.entry("n", 1), java.util.Map.entry("d", 7))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 4), java.util.Map.entry("n", 1), java.util.Map.entry("d", 53))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 2), java.util.Map.entry("n", 1), java.util.Map.entry("d", 4443)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 6), java.util.Map.entry("n", 1), java.util.Map.entry("d", 8))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 2), java.util.Map.entry("n", 1), java.util.Map.entry("d", 57))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 8), java.util.Map.entry("n", 1), java.util.Map.entry("d", 10))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -1), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -4), java.util.Map.entry("n", 1), java.util.Map.entry("d", 515)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 12), java.util.Map.entry("n", 1), java.util.Map.entry("d", 18))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 8), java.util.Map.entry("n", 1), java.util.Map.entry("d", 57))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -5), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 16), java.util.Map.entry("n", 1), java.util.Map.entry("d", 21))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 3), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 4), java.util.Map.entry("n", 3), java.util.Map.entry("d", 1042)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 22), java.util.Map.entry("n", 1), java.util.Map.entry("d", 28))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 2), java.util.Map.entry("n", 1), java.util.Map.entry("d", 443))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -5), java.util.Map.entry("n", 1), java.util.Map.entry("d", 1393))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -10), java.util.Map.entry("n", 1), java.util.Map.entry("d", 11018)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 22), java.util.Map.entry("n", 1), java.util.Map.entry("d", 38))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 17), java.util.Map.entry("n", 7), java.util.Map.entry("d", 601))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 10), java.util.Map.entry("n", 7), java.util.Map.entry("d", 8149)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 44), java.util.Map.entry("n", 1), java.util.Map.entry("d", 57))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 7), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", -12), java.util.Map.entry("n", 1), java.util.Map.entry("d", 682))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 24), java.util.Map.entry("n", 1), java.util.Map.entry("d", 12943)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 88), java.util.Map.entry("n", 1), java.util.Map.entry("d", 172))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 51), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 32), java.util.Map.entry("n", 1), java.util.Map.entry("d", 682))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 44), java.util.Map.entry("n", 1), java.util.Map.entry("d", 5357))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 68), java.util.Map.entry("n", 1), java.util.Map.entry("d", 12943)))}, new java.util.Map[]{new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 88), java.util.Map.entry("n", 1), java.util.Map.entry("d", 172))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 51), java.util.Map.entry("n", 1), java.util.Map.entry("d", 239))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 32), java.util.Map.entry("n", 1), java.util.Map.entry("d", 682))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 44), java.util.Map.entry("n", 1), java.util.Map.entry("d", 5357))), new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 68), java.util.Map.entry("n", 1), java.util.Map.entry("d", 12944)))}}));
            for (java.util.Map[] ts : testCases) {
                System.out.println("tan " + String.valueOf(format(((java.util.Map<String,Integer>[])(ts)))) + " = " + _p(tans(((java.util.Map<String,Integer>[])(ts)))));
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
        if (y.num.equals(java.math.BigInteger.ZERO)) return _bigrat(java.math.BigInteger.ZERO, java.math.BigInteger.ONE);
        return _bigrat(x.num.multiply(y.den), x.den.multiply(y.num));
    }
    static java.math.BigInteger _num(Object x) { return (x instanceof BigRat) ? ((BigRat)x).num : _toBigInt(x); }
    static java.math.BigInteger _denom(Object x) { return (x instanceof BigRat) ? ((BigRat)x).den : java.math.BigInteger.ONE; }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
