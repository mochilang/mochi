public class Main {
    static Object[] testCases;
    static class Data1 {
        int a;
        int n;
        int d;
        Data1(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data2 {
        int a;
        int n;
        int d;
        Data2(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data3 {
        int a;
        int n;
        int d;
        Data3(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data4 {
        int a;
        int n;
        int d;
        Data4(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data5 {
        int a;
        int n;
        int d;
        Data5(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data6 {
        int a;
        int n;
        int d;
        Data6(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data7 {
        int a;
        int n;
        int d;
        Data7(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data8 {
        int a;
        int n;
        int d;
        Data8(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data9 {
        int a;
        int n;
        int d;
        Data9(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data10 {
        int a;
        int n;
        int d;
        Data10(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data11 {
        int a;
        int n;
        int d;
        Data11(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data12 {
        int a;
        int n;
        int d;
        Data12(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data13 {
        int a;
        int n;
        int d;
        Data13(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data14 {
        int a;
        int n;
        int d;
        Data14(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data15 {
        int a;
        int n;
        int d;
        Data15(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }

    static class Data16 {
        int a;
        int n;
        int d;
        Data16(int a, int n, int d) {
            this.a = a;
            this.n = n;
            this.d = d;
        }
        @Override public String toString() {
            return String.format("{'a': %s, 'n': %s, 'd': %s}", String.valueOf(a), String.valueOf(n), String.valueOf(d));
        }
    }


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
        testCases = ((Object[])(new Object[]{new Data1[]{new Data1(1, 1, 2), new Data1(1, 1, 3)}, new Data2[]{new Data2(2, 1, 3), new Data2(1, 1, 7)}, new Data3[]{new Data3(4, 1, 5), new Data3(-1, 1, 239)}, new Data4[]{new Data4(5, 1, 7), new Data4(2, 3, 79)}, new Data5[]{new Data5(1, 1, 2), new Data5(1, 1, 5), new Data5(1, 1, 8)}, new Data6[]{new Data6(4, 1, 5), new Data6(-1, 1, 70), new Data6(1, 1, 99)}, new Data7[]{new Data7(5, 1, 7), new Data7(4, 1, 53), new Data7(2, 1, 4443)}, new Data8[]{new Data8(6, 1, 8), new Data8(2, 1, 57), new Data8(1, 1, 239)}, new Data9[]{new Data9(8, 1, 10), new Data9(-1, 1, 239), new Data9(-4, 1, 515)}, new Data10[]{new Data10(12, 1, 18), new Data10(8, 1, 57), new Data10(-5, 1, 239)}, new Data11[]{new Data11(16, 1, 21), new Data11(3, 1, 239), new Data11(4, 3, 1042)}, new Data12[]{new Data12(22, 1, 28), new Data12(2, 1, 443), new Data12(-5, 1, 1393), new Data12(-10, 1, 11018)}, new Data13[]{new Data13(22, 1, 38), new Data13(17, 7, 601), new Data13(10, 7, 8149)}, new Data14[]{new Data14(44, 1, 57), new Data14(7, 1, 239), new Data14(-12, 1, 682), new Data14(24, 1, 12943)}, new Data15[]{new Data15(88, 1, 172), new Data15(51, 1, 239), new Data15(32, 1, 682), new Data15(44, 1, 5357), new Data15(68, 1, 12943)}, new Data16[]{new Data16(88, 1, 172), new Data16(51, 1, 239), new Data16(32, 1, 682), new Data16(44, 1, 5357), new Data16(68, 1, 12944)}}));
        for (Object ts : testCases) {
            System.out.println("tan " + String.valueOf(format(((java.util.Map<String,Integer>[])(ts)))) + " = " + _p(tans(((java.util.Map<String,Integer>[])(ts)))));
        }
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
