public class Main {
    static java.math.BigInteger[] arr1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)}));
    static java.math.BigInteger[] st1;
    static java.math.BigInteger[] arr2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)}));
    static java.math.BigInteger[] st2;
    static java.math.BigInteger[] arr3 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1)}));
    static java.math.BigInteger[] st3;
    static java.math.BigInteger[] arr4;
    static java.math.BigInteger n4;
    static java.math.BigInteger[] st4;

    static java.math.BigInteger[] build(java.math.BigInteger[] arr, java.util.function.BiFunction<java.math.BigInteger,java.math.BigInteger,java.math.BigInteger> combine) {
        java.math.BigInteger n = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger[] st_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(2).multiply(n)) < 0) {
            st_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(st_1), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
st_1[(int)(((java.math.BigInteger)(n.add(i_1))).longValue())] = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(i_1)).longValue())]));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = new java.math.BigInteger(String.valueOf(n.subtract(java.math.BigInteger.valueOf(1))));
        while (i_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
st_1[(int)(((java.math.BigInteger)(i_1)).longValue())] = new java.math.BigInteger(String.valueOf(combine.apply(new java.math.BigInteger(String.valueOf(st_1[(int)(((java.math.BigInteger)(i_1.multiply(java.math.BigInteger.valueOf(2)))).longValue())])), new java.math.BigInteger(String.valueOf(st_1[(int)(((java.math.BigInteger)(i_1.multiply(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1)))).longValue())])))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.subtract(java.math.BigInteger.valueOf(1))));
        }
        return st_1;
    }

    static void update(java.math.BigInteger[] st, java.math.BigInteger n, java.util.function.BiFunction<java.math.BigInteger,java.math.BigInteger,java.math.BigInteger> combine, java.math.BigInteger p, java.math.BigInteger v) {
        java.math.BigInteger idx = new java.math.BigInteger(String.valueOf(p.add(n)));
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(v));
        while (idx.compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            idx = new java.math.BigInteger(String.valueOf(((Number)((idx.divide(java.math.BigInteger.valueOf(2))))).intValue()));
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(combine.apply(new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(idx.multiply(java.math.BigInteger.valueOf(2)))).longValue())])), new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(idx.multiply(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1)))).longValue())])))));
        }
    }

    static java.math.BigInteger query(java.math.BigInteger[] st, java.math.BigInteger n, java.util.function.BiFunction<java.math.BigInteger,java.math.BigInteger,java.math.BigInteger> combine, java.math.BigInteger left, java.math.BigInteger right) {
        java.math.BigInteger l = new java.math.BigInteger(String.valueOf(left.add(n)));
        java.math.BigInteger r_1 = new java.math.BigInteger(String.valueOf(right.add(n)));
        java.math.BigInteger res_1 = java.math.BigInteger.valueOf(0);
        boolean has_1 = false;
        while (l.compareTo(r_1) <= 0) {
            if (l.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                if (!has_1) {
                    res_1 = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(l)).longValue())]));
                    has_1 = true;
                } else {
                    res_1 = new java.math.BigInteger(String.valueOf(combine.apply(new java.math.BigInteger(String.valueOf(res_1)), new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(l)).longValue())])))));
                }
                l = new java.math.BigInteger(String.valueOf(l.add(java.math.BigInteger.valueOf(1))));
            }
            if (r_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                if (!has_1) {
                    res_1 = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(r_1)).longValue())]));
                    has_1 = true;
                } else {
                    res_1 = new java.math.BigInteger(String.valueOf(combine.apply(new java.math.BigInteger(String.valueOf(res_1)), new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(r_1)).longValue())])))));
                }
                r_1 = new java.math.BigInteger(String.valueOf(r_1.subtract(java.math.BigInteger.valueOf(1))));
            }
            l = new java.math.BigInteger(String.valueOf(((Number)((l.divide(java.math.BigInteger.valueOf(2))))).intValue()));
            r_1 = new java.math.BigInteger(String.valueOf(((Number)((r_1.divide(java.math.BigInteger.valueOf(2))))).intValue()));
        }
        return res_1;
    }

    static java.math.BigInteger add(java.math.BigInteger a, java.math.BigInteger b) {
        return a.add(b);
    }

    static java.math.BigInteger min_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) < 0) {
            return a;
        } else {
            return b;
        }
    }

    static java.math.BigInteger max_int(java.math.BigInteger a, java.math.BigInteger b) {
        if (a.compareTo(b) > 0) {
            return a;
        } else {
            return b;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            st1 = ((java.math.BigInteger[])(build(((java.math.BigInteger[])(arr1)), Main::add)));
            System.out.println(_p(query(((java.math.BigInteger[])(st1)), new java.math.BigInteger(String.valueOf(arr1.length)), Main::add, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2))));
            st2 = ((java.math.BigInteger[])(build(((java.math.BigInteger[])(arr2)), Main::min_int)));
            System.out.println(_p(query(((java.math.BigInteger[])(st2)), new java.math.BigInteger(String.valueOf(arr2.length)), Main::min_int, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2))));
            st3 = ((java.math.BigInteger[])(build(((java.math.BigInteger[])(arr3)), Main::max_int)));
            System.out.println(_p(query(((java.math.BigInteger[])(st3)), new java.math.BigInteger(String.valueOf(arr3.length)), Main::max_int, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(2))));
            arr4 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(6)}));
            n4 = new java.math.BigInteger(String.valueOf(arr4.length));
            st4 = ((java.math.BigInteger[])(build(((java.math.BigInteger[])(arr4)), Main::add)));
            update(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())));
            update(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3));
            System.out.println(_p(query(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2))));
            System.out.println(_p(query(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1))));
            update(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(1));
            System.out.println(_p(query(((java.math.BigInteger[])(st4)), new java.math.BigInteger(String.valueOf(n4)), Main::add, java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
