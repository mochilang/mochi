public class Main {
    static java.math.BigInteger[][] st1;
    static java.math.BigInteger[][] st2;

    static java.math.BigInteger pow2(java.math.BigInteger n) {
        java.math.BigInteger result = java.math.BigInteger.valueOf(1);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(n) < 0) {
            result = new java.math.BigInteger(String.valueOf(result.multiply(java.math.BigInteger.valueOf(2))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }

    static java.math.BigInteger int_log2(java.math.BigInteger n) {
        java.math.BigInteger v = new java.math.BigInteger(String.valueOf(n));
        java.math.BigInteger res_1 = java.math.BigInteger.valueOf(0);
        while (v.compareTo(java.math.BigInteger.valueOf(1)) > 0) {
            v = new java.math.BigInteger(String.valueOf(v.divide(java.math.BigInteger.valueOf(2))));
            res_1 = new java.math.BigInteger(String.valueOf(res_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res_1;
    }

    static java.math.BigInteger[][] build_sparse_table(java.math.BigInteger[] number_list) {
        if (new java.math.BigInteger(String.valueOf(number_list.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("empty number list not allowed"));
        }
        java.math.BigInteger length_1 = new java.math.BigInteger(String.valueOf(number_list.length));
        java.math.BigInteger row_1 = new java.math.BigInteger(String.valueOf(int_log2(new java.math.BigInteger(String.valueOf(length_1))).add(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger[][] sparse_table_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (j_1.compareTo(row_1) < 0) {
            java.math.BigInteger[] inner_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
            while (i_3.compareTo(length_1) < 0) {
                inner_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(inner_1), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
            sparse_table_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(sparse_table_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{inner_1})).toArray(java.math.BigInteger[][]::new)));
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(length_1) < 0) {
sparse_table_1[(int)(0L)][(int)(((java.math.BigInteger)(i_5)).longValue())] = new java.math.BigInteger(String.valueOf(number_list[(int)(((java.math.BigInteger)(i_5)).longValue())]));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        j_1 = java.math.BigInteger.valueOf(1);
        while (pow2(new java.math.BigInteger(String.valueOf(j_1))).compareTo(length_1) <= 0) {
            i_5 = java.math.BigInteger.valueOf(0);
            while (i_5.add(pow2(new java.math.BigInteger(String.valueOf(j_1)))).subtract(java.math.BigInteger.valueOf(1)).compareTo(length_1) < 0) {
                java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(sparse_table_1[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][(int)(((java.math.BigInteger)(i_5.add(pow2(new java.math.BigInteger(String.valueOf(j_1.subtract(java.math.BigInteger.valueOf(1)))))))).longValue())]));
                java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(sparse_table_1[(int)(((java.math.BigInteger)(j_1.subtract(java.math.BigInteger.valueOf(1)))).longValue())][(int)(((java.math.BigInteger)(i_5)).longValue())]));
                if (left_1.compareTo(right_1) < 0) {
sparse_table_1[(int)(((java.math.BigInteger)(j_1)).longValue())][(int)(((java.math.BigInteger)(i_5)).longValue())] = new java.math.BigInteger(String.valueOf(left_1));
                } else {
sparse_table_1[(int)(((java.math.BigInteger)(j_1)).longValue())][(int)(((java.math.BigInteger)(i_5)).longValue())] = new java.math.BigInteger(String.valueOf(right_1));
                }
                i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
            }
            j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
        }
        return sparse_table_1;
    }

    static java.math.BigInteger query(java.math.BigInteger[][] sparse_table, java.math.BigInteger left_bound, java.math.BigInteger right_bound) {
        if (left_bound.compareTo(java.math.BigInteger.valueOf(0)) < 0 || right_bound.compareTo(new java.math.BigInteger(String.valueOf(sparse_table[(int)(0L)].length))) >= 0) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        java.math.BigInteger interval_1 = new java.math.BigInteger(String.valueOf(right_bound.subtract(left_bound).add(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(int_log2(new java.math.BigInteger(String.valueOf(interval_1)))));
        java.math.BigInteger val1_1 = new java.math.BigInteger(String.valueOf(sparse_table[(int)(((java.math.BigInteger)(j_3)).longValue())][(int)(((java.math.BigInteger)(right_bound.subtract(pow2(new java.math.BigInteger(String.valueOf(j_3)))).add(java.math.BigInteger.valueOf(1)))).longValue())]));
        java.math.BigInteger val2_1 = new java.math.BigInteger(String.valueOf(sparse_table[(int)(((java.math.BigInteger)(j_3)).longValue())][(int)(((java.math.BigInteger)(left_bound)).longValue())]));
        if (val1_1.compareTo(val2_1) < 0) {
            return val1_1;
        }
        return val2_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            st1 = ((java.math.BigInteger[][])(build_sparse_table(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(3)})))));
            System.out.println(_p(st1));
            st2 = ((java.math.BigInteger[][])(build_sparse_table(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(9)})))));
            System.out.println(_p(st2));
            System.out.println(_p(query(((java.math.BigInteger[][])(st1)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4))));
            System.out.println(_p(query(((java.math.BigInteger[][])(st1)), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6))));
            System.out.println(_p(query(((java.math.BigInteger[][])(st2)), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2))));
            System.out.println(_p(query(((java.math.BigInteger[][])(st2)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1))));
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
