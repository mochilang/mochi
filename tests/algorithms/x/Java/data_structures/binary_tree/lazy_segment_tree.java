public class Main {
    static java.math.BigInteger NEG_INF;
    static java.math.BigInteger[] A;
    static java.math.BigInteger n = java.math.BigInteger.valueOf(15);
    static java.math.BigInteger[] segment_tree = new java.math.BigInteger[0];
    static java.math.BigInteger[] lazy = new java.math.BigInteger[0];
    static boolean[] flag = new boolean[0];

    static java.math.BigInteger[] init_int_array(java.math.BigInteger n) {
        java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(java.math.BigInteger.valueOf(4).multiply(n).add(java.math.BigInteger.valueOf(5))) < 0) {
            arr = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return arr;
    }

    static boolean[] init_bool_array(java.math.BigInteger n) {
        boolean[] arr_1 = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(java.math.BigInteger.valueOf(4).multiply(n).add(java.math.BigInteger.valueOf(5))) < 0) {
            arr_1 = ((boolean[])(appendBool(arr_1, false)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return arr_1;
    }

    static java.math.BigInteger left(java.math.BigInteger idx) {
        return idx.multiply(java.math.BigInteger.valueOf(2));
    }

    static java.math.BigInteger right(java.math.BigInteger idx) {
        return idx.multiply(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1));
    }

    static void build(java.math.BigInteger[] segment_tree, java.math.BigInteger idx, java.math.BigInteger l, java.math.BigInteger r, java.math.BigInteger[] a) {
        if (l.compareTo(r) == 0) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(a[(int)(((java.math.BigInteger)(l.subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
        } else {
            java.math.BigInteger mid = new java.math.BigInteger(String.valueOf((l.add(r)).divide(java.math.BigInteger.valueOf(2))));
            build(((java.math.BigInteger[])(segment_tree)), new java.math.BigInteger(String.valueOf(left(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(l)), new java.math.BigInteger(String.valueOf(mid)), ((java.math.BigInteger[])(a)));
            build(((java.math.BigInteger[])(segment_tree)), new java.math.BigInteger(String.valueOf(right(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(r)), ((java.math.BigInteger[])(a)));
            java.math.BigInteger lv = new java.math.BigInteger(String.valueOf(segment_tree[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
            java.math.BigInteger rv = new java.math.BigInteger(String.valueOf(segment_tree[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
            if (lv.compareTo(rv) > 0) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(lv));
            } else {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(rv));
            }
        }
    }

    static void update(java.math.BigInteger[] segment_tree, java.math.BigInteger[] lazy, boolean[] flag, java.math.BigInteger idx, java.math.BigInteger l, java.math.BigInteger r, java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger val) {
        if (flag[(int)(((java.math.BigInteger)(idx)).longValue())]) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
flag[(int)(((java.math.BigInteger)(idx)).longValue())] = false;
            if (l.compareTo(r) != 0) {
lazy[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
lazy[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
flag[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
flag[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
            }
        }
        if (r.compareTo(a) < 0 || l.compareTo(b) > 0) {
            return;
        }
        if (l.compareTo(a) >= 0 && r.compareTo(b) <= 0) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(val));
            if (l.compareTo(r) != 0) {
lazy[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(val));
lazy[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(val));
flag[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
flag[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
            }
            return;
        }
        java.math.BigInteger mid_2 = new java.math.BigInteger(String.valueOf((l.add(r)).divide(java.math.BigInteger.valueOf(2))));
        update(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), new java.math.BigInteger(String.valueOf(left(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(l)), new java.math.BigInteger(String.valueOf(mid_2)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)), new java.math.BigInteger(String.valueOf(val)));
        update(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), new java.math.BigInteger(String.valueOf(right(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid_2.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(r)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)), new java.math.BigInteger(String.valueOf(val)));
        java.math.BigInteger lv_2 = new java.math.BigInteger(String.valueOf(segment_tree[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
        java.math.BigInteger rv_2 = new java.math.BigInteger(String.valueOf(segment_tree[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
        if (lv_2.compareTo(rv_2) > 0) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(lv_2));
        } else {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(rv_2));
        }
    }

    static java.math.BigInteger query(java.math.BigInteger[] segment_tree, java.math.BigInteger[] lazy, boolean[] flag, java.math.BigInteger idx, java.math.BigInteger l, java.math.BigInteger r, java.math.BigInteger a, java.math.BigInteger b) {
        if (flag[(int)(((java.math.BigInteger)(idx)).longValue())]) {
segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
flag[(int)(((java.math.BigInteger)(idx)).longValue())] = false;
            if (l.compareTo(r) != 0) {
lazy[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
lazy[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = new java.math.BigInteger(String.valueOf(lazy[(int)(((java.math.BigInteger)(idx)).longValue())]));
flag[(int)(((java.math.BigInteger)(left(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
flag[(int)(((java.math.BigInteger)(right(new java.math.BigInteger(String.valueOf(idx))))).longValue())] = true;
            }
        }
        if (r.compareTo(a) < 0 || l.compareTo(b) > 0) {
            return NEG_INF;
        }
        if (l.compareTo(a) >= 0 && r.compareTo(b) <= 0) {
            return segment_tree[(int)(((java.math.BigInteger)(idx)).longValue())];
        }
        java.math.BigInteger mid_4 = new java.math.BigInteger(String.valueOf((l.add(r)).divide(java.math.BigInteger.valueOf(2))));
        java.math.BigInteger q1_1 = new java.math.BigInteger(String.valueOf(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), new java.math.BigInteger(String.valueOf(left(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(l)), new java.math.BigInteger(String.valueOf(mid_4)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)))));
        java.math.BigInteger q2_1 = new java.math.BigInteger(String.valueOf(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), new java.math.BigInteger(String.valueOf(right(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid_4.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(r)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)))));
        if (q1_1.compareTo(q2_1) > 0) {
            return q1_1;
        } else {
            return q2_1;
        }
    }

    static String segtree_to_string(java.math.BigInteger[] segment_tree, java.math.BigInteger[] lazy, boolean[] flag, java.math.BigInteger n) {
        String res = "[";
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(1);
        while (i_5.compareTo(n) <= 0) {
            java.math.BigInteger v_1 = new java.math.BigInteger(String.valueOf(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), new java.math.BigInteger(String.valueOf(i_5)), new java.math.BigInteger(String.valueOf(i_5)))));
            res = res + _p(v_1);
            if (i_5.compareTo(n) < 0) {
                res = res + ", ";
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        res = res + "]";
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NEG_INF = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1000000000)).negate()));
            A = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate())), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5)).negate())), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(11), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(20)).negate())), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(8)).negate()))}));
            segment_tree = ((java.math.BigInteger[])(init_int_array(new java.math.BigInteger(String.valueOf(n)))));
            lazy = ((java.math.BigInteger[])(init_int_array(new java.math.BigInteger(String.valueOf(n)))));
            flag = ((boolean[])(init_bool_array(new java.math.BigInteger(String.valueOf(n)))));
            build(((java.math.BigInteger[])(segment_tree)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), ((java.math.BigInteger[])(A)));
            System.out.println(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6)));
            System.out.println(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(11)));
            System.out.println(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(12)));
            update(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(111));
            System.out.println(query(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(15)));
            update(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(n)), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(235));
            System.out.println(segtree_to_string(((java.math.BigInteger[])(segment_tree)), ((java.math.BigInteger[])(lazy)), ((boolean[])(flag)), new java.math.BigInteger(String.valueOf(n))));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
