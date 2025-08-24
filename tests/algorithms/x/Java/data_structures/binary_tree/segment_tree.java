public class Main {
    static java.math.BigInteger[] A = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    static java.math.BigInteger N = java.math.BigInteger.valueOf(0);
    static java.math.BigInteger[] st = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    static java.math.BigInteger NEG_INF;

    static java.math.BigInteger left_child(java.math.BigInteger idx) {
        return idx.multiply(java.math.BigInteger.valueOf(2));
    }

    static java.math.BigInteger right_child(java.math.BigInteger idx) {
        return idx.multiply(java.math.BigInteger.valueOf(2)).add(java.math.BigInteger.valueOf(1));
    }

    static void build(java.math.BigInteger idx, java.math.BigInteger left, java.math.BigInteger right) {
        if (left.compareTo(right) == 0) {
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(A[(int)(((java.math.BigInteger)(left)).longValue())]));
        } else {
            java.math.BigInteger mid = new java.math.BigInteger(String.valueOf((left.add(right)).divide(java.math.BigInteger.valueOf(2))));
            build(new java.math.BigInteger(String.valueOf(left_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(left)), new java.math.BigInteger(String.valueOf(mid)));
            build(new java.math.BigInteger(String.valueOf(right_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(right)));
            java.math.BigInteger left_val = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(left_child(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
            java.math.BigInteger right_val = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(right_child(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(left_val.compareTo(right_val) > 0 ? left_val : right_val));
        }
    }

    static boolean update_recursive(java.math.BigInteger idx, java.math.BigInteger left, java.math.BigInteger right, java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger val) {
        if (right.compareTo(a) < 0 || left.compareTo(b) > 0) {
            return true;
        }
        if (left.compareTo(right) == 0) {
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(val));
            return true;
        }
        java.math.BigInteger mid_2 = new java.math.BigInteger(String.valueOf((left.add(right)).divide(java.math.BigInteger.valueOf(2))));
        update_recursive(new java.math.BigInteger(String.valueOf(left_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(left)), new java.math.BigInteger(String.valueOf(mid_2)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)), new java.math.BigInteger(String.valueOf(val)));
        update_recursive(new java.math.BigInteger(String.valueOf(right_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid_2.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(right)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)), new java.math.BigInteger(String.valueOf(val)));
        java.math.BigInteger left_val_2 = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(left_child(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
        java.math.BigInteger right_val_2 = new java.math.BigInteger(String.valueOf(st[(int)(((java.math.BigInteger)(right_child(new java.math.BigInteger(String.valueOf(idx))))).longValue())]));
st[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(left_val_2.compareTo(right_val_2) > 0 ? left_val_2 : right_val_2));
        return true;
    }

    static boolean update(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger val) {
        return update_recursive(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(N.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(a.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(b.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(val)));
    }

    static java.math.BigInteger query_recursive(java.math.BigInteger idx, java.math.BigInteger left, java.math.BigInteger right, java.math.BigInteger a, java.math.BigInteger b) {
        if (right.compareTo(a) < 0 || left.compareTo(b) > 0) {
            return NEG_INF;
        }
        if (left.compareTo(a) >= 0 && right.compareTo(b) <= 0) {
            return st[(int)(((java.math.BigInteger)(idx)).longValue())];
        }
        java.math.BigInteger mid_4 = new java.math.BigInteger(String.valueOf((left.add(right)).divide(java.math.BigInteger.valueOf(2))));
        java.math.BigInteger q1_1 = new java.math.BigInteger(String.valueOf(query_recursive(new java.math.BigInteger(String.valueOf(left_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(left)), new java.math.BigInteger(String.valueOf(mid_4)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)))));
        java.math.BigInteger q2_1 = new java.math.BigInteger(String.valueOf(query_recursive(new java.math.BigInteger(String.valueOf(right_child(new java.math.BigInteger(String.valueOf(idx))))), new java.math.BigInteger(String.valueOf(mid_4.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(right)), new java.math.BigInteger(String.valueOf(a)), new java.math.BigInteger(String.valueOf(b)))));
        return q1_1.compareTo(q2_1) > 0 ? q1_1 : q2_1;
    }

    static java.math.BigInteger query(java.math.BigInteger a, java.math.BigInteger b) {
        return query_recursive(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(N.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(a.subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(b.subtract(java.math.BigInteger.valueOf(1)))));
    }

    static void show_data() {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        java.math.BigInteger[] show_list_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        while (i.compareTo(N) < 0) {
            show_list_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(show_list_1), java.util.stream.Stream.of(query(new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))))))).toArray(java.math.BigInteger[]::new)));
            i = new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println(java.util.Arrays.toString(show_list_1));
    }

    static void main() {
        A = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(4)).negate())), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(5)).negate())), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(11), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(20)).negate())), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(15), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(8)).negate()))}));
        N = new java.math.BigInteger(String.valueOf(A.length));
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(0);
        while (i_2.compareTo(java.math.BigInteger.valueOf(4).multiply(N)) < 0) {
            st = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(st), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
            i_2 = new java.math.BigInteger(String.valueOf(i_2.add(java.math.BigInteger.valueOf(1))));
        }
        if (N.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            build(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(N.subtract(java.math.BigInteger.valueOf(1)))));
        }
        System.out.println(query(java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6)));
        System.out.println(query(java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(11)));
        System.out.println(query(java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(12)));
        update(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(111));
        System.out.println(query(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(15)));
        update(java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(235));
        show_data();
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NEG_INF = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1000000000)).negate()));
            main();
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
}
