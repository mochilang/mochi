public class Main {
    static class FenwickTree {
        java.math.BigInteger size;
        java.math.BigInteger[] tree;
        FenwickTree(java.math.BigInteger size, java.math.BigInteger[] tree) {
            this.size = size;
            this.tree = tree;
        }
        FenwickTree() {}
        @Override public String toString() {
            return String.format("{'size': %s, 'tree': %s}", String.valueOf(size), String.valueOf(tree));
        }
    }

    static FenwickTree f_base;
    static FenwickTree f = null;
    static FenwickTree f2;
    static FenwickTree f3;

    static FenwickTree fenwick_from_list(java.math.BigInteger[] arr) {
        java.math.BigInteger size = new java.math.BigInteger(String.valueOf(arr.length));
        java.math.BigInteger[] tree_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(size) < 0) {
            tree_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tree_1), java.util.stream.Stream.of(arr[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(size) < 0) {
            java.math.BigInteger j_1 = new java.math.BigInteger(String.valueOf(fenwick_next(new java.math.BigInteger(String.valueOf(i_1)))));
            if (j_1.compareTo(size) < 0) {
tree_1[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf(tree_1[(int)(((java.math.BigInteger)(j_1)).longValue())].add(tree_1[(int)(((java.math.BigInteger)(i_1)).longValue())])));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new FenwickTree(size, tree_1);
    }

    static FenwickTree fenwick_empty(java.math.BigInteger size) {
        java.math.BigInteger[] tree_2 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(size) < 0) {
            tree_2 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tree_2), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return new FenwickTree(size, tree_2);
    }

    static java.math.BigInteger[] fenwick_get_array(FenwickTree f) {
        java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(0);
        while (i_5.compareTo(f.size) < 0) {
            arr = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(f.tree[(int)(((java.math.BigInteger)(i_5)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_5 = new java.math.BigInteger(String.valueOf(i_5.add(java.math.BigInteger.valueOf(1))));
        }
        i_5 = new java.math.BigInteger(String.valueOf(f.size.subtract(java.math.BigInteger.valueOf(1))));
        while (i_5.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            java.math.BigInteger j_3 = new java.math.BigInteger(String.valueOf(fenwick_next(new java.math.BigInteger(String.valueOf(i_5)))));
            if (j_3.compareTo(f.size) < 0) {
arr[(int)(((java.math.BigInteger)(j_3)).longValue())] = new java.math.BigInteger(String.valueOf(arr[(int)(((java.math.BigInteger)(j_3)).longValue())].subtract(arr[(int)(((java.math.BigInteger)(i_5)).longValue())])));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.subtract(java.math.BigInteger.valueOf(1))));
        }
        return arr;
    }

    static java.math.BigInteger bit_and(java.math.BigInteger a, java.math.BigInteger b) {
        java.math.BigInteger ua = new java.math.BigInteger(String.valueOf(a));
        java.math.BigInteger ub_1 = new java.math.BigInteger(String.valueOf(b));
        java.math.BigInteger res_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger bit_1 = java.math.BigInteger.valueOf(1);
        while (ua.compareTo(java.math.BigInteger.valueOf(0)) != 0 || ub_1.compareTo(java.math.BigInteger.valueOf(0)) != 0) {
            if (ua.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(1)) == 0 && ub_1.remainder(java.math.BigInteger.valueOf(2)).compareTo(java.math.BigInteger.valueOf(1)) == 0) {
                res_1 = new java.math.BigInteger(String.valueOf(res_1.add(bit_1)));
            }
            ua = new java.math.BigInteger(String.valueOf(((Number)((ua.divide(java.math.BigInteger.valueOf(2))))).intValue()));
            ub_1 = new java.math.BigInteger(String.valueOf(((Number)((ub_1.divide(java.math.BigInteger.valueOf(2))))).intValue()));
            bit_1 = new java.math.BigInteger(String.valueOf(bit_1.multiply(java.math.BigInteger.valueOf(2))));
        }
        return res_1;
    }

    static java.math.BigInteger low_bit(java.math.BigInteger x) {
        if (x.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        return x.subtract(bit_and(new java.math.BigInteger(String.valueOf(x)), new java.math.BigInteger(String.valueOf(x.subtract(java.math.BigInteger.valueOf(1))))));
    }

    static java.math.BigInteger fenwick_next(java.math.BigInteger index) {
        return index.add(low_bit(new java.math.BigInteger(String.valueOf(index))));
    }

    static java.math.BigInteger fenwick_prev(java.math.BigInteger index) {
        return index.subtract(low_bit(new java.math.BigInteger(String.valueOf(index))));
    }

    static FenwickTree fenwick_add(FenwickTree f, java.math.BigInteger index, java.math.BigInteger value) {
        java.math.BigInteger[] tree_3 = ((java.math.BigInteger[])(f.tree));
        if (index.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
tree_3[(int)(0L)] = new java.math.BigInteger(String.valueOf(tree_3[(int)(0L)].add(value)));
            return new FenwickTree(f.size, tree_3);
        }
        java.math.BigInteger i_7 = new java.math.BigInteger(String.valueOf(index));
        while (i_7.compareTo(f.size) < 0) {
tree_3[(int)(((java.math.BigInteger)(i_7)).longValue())] = new java.math.BigInteger(String.valueOf(tree_3[(int)(((java.math.BigInteger)(i_7)).longValue())].add(value)));
            i_7 = new java.math.BigInteger(String.valueOf(fenwick_next(new java.math.BigInteger(String.valueOf(i_7)))));
        }
        return new FenwickTree(f.size, tree_3);
    }

    static FenwickTree fenwick_update(FenwickTree f, java.math.BigInteger index, java.math.BigInteger value) {
        java.math.BigInteger current = new java.math.BigInteger(String.valueOf(fenwick_get(f, new java.math.BigInteger(String.valueOf(index)))));
        return fenwick_add(f, new java.math.BigInteger(String.valueOf(index)), new java.math.BigInteger(String.valueOf(value.subtract(current))));
    }

    static java.math.BigInteger fenwick_prefix(FenwickTree f, java.math.BigInteger right) {
        if (right.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return 0;
        }
        java.math.BigInteger result_1 = new java.math.BigInteger(String.valueOf(f.tree[(int)(0L)]));
        java.math.BigInteger r_1 = new java.math.BigInteger(String.valueOf(right.subtract(java.math.BigInteger.valueOf(1))));
        while (r_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            result_1 = new java.math.BigInteger(String.valueOf(result_1.add(f.tree[(int)(((java.math.BigInteger)(r_1)).longValue())])));
            r_1 = new java.math.BigInteger(String.valueOf(fenwick_prev(new java.math.BigInteger(String.valueOf(r_1)))));
        }
        return result_1;
    }

    static java.math.BigInteger fenwick_query(FenwickTree f, java.math.BigInteger left, java.math.BigInteger right) {
        return fenwick_prefix(f, new java.math.BigInteger(String.valueOf(right))).subtract(fenwick_prefix(f, new java.math.BigInteger(String.valueOf(left))));
    }

    static java.math.BigInteger fenwick_get(FenwickTree f, java.math.BigInteger index) {
        return fenwick_query(f, new java.math.BigInteger(String.valueOf(index)), new java.math.BigInteger(String.valueOf(index.add(java.math.BigInteger.valueOf(1)))));
    }

    static java.math.BigInteger fenwick_rank_query(FenwickTree f, java.math.BigInteger value) {
        java.math.BigInteger v = new java.math.BigInteger(String.valueOf(value.subtract(f.tree[(int)(0L)])));
        if (v.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            return (java.math.BigInteger.valueOf(1)).negate();
        }
        java.math.BigInteger j_5 = java.math.BigInteger.valueOf(1);
        while (j_5.multiply(java.math.BigInteger.valueOf(2)).compareTo(f.size) < 0) {
            j_5 = new java.math.BigInteger(String.valueOf(j_5.multiply(java.math.BigInteger.valueOf(2))));
        }
        java.math.BigInteger i_9 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger jj_1 = new java.math.BigInteger(String.valueOf(j_5));
        while (jj_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
            if (i_9.add(jj_1).compareTo(f.size) < 0 && f.tree[(int)(((java.math.BigInteger)(i_9.add(jj_1))).longValue())].compareTo(v) <= 0) {
                v = new java.math.BigInteger(String.valueOf(v.subtract(f.tree[(int)(((java.math.BigInteger)(i_9.add(jj_1))).longValue())])));
                i_9 = new java.math.BigInteger(String.valueOf(i_9.add(jj_1)));
            }
            jj_1 = new java.math.BigInteger(String.valueOf(jj_1.divide(java.math.BigInteger.valueOf(2))));
        }
        return i_9;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            f_base = fenwick_from_list(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)})));
            System.out.println(java.util.Arrays.toString(fenwick_get_array(f_base)));
            f = fenwick_from_list(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)})));
            f = fenwick_add(f, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1));
            f = fenwick_add(f, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2));
            f = fenwick_add(f, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3));
            f = fenwick_add(f, java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4));
            f = fenwick_add(f, java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5));
            System.out.println(java.util.Arrays.toString(fenwick_get_array(f)));
            f2 = fenwick_from_list(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)})));
            System.out.println(fenwick_prefix(f2, java.math.BigInteger.valueOf(3)));
            System.out.println(fenwick_query(f2, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(4)));
            f3 = fenwick_from_list(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5)})));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(0)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(2)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(1)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(3)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(5)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(6)));
            System.out.println(fenwick_rank_query(f3, java.math.BigInteger.valueOf(11)));
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
