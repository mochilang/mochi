public class Main {

    static long[][] sort_edges(long[][] edges) {
        long[][] es = ((long[][])(edges));
        long i_1 = 0L;
        while (i_1 < es.length) {
            long j_1 = 0L;
            while (j_1 < es.length - i_1 - 1) {
                if (es[(int)((long)(j_1))][(int)((long)(2))] > es[(int)((long)(j_1 + 1))][(int)((long)(2))]) {
                    long[] tmp_1 = ((long[])(es[(int)((long)(j_1))]));
es[(int)((long)(j_1))] = ((long[])(es[(int)((long)(j_1 + 1))]));
es[(int)((long)(j_1 + 1))] = ((long[])(tmp_1));
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return es;
    }

    static long find(long[] parent, long x) {
        long r = x;
        while (parent[(int)((long)(r))] != r) {
            r = parent[(int)((long)(r))];
        }
        return r;
    }

    static long[][] kruskal(long n, long[][] edges) {
        long[] parent = ((long[])(new long[]{}));
        long i_3 = 0L;
        while (i_3 < n) {
            parent = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent), java.util.stream.LongStream.of(i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        long[][] sorted_1 = ((long[][])(sort_edges(((long[][])(edges)))));
        long[][] mst_1 = ((long[][])(new long[][]{}));
        long e_1 = 0L;
        while (e_1 < sorted_1.length) {
            if (mst_1.length == n - 1) {
                break;
            }
            long[] edge_1 = ((long[])(sorted_1[(int)((long)(e_1))]));
            e_1 = e_1 + 1;
            long u_1 = edge_1[(int)((long)(0))];
            long v_1 = edge_1[(int)((long)(1))];
            long w_1 = edge_1[(int)((long)(2))];
            long ru_1 = find(((long[])(parent)), u_1);
            long rv_1 = find(((long[])(parent)), v_1);
            if (ru_1 != rv_1) {
parent[(int)((long)(ru_1))] = rv_1;
                mst_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_1), java.util.stream.Stream.of(new long[]{u_1, v_1, w_1})).toArray(long[][]::new)));
            }
        }
        return mst_1;
    }

    static boolean edges_equal(long[][] a, long[][] b) {
        if (a.length != b.length) {
            return false;
        }
        long i_5 = 0L;
        while (i_5 < a.length) {
            long[] e1_1 = ((long[])(a[(int)((long)(i_5))]));
            long[] e2_1 = ((long[])(b[(int)((long)(i_5))]));
            if (e1_1[(int)((long)(0))] != e2_1[(int)((long)(0))] || e1_1[(int)((long)(1))] != e2_1[(int)((long)(1))] || e1_1[(int)((long)(2))] != e2_1[(int)((long)(2))]) {
                return false;
            }
            i_5 = i_5 + 1;
        }
        return true;
    }

    static void main() {
        long num_nodes = 9;
        long[][] edges_1 = ((long[][])(new long[][]{new long[]{0, 1, 4}, new long[]{0, 7, 8}, new long[]{1, 2, 8}, new long[]{7, 8, 7}, new long[]{7, 6, 1}, new long[]{2, 8, 2}, new long[]{8, 6, 6}, new long[]{2, 3, 7}, new long[]{2, 5, 4}, new long[]{6, 5, 2}, new long[]{3, 5, 14}, new long[]{3, 4, 9}, new long[]{5, 4, 10}, new long[]{1, 7, 11}}));
        long[][] expected_1 = ((long[][])(new long[][]{new long[]{7, 6, 1}, new long[]{2, 8, 2}, new long[]{6, 5, 2}, new long[]{0, 1, 4}, new long[]{2, 5, 4}, new long[]{2, 3, 7}, new long[]{0, 7, 8}, new long[]{3, 4, 9}}));
        long[][] result_1 = ((long[][])(kruskal(num_nodes, ((long[][])(edges_1)))));
        long[][] sorted_result_1 = ((long[][])(sort_edges(((long[][])(result_1)))));
        long[][] sorted_expected_1 = ((long[][])(sort_edges(((long[][])(expected_1)))));
        System.out.println(_p(sorted_result_1));
        if (((Boolean)(edges_equal(((long[][])(sorted_expected_1)), ((long[][])(sorted_result_1)))))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        main();
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
