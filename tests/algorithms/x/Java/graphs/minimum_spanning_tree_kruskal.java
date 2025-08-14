public class Main {
    static long[][] edges1;
    static long[][] edges2;
    static long[][] edges3;

    static long[][] sort_edges(long[][] edges) {
        long[][] es = ((long[][])(edges));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(es.length)) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)((long)(es.length) - (long)(i_1)) - 1L)) {
                if ((long)(es[(int)((long)(j_1))][(int)((long)(2))]) > (long)(es[(int)((long)((long)(j_1) + 1L))][(int)((long)(2))])) {
                    long[] temp_1 = ((long[])(es[(int)((long)(j_1))]));
es[(int)((long)(j_1))] = ((long[])(es[(int)((long)((long)(j_1) + 1L))]));
es[(int)((long)((long)(j_1) + 1L))] = ((long[])(temp_1));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return es;
    }

    static long find_parent(long[] parent, long i) {
        if ((long)(parent[(int)((long)(i))]) != (long)(i)) {
parent[(int)((long)(i))] = (long)(find_parent(((long[])(parent)), (long)(parent[(int)((long)(i))])));
        }
        return parent[(int)((long)(i))];
    }

    static long[][] kruskal(long num_nodes, long[][] edges) {
        long[][] es_1 = ((long[][])(sort_edges(((long[][])(edges)))));
        long[] parent_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(num_nodes)) {
            parent_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent_1), java.util.stream.LongStream.of((long)(i_3))).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long[][] mst_1 = ((long[][])(new long[][]{}));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(es_1.length)) {
            long[] e_1 = ((long[])(es_1[(int)((long)(idx_1))]));
            long pa_1 = (long)(find_parent(((long[])(parent_1)), (long)(e_1[(int)((long)(0))])));
            long pb_1 = (long)(find_parent(((long[])(parent_1)), (long)(e_1[(int)((long)(1))])));
            if ((long)(pa_1) != (long)(pb_1)) {
                mst_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_1), java.util.stream.Stream.of(new long[][]{e_1})).toArray(long[][]::new)));
parent_1[(int)((long)(pa_1))] = (long)(pb_1);
            }
            idx_1 = (long)((long)(idx_1) + 1L);
        }
        return mst_1;
    }

    static String edges_to_string(long[][] es) {
        String s = "[";
        long i_5 = 0L;
        while ((long)(i_5) < (long)(es.length)) {
            long[] e_3 = ((long[])(es[(int)((long)(i_5))]));
            s = s + "(" + _p(_geti(e_3, ((Number)(0)).intValue())) + ", " + _p(_geti(e_3, ((Number)(1)).intValue())) + ", " + _p(_geti(e_3, ((Number)(2)).intValue())) + ")";
            if ((long)(i_5) < (long)((long)(es.length) - 1L)) {
                s = s + ", ";
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            edges1 = ((long[][])(new long[][]{new long[]{0, 1, 3}, new long[]{1, 2, 5}, new long[]{2, 3, 1}}));
            System.out.println(edges_to_string(((long[][])(kruskal(4L, ((long[][])(edges1)))))));
            edges2 = ((long[][])(new long[][]{new long[]{0, 1, 3}, new long[]{1, 2, 5}, new long[]{2, 3, 1}, new long[]{0, 2, 1}, new long[]{0, 3, 2}}));
            System.out.println(edges_to_string(((long[][])(kruskal(4L, ((long[][])(edges2)))))));
            edges3 = ((long[][])(new long[][]{new long[]{0, 1, 3}, new long[]{1, 2, 5}, new long[]{2, 3, 1}, new long[]{0, 2, 1}, new long[]{0, 3, 2}, new long[]{2, 1, 1}}));
            System.out.println(edges_to_string(((long[][])(kruskal(4L, ((long[][])(edges3)))))));
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
