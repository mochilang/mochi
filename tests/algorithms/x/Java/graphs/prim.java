public class Main {
    static long INF = 1000000000L;

    static java.util.Map<Long,long[][]> connect(java.util.Map<Long,long[][]> graph, long a, long b, long w) {
        long u = (long)((long)(a) - 1L);
        long v_1 = (long)((long)(b) - 1L);
        java.util.Map<Long,long[][]> g_1 = graph;
g_1.put(u, ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(((long[][])(g_1).get(u))), java.util.stream.Stream.of(new long[][]{new long[]{v_1, w}})).toArray(long[][]::new))));
g_1.put(v_1, ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(((long[][])(g_1).get(v_1))), java.util.stream.Stream.of(new long[][]{new long[]{u, w}})).toArray(long[][]::new))));
        return g_1;
    }

    static boolean in_list(long[] arr, long x) {
        long i = 0L;
        while ((long)(i) < (long)(arr.length)) {
            if ((long)(arr[(int)((long)(i))]) == (long)(x)) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static long[][] prim(java.util.Map<Long,long[][]> graph, long s, long n) {
        java.util.Map<Long,Long> dist = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        java.util.Map<Long,Long> parent_1 = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
dist.put(s, 0L);
parent_1.put(s, (long)(-1));
        long[] known_1 = ((long[])(new long[]{}));
        long[] keys_1 = ((long[])(new long[]{s}));
        while ((long)(known_1.length) < (long)(n)) {
            long mini_1 = (long)(INF);
            long u_2 = (long)(-1);
            long i_2 = 0L;
            while ((long)(i_2) < (long)(keys_1.length)) {
                long k_1 = (long)(keys_1[(int)((long)(i_2))]);
                long d_1 = (long)(((long)(dist).getOrDefault(k_1, 0L)));
                if (!(Boolean)(in_list(((long[])(known_1)), (long)(k_1))) && (long)(d_1) < (long)(mini_1)) {
                    mini_1 = (long)(d_1);
                    u_2 = (long)(k_1);
                }
                i_2 = (long)((long)(i_2) + 1L);
            }
            known_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(known_1), java.util.stream.LongStream.of((long)(u_2))).toArray()));
            for (long[] e : ((long[][])(graph).get(u_2))) {
                long v_3 = (long)(e[(int)((long)(0))]);
                long w_1 = (long)(e[(int)((long)(1))]);
                if (!(Boolean)(in_list(((long[])(keys_1)), (long)(v_3)))) {
                    keys_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(keys_1), java.util.stream.LongStream.of((long)(v_3))).toArray()));
                }
                long cur_1 = (long)(dist.containsKey(v_3) ? ((long)(dist).getOrDefault(v_3, 0L)) : INF);
                if (!(Boolean)(in_list(((long[])(known_1)), (long)(v_3))) && (long)(w_1) < (long)(cur_1)) {
dist.put(v_3, (long)(w_1));
parent_1.put(v_3, (long)(u_2));
                }
            }
        }
        long[][] edges_1 = ((long[][])(new long[][]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(keys_1.length)) {
            long v_5 = (long)(keys_1[(int)((long)(j_1))]);
            if ((long)(v_5) != (long)(s)) {
                edges_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges_1), java.util.stream.Stream.of(new long[][]{new long[]{(long)(v_5) + 1L, (long)(((long)(parent_1).getOrDefault(v_5, 0L))) + 1L}})).toArray(long[][]::new)));
            }
            j_1 = (long)((long)(j_1) + 1L);
        }
        return edges_1;
    }

    static long[] sort_heap(long[] h, java.util.Map<Long,Long> dist) {
        long[] a = ((long[])(h));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(a.length)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)((long)((long)(a.length) - (long)(i_4)) - 1L)) {
                long dj_1 = (long)(dist.containsKey(a[(int)((long)(j_3))]) ? ((long)(dist).getOrDefault(a[(int)((long)(j_3))], 0L)) : INF);
                long dj1_1 = (long)(dist.containsKey(a[(int)((long)((long)(j_3) + 1L))]) ? ((long)(dist).getOrDefault(a[(int)((long)((long)(j_3) + 1L))], 0L)) : INF);
                if ((long)(dj_1) > (long)(dj1_1)) {
                    long t_1 = (long)(a[(int)((long)(j_3))]);
a[(int)((long)(j_3))] = (long)(a[(int)((long)((long)(j_3) + 1L))]);
a[(int)((long)((long)(j_3) + 1L))] = (long)(t_1);
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        return a;
    }

    static long[][] prim_heap(java.util.Map<Long,long[][]> graph, long s, long n) {
        java.util.Map<Long,Long> dist_1 = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        java.util.Map<Long,Long> parent_3 = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
dist_1.put(s, 0L);
parent_3.put(s, (long)(-1));
        long[] h_1 = ((long[])(new long[]{}));
        long i_6 = 0L;
        while ((long)(i_6) < (long)(n)) {
            h_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(h_1), java.util.stream.LongStream.of((long)(i_6))).toArray()));
            i_6 = (long)((long)(i_6) + 1L);
        }
        h_1 = ((long[])(sort_heap(((long[])(h_1)), dist_1)));
        long[] known_3 = ((long[])(new long[]{}));
        while ((long)(h_1.length) > 0L) {
            long u_4 = (long)(h_1[(int)((long)(0))]);
            h_1 = ((long[])(java.util.Arrays.copyOfRange(h_1, (int)((long)(1)), (int)((long)(h_1.length)))));
            known_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(known_3), java.util.stream.LongStream.of((long)(u_4))).toArray()));
            for (long[] e : ((long[][])(graph).get(u_4))) {
                long v_7 = (long)(e[(int)((long)(0))]);
                long w_3 = (long)(e[(int)((long)(1))]);
                long cur_3 = (long)(dist_1.containsKey(v_7) ? ((long)(dist_1).getOrDefault(v_7, 0L)) : INF);
                if (!(Boolean)(in_list(((long[])(known_3)), (long)(v_7))) && (long)(w_3) < (long)(cur_3)) {
dist_1.put(v_7, (long)(w_3));
parent_3.put(v_7, (long)(u_4));
                }
            }
            h_1 = ((long[])(sort_heap(((long[])(h_1)), dist_1)));
        }
        long[][] edges_3 = ((long[][])(new long[][]{}));
        long j_5 = 0L;
        while ((long)(j_5) < (long)(n)) {
            if ((long)(j_5) != (long)(s)) {
                edges_3 = java.util.stream.Stream.concat(java.util.Arrays.stream(edges_3), java.util.stream.Stream.of(new Object[][]{new double[]{(long)(j_5) + 1L, (long)(((long)(parent_3).getOrDefault(j_5, 0L))) + 1L}})).toArray(Object[][]::new);
            }
            j_5 = (long)((long)(j_5) + 1L);
        }
        return ((long[][])(edges_3));
    }

    static void print_edges(long[][] edges) {
        long i_7 = 0L;
        while ((long)(i_7) < (long)(edges.length)) {
            long[] e_1 = ((long[])(edges[(int)((long)(i_7))]));
            System.out.println("(" + _p(_geti(e_1, ((Number)(0)).intValue())) + ", " + _p(_geti(e_1, ((Number)(1)).intValue())) + ")");
            i_7 = (long)((long)(i_7) + 1L);
        }
    }

    static void test_vector() {
        long x = 5L;
        java.util.Map<Long,long[][]> G_1 = ((java.util.Map<Long,long[][]>)(new java.util.LinkedHashMap<Long, long[][]>()));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(x)) {
G_1.put(i_9, ((long[][])(new long[][]{})));
            i_9 = (long)((long)(i_9) + 1L);
        }
        G_1 = connect(G_1, 1L, 2L, 15L);
        G_1 = connect(G_1, 1L, 3L, 12L);
        G_1 = connect(G_1, 2L, 4L, 13L);
        G_1 = connect(G_1, 2L, 5L, 5L);
        G_1 = connect(G_1, 3L, 2L, 6L);
        G_1 = connect(G_1, 3L, 4L, 6L);
        long[][] mst_1 = ((long[][])(prim(G_1, 0L, (long)(x))));
        print_edges(((long[][])(mst_1)));
        long[][] mst_heap_1 = ((long[][])(prim_heap(G_1, 0L, (long)(x))));
        print_edges(((long[][])(mst_heap_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_vector();
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
