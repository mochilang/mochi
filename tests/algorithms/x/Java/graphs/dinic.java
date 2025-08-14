public class Main {
    static long INF = 1000000000L;
    static class Dinic {
        long n;
        long[] lvl;
        long[] ptr;
        long[] q;
        long[][][] adj;
        Dinic(long n, long[] lvl, long[] ptr, long[] q, long[][][] adj) {
            this.n = n;
            this.lvl = lvl;
            this.ptr = ptr;
            this.q = q;
            this.adj = adj;
        }
        Dinic() {}
        @Override public String toString() {
            return String.format("{'n': %s, 'lvl': %s, 'ptr': %s, 'q': %s, 'adj': %s}", String.valueOf(n), String.valueOf(lvl), String.valueOf(ptr), String.valueOf(q), String.valueOf(adj));
        }
    }

    static Dinic graph = null;
    static long source = 0L;
    static long sink = 9L;
    static long v_2 = 1L;

    static long pow2(long k) {
        long res = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(k)) {
            res = (long)((long)(res) * 2L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static long min2(long a, long b) {
        if ((long)(a) < (long)(b)) {
            return a;
        }
        return b;
    }

    static Dinic new_dinic(long n) {
        long[] lvl = ((long[])(new long[]{}));
        long[] ptr_1 = ((long[])(new long[]{}));
        long[] q_1 = ((long[])(new long[]{}));
        long[][][] adj_1 = ((long[][][])(new long[][][]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            lvl = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lvl), java.util.stream.LongStream.of(0L)).toArray()));
            ptr_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(ptr_1), java.util.stream.LongStream.of(0L)).toArray()));
            q_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(q_1), java.util.stream.LongStream.of(0L)).toArray()));
            long[][] edges_1 = ((long[][])(new long[][]{}));
            adj_1 = ((long[][][])(java.util.stream.Stream.concat(java.util.Arrays.stream(adj_1), java.util.stream.Stream.of(new long[][][]{edges_1})).toArray(long[][][]::new)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return new Dinic(n, lvl, ptr_1, q_1, adj_1);
    }

    static void add_edge(Dinic g, long a, long b, long c, long rcap) {
        long[][][] adj_2 = ((long[][][])(g.adj));
        long[][] list_a_1 = ((long[][])(adj_2[(int)((long)(a))]));
        long[][] list_b_1 = ((long[][])(adj_2[(int)((long)(b))]));
        long[] e1_1 = ((long[])(new double[]{b, list_b_1.length, c, 0}));
        long[] e2_1 = ((long[])(new double[]{a, list_a_1.length, rcap, 0}));
        list_a_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_a_1), java.util.stream.Stream.of(new long[][]{e1_1})).toArray(long[][]::new)));
        list_b_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(list_b_1), java.util.stream.Stream.of(new long[][]{e2_1})).toArray(long[][]::new)));
adj_2[(int)((long)(a))] = ((long[][])(list_a_1));
adj_2[(int)((long)(b))] = ((long[][])(list_b_1));
g.adj = adj_2;
    }

    static long dfs(Dinic g, long v, long sink, long flow) {
        if ((long)(v) == (long)(sink) || (long)(flow) == 0L) {
            return flow;
        }
        long[] ptr_3 = ((long[])(g.ptr));
        long i_5 = (long)(ptr_3[(int)((long)(v))]);
        long[][][] adj_all_1 = ((long[][][])(g.adj));
        long[][] adj_v_1 = ((long[][])(adj_all_1[(int)((long)(v))]));
        while ((long)(i_5) < (long)(adj_v_1.length)) {
            long[] e_1 = ((long[])(adj_v_1[(int)((long)(i_5))]));
            long to_1 = (long)(e_1[(int)((long)(0))]);
            if ((long)(g.lvl[(int)((long)(to_1))]) == (long)((long)(g.lvl[(int)((long)(v))]) + 1L)) {
                long avail_1 = (long)((long)(e_1[(int)((long)(2))]) - (long)(e_1[(int)((long)(3))]));
                long pushed_1 = (long)(dfs(g, (long)(to_1), (long)(sink), (long)(min2((long)(flow), (long)(avail_1)))));
                if ((long)(pushed_1) > 0L) {
e_1[(int)((long)(3))] = (long)((long)(e_1[(int)((long)(3))]) + (long)(pushed_1));
adj_v_1[(int)((long)(i_5))] = ((long[])(e_1));
                    long[][] adj_to_1 = ((long[][])(adj_all_1[(int)((long)(to_1))]));
                    long[] back_1 = ((long[])(adj_to_1[(int)((long)(e_1[(int)((long)(1))]))]));
back_1[(int)((long)(3))] = (long)((long)(back_1[(int)((long)(3))]) - (long)(pushed_1));
adj_to_1[(int)((long)(e_1[(int)((long)(1))]))] = ((long[])(back_1));
adj_all_1[(int)((long)(to_1))] = ((long[][])(adj_to_1));
adj_all_1[(int)((long)(v))] = ((long[][])(adj_v_1));
g.adj = adj_all_1;
                    return pushed_1;
                }
            }
            i_5 = (long)((long)(i_5) + 1L);
ptr_3[(int)((long)(v))] = (long)(i_5);
        }
g.ptr = ptr_3;
adj_all_1[(int)((long)(v))] = ((long[][])(adj_v_1));
g.adj = adj_all_1;
        return 0;
    }

    static long max_flow(Dinic g, long source, long sink) {
        long flow = 0L;
        long l_1 = 0L;
        while ((long)(l_1) < 31L) {
            long threshold_1 = (long)(pow2((long)(30L - (long)(l_1))));
            while (true) {
                long[] lvl_2 = ((long[])(new long[]{}));
                long[] ptr_5 = ((long[])(new long[]{}));
                long i_7 = 0L;
                while ((long)(i_7) < (long)(g.n)) {
                    lvl_2 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lvl_2), java.util.stream.LongStream.of(0L)).toArray()));
                    ptr_5 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(ptr_5), java.util.stream.LongStream.of(0L)).toArray()));
                    i_7 = (long)((long)(i_7) + 1L);
                }
g.lvl = lvl_2;
g.ptr = ptr_5;
                long qi_1 = 0L;
                long qe_1 = 1L;
lvl_2[(int)((long)(source))] = 1L;
g.lvl = lvl_2;
                long[] q_3 = ((long[])(g.q));
q_3[(int)((long)(0))] = (long)(source);
                while ((long)(qi_1) < (long)(qe_1) && (long)(g.lvl[(int)((long)(sink))]) == 0L) {
                    long v_1 = (long)(q_3[(int)((long)(qi_1))]);
                    qi_1 = (long)((long)(qi_1) + 1L);
                    long[][] edges_3 = ((long[][])(((long[][])(g.adj[(int)((long)(v_1))]))));
                    long j_1 = 0L;
                    while ((long)(j_1) < (long)(edges_3.length)) {
                        long[] e_3 = ((long[])(edges_3[(int)((long)(j_1))]));
                        long to_3 = (long)(e_3[(int)((long)(0))]);
                        long residual_1 = (long)((long)(e_3[(int)((long)(2))]) - (long)(e_3[(int)((long)(3))]));
                        long[] lvl_inner_1 = ((long[])(g.lvl));
                        if ((long)(lvl_inner_1[(int)((long)(to_3))]) == 0L && (long)(residual_1) >= (long)(threshold_1)) {
q_3[(int)((long)(qe_1))] = (long)(to_3);
                            qe_1 = (long)((long)(qe_1) + 1L);
lvl_inner_1[(int)((long)(to_3))] = (long)((long)(lvl_inner_1[(int)((long)(v_1))]) + 1L);
g.lvl = lvl_inner_1;
                        }
                        j_1 = (long)((long)(j_1) + 1L);
                    }
                }
                long p_1 = (long)(dfs(g, (long)(source), (long)(sink), (long)(INF)));
                while ((long)(p_1) > 0L) {
                    flow = (long)((long)(flow) + (long)(p_1));
                    p_1 = (long)(dfs(g, (long)(source), (long)(sink), (long)(INF)));
                }
                if ((long)(g.lvl[(int)((long)(sink))]) == 0L) {
                    break;
                }
            }
            l_1 = (long)((long)(l_1) + 1L);
        }
        return flow;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = new_dinic(10L);
            while ((long)(v_2) < 5L) {
                add_edge(graph, (long)(source), (long)(v_2), 1L, 0L);
                v_2 = (long)((long)(v_2) + 1L);
            }
            v_2 = 5L;
            while ((long)(v_2) < 9L) {
                add_edge(graph, (long)(v_2), (long)(sink), 1L, 0L);
                v_2 = (long)((long)(v_2) + 1L);
            }
            v_2 = 1L;
            while ((long)(v_2) < 5L) {
                add_edge(graph, (long)(v_2), (long)((long)(v_2) + 4L), 1L, 0L);
                v_2 = (long)((long)(v_2) + 1L);
            }
            System.out.println(_p(max_flow(graph, (long)(source), (long)(sink))));
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
}
