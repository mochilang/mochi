public class Main {
    static class Edge {
        long node;
        long weight;
        Edge(long node, long weight) {
            this.node = node;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'weight': %s}", String.valueOf(node), String.valueOf(weight));
        }
    }

    static Edge[][] graph;
    static long[] dist_2;

    static long[] make_int_list(long n, long value) {
        long[] lst = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            lst = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(lst), java.util.stream.LongStream.of((long)(value))).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return lst;
    }

    static boolean[] make_bool_list(long n) {
        boolean[] lst_1 = ((boolean[])(new boolean[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            lst_1 = ((boolean[])(appendBool(lst_1, false)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return lst_1;
    }

    static long[] dijkstra(Edge[][] graph, long src) {
        long n = (long)(graph.length);
        long[] dist_1 = ((long[])(make_int_list((long)(n), 1000000000L)));
        boolean[] visited_1 = ((boolean[])(make_bool_list((long)(n))));
dist_1[(int)((long)(src))] = 0L;
        long count_1 = 0L;
        while ((long)(count_1) < (long)(n)) {
            long u_1 = (long)(-1);
            long min_dist_1 = 1000000000L;
            long i_5 = 0L;
            while ((long)(i_5) < (long)(n)) {
                if (!visited_1[(int)((long)(i_5))] && (long)(dist_1[(int)((long)(i_5))]) < (long)(min_dist_1)) {
                    min_dist_1 = (long)(dist_1[(int)((long)(i_5))]);
                    u_1 = (long)(i_5);
                }
                i_5 = (long)((long)(i_5) + 1L);
            }
            if ((long)(u_1) < 0L) {
                break;
            }
visited_1[(int)((long)(u_1))] = true;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(graph[(int)((long)(u_1))].length)) {
                Edge e_1 = graph[(int)((long)(u_1))][(int)((long)(j_1))];
                long v_1 = (long)(e_1.node);
                long w_1 = (long)(e_1.weight);
                if (!visited_1[(int)((long)(v_1))]) {
                    long new_dist_1 = (long)((long)(dist_1[(int)((long)(u_1))]) + (long)(w_1));
                    if ((long)(new_dist_1) < (long)(dist_1[(int)((long)(v_1))])) {
dist_1[(int)((long)(v_1))] = (long)(new_dist_1);
                    }
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            count_1 = (long)((long)(count_1) + 1L);
        }
        return dist_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((Edge[][])(new Edge[][]{new Edge[]{new Edge(1, 10), new Edge(3, 5)}, new Edge[]{new Edge(2, 1), new Edge(3, 2)}, new Edge[]{new Edge(4, 4)}, new Edge[]{new Edge(1, 3), new Edge(2, 9), new Edge(4, 2)}, new Edge[]{new Edge(0, 7), new Edge(2, 6)}}));
            dist_2 = ((long[])(dijkstra(((Edge[][])(graph)), 0L)));
            System.out.println(_p(_geti(dist_2, ((Number)(0)).intValue())));
            System.out.println(_p(_geti(dist_2, ((Number)(1)).intValue())));
            System.out.println(_p(_geti(dist_2, ((Number)(2)).intValue())));
            System.out.println(_p(_geti(dist_2, ((Number)(3)).intValue())));
            System.out.println(_p(_geti(dist_2, ((Number)(4)).intValue())));
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
