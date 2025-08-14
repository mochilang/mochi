public class Main {
    static class Edge {
        long to;
        long weight;
        Edge(long to, long weight) {
            this.to = to;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'to': %s, 'weight': %s}", String.valueOf(to), String.valueOf(weight));
        }
    }

    static class Pair {
        long u;
        long v;
        Pair(long u, long v) {
            this.u = u;
            this.v = v;
        }
        Pair() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s}", String.valueOf(u), String.valueOf(v));
        }
    }

    static long INF = 1000000000L;
    static Edge[][] adjacency_list;
    static Pair[] mst_edges;

    static String pairs_to_string(Pair[] edges) {
        String s = "[";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(edges.length)) {
            Pair e_1 = edges[(int)((long)(i_1))];
            s = s + "(" + _p(e_1.u) + ", " + _p(e_1.v) + ")";
            if ((long)(i_1) < (long)((long)(edges.length) - 1L)) {
                s = s + ", ";
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s + "]";
    }

    static Pair[] prim_mst(Edge[][] graph) {
        long n = (long)(graph.length);
        boolean[] visited_1 = ((boolean[])(new boolean[]{}));
        long[] dist_1 = ((long[])(new long[]{}));
        long[] parent_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(n)) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
            dist_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dist_1), java.util.stream.LongStream.of((long)(INF))).toArray()));
            parent_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
dist_1[(int)((long)(0))] = 0L;
        Pair[] result_1 = ((Pair[])(new Pair[]{}));
        long count_1 = 0L;
        while ((long)(count_1) < (long)(n)) {
            long min_val_1 = (long)(INF);
            long u_1 = 0L;
            long v_1 = 0L;
            while ((long)(v_1) < (long)(n)) {
                if ((visited_1[(int)((long)(v_1))] == false) && (long)(dist_1[(int)((long)(v_1))]) < (long)(min_val_1)) {
                    min_val_1 = (long)(dist_1[(int)((long)(v_1))]);
                    u_1 = (long)(v_1);
                }
                v_1 = (long)((long)(v_1) + 1L);
            }
            if ((long)(min_val_1) == (long)(INF)) {
                break;
            }
visited_1[(int)((long)(u_1))] = true;
            if ((long)(u_1) != 0L) {
                result_1 = ((Pair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new Pair(parent_1[(int)((long)(u_1))], u_1))).toArray(Pair[]::new)));
            }
            for (Edge e : graph[(int)((long)(u_1))]) {
                if ((visited_1[(int)((long)(e.to))] == false) && (long)(e.weight) < (long)(dist_1[(int)((long)(e.to))])) {
dist_1[(int)((long)(e.to))] = (long)(e.weight);
parent_1[(int)((long)(e.to))] = (long)(u_1);
                }
            }
            count_1 = (long)((long)(count_1) + 1L);
        }
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            adjacency_list = ((Edge[][])(new Edge[][]{new Edge[]{new Edge(1, 1), new Edge(3, 3)}, new Edge[]{new Edge(0, 1), new Edge(2, 6), new Edge(3, 5), new Edge(4, 1)}, new Edge[]{new Edge(1, 6), new Edge(4, 5), new Edge(5, 2)}, new Edge[]{new Edge(0, 3), new Edge(1, 5), new Edge(4, 1)}, new Edge[]{new Edge(1, 1), new Edge(2, 5), new Edge(3, 1), new Edge(5, 4)}, new Edge[]{new Edge(2, 2), new Edge(4, 4)}}));
            mst_edges = ((Pair[])(prim_mst(((Edge[][])(adjacency_list)))));
            System.out.println(pairs_to_string(((Pair[])(mst_edges))));
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
}
