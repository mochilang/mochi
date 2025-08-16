public class Main {
    static long INF = 1000000000L;
    static long[][] graph = ((long[][])(new long[][]{new long[]{0, 16, 13, 0, 0, 0}, new long[]{0, 0, 10, 12, 0, 0}, new long[]{0, 4, 0, 0, 14, 0}, new long[]{0, 0, 9, 0, 0, 20}, new long[]{0, 0, 0, 7, 0, 4}, new long[]{0, 0, 0, 0, 0, 0}}));

    static boolean breadth_first_search(long[][] graph, long source, long sink, long[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(graph.length)) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] queue_1 = ((long[])(new long[]{}));
        queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(source))).toArray()));
visited[(int)((long)(source))] = true;
        long head_1 = 0L;
        while ((long)(head_1) < (long)(queue_1.length)) {
            long u_1 = (long)(queue_1[(int)((long)(head_1))]);
            head_1 = (long)((long)(head_1) + 1L);
            long[] row_1 = ((long[])(graph[(int)((long)(u_1))]));
            long ind_1 = 0L;
            while ((long)(ind_1) < (long)(row_1.length)) {
                long capacity_1 = (long)(row_1[(int)((long)(ind_1))]);
                if ((visited[(int)((long)(ind_1))] == false) && (long)(capacity_1) > 0L) {
                    queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(ind_1))).toArray()));
visited[(int)((long)(ind_1))] = true;
parent[(int)((long)(ind_1))] = (long)(u_1);
                }
                ind_1 = (long)((long)(ind_1) + 1L);
            }
        }
        return visited[(int)((long)(sink))];
    }

    static long ford_fulkerson(long[][] graph, long source, long sink) {
        long[] parent = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(graph.length)) {
            parent = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long max_flow_1 = 0L;
        while (breadth_first_search(((long[][])(graph)), (long)(source), (long)(sink), ((long[])(parent)))) {
            long path_flow_1 = (long)(INF);
            long s_1 = (long)(sink);
            while ((long)(s_1) != (long)(source)) {
                long prev_1 = (long)(parent[(int)((long)(s_1))]);
                long cap_1 = (long)(graph[(int)((long)(prev_1))][(int)((long)(s_1))]);
                if ((long)(cap_1) < (long)(path_flow_1)) {
                    path_flow_1 = (long)(cap_1);
                }
                s_1 = (long)(prev_1);
            }
            max_flow_1 = (long)((long)(max_flow_1) + (long)(path_flow_1));
            long v_1 = (long)(sink);
            while ((long)(v_1) != (long)(source)) {
                long u_3 = (long)(parent[(int)((long)(v_1))]);
graph[(int)((long)(u_3))][(int)((long)(v_1))] = (long)((long)(graph[(int)((long)(u_3))][(int)((long)(v_1))]) - (long)(path_flow_1));
graph[(int)((long)(v_1))][(int)((long)(u_3))] = (long)((long)(graph[(int)((long)(v_1))][(int)((long)(u_3))]) + (long)(path_flow_1));
                v_1 = (long)(u_3);
            }
            long j_1 = 0L;
            while ((long)(j_1) < (long)(parent.length)) {
parent[(int)((long)(j_1))] = (long)(-1);
                j_1 = (long)((long)(j_1) + 1L);
            }
        }
        return max_flow_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(ford_fulkerson(((long[][])(graph)), 0L, 5L)));
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
