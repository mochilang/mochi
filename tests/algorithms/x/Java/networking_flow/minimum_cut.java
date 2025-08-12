public class Main {
    static long[][] test_graph;
    static long[][] result;

    static boolean bfs(long[][] graph, long s, long t, long[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(graph.length)) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        long[] queue_1 = ((long[])(new long[]{s}));
        long head_1 = 0L;
visited[(int)((long)(s))] = true;
        while ((long)(head_1) < (long)(queue_1.length)) {
            long u_1 = queue_1[(int)((long)(head_1))];
            head_1 = (long)((long)(head_1) + (long)(1));
            long ind_1 = 0L;
            while ((long)(ind_1) < (long)(graph[(int)((long)(u_1))].length)) {
                if ((visited[(int)((long)(ind_1))] == false) && graph[(int)((long)(u_1))][(int)((long)(ind_1))] > (long)(0)) {
                    queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(ind_1))).toArray()));
visited[(int)((long)(ind_1))] = true;
parent[(int)((long)(ind_1))] = u_1;
                }
                ind_1 = (long)((long)(ind_1) + (long)(1));
            }
        }
        return visited[(int)((long)(t))];
    }

    static long[][] mincut(long[][] graph, long source, long sink) {
        long[][] g = ((long[][])(graph));
        long[] parent_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(g.length)) {
            parent_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent_1), java.util.stream.LongStream.of((long)(-1))).toArray()));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        long[][] temp_1 = ((long[][])(new long[][]{}));
        i_3 = (long)(0);
        while ((long)(i_3) < (long)(g.length)) {
            long[] row_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(g[(int)((long)(i_3))].length)) {
                row_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(row_1), java.util.stream.LongStream.of(g[(int)((long)(i_3))][(int)((long)(j_1))])).toArray()));
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            temp_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_1), java.util.stream.Stream.of(row_1)).toArray(long[][]::new)));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        while (bfs(((long[][])(g)), source, sink, ((long[])(parent_1)))) {
            long path_flow_1 = 1000000000L;
            long s_1 = sink;
            while ((long)(s_1) != source) {
                long p_1 = parent_1[(int)((long)(s_1))];
                long cap_1 = g[(int)((long)(p_1))][(int)((long)(s_1))];
                if (cap_1 < (long)(path_flow_1)) {
                    path_flow_1 = cap_1;
                }
                s_1 = p_1;
            }
            long v_1 = sink;
            while ((long)(v_1) != source) {
                long u_3 = parent_1[(int)((long)(v_1))];
g[(int)((long)(u_3))][(int)((long)(v_1))] = (long)(g[(int)((long)(u_3))][(int)((long)(v_1))] - (long)(path_flow_1));
g[(int)((long)(v_1))][(int)((long)(u_3))] = (long)(g[(int)((long)(v_1))][(int)((long)(u_3))] + (long)(path_flow_1));
                v_1 = u_3;
            }
        }
        long[][] res_1 = ((long[][])(new long[][]{}));
        i_3 = (long)(0);
        while ((long)(i_3) < (long)(g.length)) {
            long j_3 = 0L;
            while ((long)(j_3) < (long)(g[(int)((long)(0))].length)) {
                if (g[(int)((long)(i_3))][(int)((long)(j_3))] == (long)(0) && temp_1[(int)((long)(i_3))][(int)((long)(j_3))] > (long)(0)) {
                    res_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new long[]{i_3, j_3})).toArray(long[][]::new)));
                }
                j_3 = (long)((long)(j_3) + (long)(1));
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return res_1;
    }
    public static void main(String[] args) {
        test_graph = ((long[][])(new long[][]{new long[]{0, 16, 13, 0, 0, 0}, new long[]{0, 0, 10, 12, 0, 0}, new long[]{0, 4, 0, 0, 14, 0}, new long[]{0, 0, 9, 0, 0, 20}, new long[]{0, 0, 0, 7, 0, 4}, new long[]{0, 0, 0, 0, 0, 0}}));
        result = ((long[][])(mincut(((long[][])(test_graph)), 0L, 5L)));
        System.out.println(_p(result));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
