public class Main {
    static long[][] graph;
    static long[] distances_2;

    static long minimum_distance(long[] distances, boolean[] visited) {
        long minimum = 10000000L;
        long min_index_1 = 0L;
        long vertex_1 = 0L;
        while ((long)(vertex_1) < (long)(distances.length)) {
            if ((long)(distances[(int)((long)(vertex_1))]) < (long)(minimum) && (visited[(int)((long)(vertex_1))] == false)) {
                minimum = (long)(distances[(int)((long)(vertex_1))]);
                min_index_1 = (long)(vertex_1);
            }
            vertex_1 = (long)((long)(vertex_1) + 1L);
        }
        return min_index_1;
    }

    static long[] dijkstra(long[][] graph, long source) {
        long vertices = (long)(graph.length);
        long[] distances_1 = new long[0];
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vertices)) {
            distances_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(distances_1), java.util.stream.LongStream.of(10000000L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
distances_1[(int)((long)(source))] = 0L;
        boolean[] visited_1 = new boolean[0];
        i_1 = 0L;
        while ((long)(i_1) < (long)(vertices)) {
            visited_1 = ((boolean[])(appendBool(visited_1, false)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long count_1 = 0L;
        while ((long)(count_1) < (long)(vertices)) {
            long u_1 = (long)(minimum_distance(((long[])(distances_1)), ((boolean[])(visited_1))));
visited_1[(int)((long)(u_1))] = true;
            long v_1 = 0L;
            while ((long)(v_1) < (long)(vertices)) {
                if ((long)(graph[(int)((long)(u_1))][(int)((long)(v_1))]) > 0L && (visited_1[(int)((long)(v_1))] == false) && (long)(distances_1[(int)((long)(v_1))]) > (long)((long)(distances_1[(int)((long)(u_1))]) + (long)(graph[(int)((long)(u_1))][(int)((long)(v_1))]))) {
distances_1[(int)((long)(v_1))] = (long)((long)(distances_1[(int)((long)(u_1))]) + (long)(graph[(int)((long)(u_1))][(int)((long)(v_1))]));
                }
                v_1 = (long)((long)(v_1) + 1L);
            }
            count_1 = (long)((long)(count_1) + 1L);
        }
        return distances_1;
    }

    static void print_solution(long[] distances) {
        System.out.println("Vertex \t Distance from Source");
        long v_3 = 0L;
        while ((long)(v_3) < (long)(distances.length)) {
            System.out.println(_p(v_3) + "\t\t" + _p(_geti(distances, ((Number)(v_3)).intValue())));
            v_3 = (long)((long)(v_3) + 1L);
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((long[][])(new long[][]{new long[]{0, 4, 0, 0, 0, 0, 0, 8, 0}, new long[]{4, 0, 8, 0, 0, 0, 0, 11, 0}, new long[]{0, 8, 0, 7, 0, 4, 0, 0, 2}, new long[]{0, 0, 7, 0, 9, 14, 0, 0, 0}, new long[]{0, 0, 0, 9, 0, 10, 0, 0, 0}, new long[]{0, 0, 4, 14, 10, 0, 2, 0, 0}, new long[]{0, 0, 0, 0, 0, 2, 0, 1, 6}, new long[]{8, 11, 0, 0, 0, 0, 1, 0, 7}, new long[]{0, 0, 2, 0, 0, 0, 6, 7, 0}}));
            distances_2 = ((long[])(dijkstra(((long[][])(graph)), 0L)));
            print_solution(((long[])(distances_2)));
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
