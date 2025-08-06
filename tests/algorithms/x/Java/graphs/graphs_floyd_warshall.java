public class Main {
    static double INF;
    static Object[][] graph;
    static double[][] result;

    static double[][] floyd_warshall(double[][] graph) {
        int v = graph.length;
        double[][] dist = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < v) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < v) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(graph[i][j])).toArray()));
                j = j + 1;
            }
            dist = ((double[][])(appendObj(dist, row)));
            i = i + 1;
        }
        int k = 0;
        while (k < v) {
            int i_1 = 0;
            while (i_1 < v) {
                int j_1 = 0;
                while (j_1 < v) {
                    if (dist[i_1][k] < INF && dist[k][j_1] < INF && dist[i_1][k] + dist[k][j_1] < dist[i_1][j_1]) {
dist[i_1][j_1] = dist[i_1][k] + dist[k][j_1];
                    }
                    j_1 = j_1 + 1;
                }
                i_1 = i_1 + 1;
            }
            k = k + 1;
        }
        return dist;
    }

    static void print_dist(double[][] dist) {
        System.out.println("\nThe shortest path matrix using Floyd Warshall algorithm\n");
        int i_2 = 0;
        while (i_2 < dist.length) {
            int j_2 = 0;
            String line = "";
            while (j_2 < dist[i_2].length) {
                if (dist[i_2][j_2] >= INF / 2.0) {
                    line = line + "INF\t";
                } else {
                    line = line + _p(((Number)(dist[i_2][j_2])).intValue()) + "\t";
                }
                j_2 = j_2 + 1;
            }
            System.out.println(line);
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000.0;
            graph = ((Object[][])(new Object[][]{new Object[]{0.0, 5.0, INF, 10.0}, new Object[]{INF, 0.0, 3.0, INF}, new Object[]{INF, INF, 0.0, 1.0}, new Object[]{INF, INF, INF, 0.0}}));
            result = ((double[][])(floyd_warshall(((double[][])(graph)))));
            print_dist(((double[][])(result)));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        return String.valueOf(v);
    }
}
