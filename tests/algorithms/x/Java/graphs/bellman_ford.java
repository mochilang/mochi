public class Main {
    static class Edge {
        int src;
        int dst;
        int weight;
        Edge(int src, int dst, int weight) {
            this.src = src;
            this.dst = dst;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'src': %s, 'dst': %s, 'weight': %s}", String.valueOf(src), String.valueOf(dst), String.valueOf(weight));
        }
    }

    static double INF;
    static Edge[] edges;
    static double[] distances;

    static String list_to_string(double[] arr) {
        String s = "[";
        int i = 0;
        while (i < arr.length) {
            s = s + _p(_geto(arr, i));
            if (i < arr.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        return s + "]";
    }

    static boolean check_negative_cycle(Edge[] graph, double[] distance, int edge_count) {
        int j = 0;
        while (j < edge_count) {
            Edge e = graph[j];
            int u = e.src;
            int v = e.dst;
            double w = ((Number)(e.weight)).doubleValue();
            if (distance[u] < INF && distance[u] + w < distance[v]) {
                return true;
            }
            j = j + 1;
        }
        return false;
    }

    static double[] bellman_ford(Edge[] graph, int vertex_count, int edge_count, int src) {
        double[] distance = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < vertex_count) {
            distance = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(distance), java.util.stream.DoubleStream.of(INF)).toArray()));
            i_1 = i_1 + 1;
        }
distance[src] = 0.0;
        int k = 0;
        while (k < vertex_count - 1) {
            int j_1 = 0;
            while (j_1 < edge_count) {
                Edge e_1 = graph[j_1];
                int u_1 = e_1.src;
                int v_1 = e_1.dst;
                double w_1 = ((Number)(e_1.weight)).doubleValue();
                if (distance[u_1] < INF && distance[u_1] + w_1 < distance[v_1]) {
distance[v_1] = distance[u_1] + w_1;
                }
                j_1 = j_1 + 1;
            }
            k = k + 1;
        }
        if (((Boolean)(check_negative_cycle(((Edge[])(graph)), ((double[])(distance)), edge_count)))) {
            throw new RuntimeException(String.valueOf("Negative cycle found"));
        }
        return distance;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000.0;
            edges = ((Edge[])(new Edge[]{new Edge(2, 1, -10), new Edge(3, 2, 3), new Edge(0, 3, 5), new Edge(0, 1, 4)}));
            distances = ((double[])(bellman_ford(((Edge[])(edges)), 4, edges.length, 0)));
            System.out.println(list_to_string(((double[])(distances))));
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
        return String.valueOf(v);
    }

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
