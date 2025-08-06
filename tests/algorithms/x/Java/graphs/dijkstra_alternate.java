public class Main {
    static int[][] graph;
    static int[] distances_1;

    static int minimum_distance(int[] distances, boolean[] visited) {
        int minimum = 10000000;
        int min_index = 0;
        int vertex = 0;
        while (vertex < distances.length) {
            if (distances[vertex] < minimum && visited[vertex] == false) {
                minimum = distances[vertex];
                min_index = vertex;
            }
            vertex = vertex + 1;
        }
        return min_index;
    }

    static int[] dijkstra(int[][] graph, int source) {
        int vertices = graph.length;
        int[] distances = new int[0];
        int i = 0;
        while (i < vertices) {
            distances = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(distances), java.util.stream.IntStream.of(10000000)).toArray()));
            i = i + 1;
        }
distances[source] = 0;
        boolean[] visited = new boolean[0];
        i = 0;
        while (i < vertices) {
            visited = ((boolean[])(appendBool(visited, false)));
            i = i + 1;
        }
        int count = 0;
        while (count < vertices) {
            int u = minimum_distance(((int[])(distances)), ((boolean[])(visited)));
visited[u] = true;
            int v = 0;
            while (v < vertices) {
                if (graph[u][v] > 0 && visited[v] == false && distances[v] > distances[u] + graph[u][v]) {
distances[v] = distances[u] + graph[u][v];
                }
                v = v + 1;
            }
            count = count + 1;
        }
        return distances;
    }

    static void print_solution(int[] distances) {
        System.out.println("Vertex \t Distance from Source");
        int v_1 = 0;
        while (v_1 < distances.length) {
            System.out.println(_p(v_1) + "\t\t" + _p(_geti(distances, v_1)));
            v_1 = v_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((int[][])(new int[][]{new int[]{0, 4, 0, 0, 0, 0, 0, 8, 0}, new int[]{4, 0, 8, 0, 0, 0, 0, 11, 0}, new int[]{0, 8, 0, 7, 0, 4, 0, 0, 2}, new int[]{0, 0, 7, 0, 9, 14, 0, 0, 0}, new int[]{0, 0, 0, 9, 0, 10, 0, 0, 0}, new int[]{0, 0, 4, 14, 10, 0, 2, 0, 0}, new int[]{0, 0, 0, 0, 0, 2, 0, 1, 6}, new int[]{8, 11, 0, 0, 0, 0, 1, 0, 7}, new int[]{0, 0, 2, 0, 0, 0, 6, 7, 0}}));
            distances_1 = ((int[])(dijkstra(((int[][])(graph)), 0)));
            print_solution(((int[])(distances_1)));
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
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
