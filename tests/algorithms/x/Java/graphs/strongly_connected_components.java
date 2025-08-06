public class Main {

    static int[] topology_sort(int[][] graph, int vert, boolean[] visited) {
visited[vert] = true;
        int[] order = ((int[])(new int[]{}));
        for (int neighbour : graph[vert]) {
            if (!(Boolean)visited[neighbour]) {
                order = ((int[])(concat(order, topology_sort(((int[][])(graph)), neighbour, ((boolean[])(visited))))));
            }
        }
        order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(vert)).toArray()));
        return order;
    }

    static int[] find_component(int[][] graph, int vert, boolean[] visited) {
visited[vert] = true;
        int[] comp = ((int[])(new int[]{vert}));
        for (int neighbour : graph[vert]) {
            if (!(Boolean)visited[neighbour]) {
                comp = ((int[])(concat(comp, find_component(((int[][])(graph)), neighbour, ((boolean[])(visited))))));
            }
        }
        return comp;
    }

    static int[][] strongly_connected_components(int[][] graph) {
        int n = graph.length;
        boolean[] visited = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited = ((boolean[])(appendBool(visited, false)));
        }
        int[][] reversed = ((int[][])(new int[][]{}));
        for (int _v = 0; _v < n; _v++) {
            reversed = ((int[][])(appendObj(reversed, new int[]{})));
        }
        for (int i = 0; i < n; i++) {
            for (int neighbour : graph[i]) {
reversed[neighbour] = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(reversed[neighbour]), java.util.stream.IntStream.of(i)).toArray()));
            }
        }
        int[] order_1 = ((int[])(new int[]{}));
        for (int i = 0; i < n; i++) {
            if (!(Boolean)visited[i]) {
                order_1 = ((int[])(concat(order_1, topology_sort(((int[][])(graph)), i, ((boolean[])(visited))))));
            }
        }
        visited = ((boolean[])(new boolean[]{}));
        for (int _v = 0; _v < n; _v++) {
            visited = ((boolean[])(appendBool(visited, false)));
        }
        int[][] components = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int v = order_1[n - i - 1];
            if (!(Boolean)visited[v]) {
                int[] comp_1 = ((int[])(find_component(((int[][])(reversed)), v, ((boolean[])(visited)))));
                components = ((int[][])(appendObj(components, comp_1)));
            }
            i = i + 1;
        }
        return components;
    }

    static void main() {
        int[][] test_graph_1 = ((int[][])(new int[][]{new int[]{2, 3}, new int[]{0}, new int[]{1}, new int[]{4}, new int[]{}}));
        int[][] test_graph_2 = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{2}, new int[]{0}, new int[]{4}, new int[]{5}, new int[]{3}}));
        System.out.println(_p(strongly_connected_components(((int[][])(test_graph_1)))));
        System.out.println(_p(strongly_connected_components(((int[][])(test_graph_2)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
