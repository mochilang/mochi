public class Main {
    static int INF;
    static class Graph {
        int n;
        int[][] dp;
        Graph(int n, int[][] dp) {
            this.n = n;
            this.dp = dp;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'n': %s, 'dp': %s}", String.valueOf(n), String.valueOf(dp));
        }
    }

    static Graph graph = null;

    static Graph new_graph(int n) {
        int[][] dp = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                if (i == j) {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                } else {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(INF)).toArray()));
                }
                j = j + 1;
            }
            dp = ((int[][])(appendObj((int[][])dp, row)));
            i = i + 1;
        }
        return new Graph(n, dp);
    }

    static void add_edge(Graph g, int u, int v, int w) {
        int[][] dp_1 = ((int[][])(g.dp));
        int[] row_1 = ((int[])(dp_1[u]));
row_1[v] = w;
dp_1[u] = ((int[])(row_1));
g.dp = dp_1;
    }

    static void floyd_warshall(Graph g) {
        int[][] dp_2 = ((int[][])(g.dp));
        int k = 0;
        while (k < g.n) {
            int i_1 = 0;
            while (i_1 < g.n) {
                int j_1 = 0;
                while (j_1 < g.n) {
                    int alt = dp_2[i_1][k] + dp_2[k][j_1];
                    int[] row_2 = ((int[])(dp_2[i_1]));
                    if (alt < row_2[j_1]) {
row_2[j_1] = alt;
dp_2[i_1] = ((int[])(row_2));
                    }
                    j_1 = j_1 + 1;
                }
                i_1 = i_1 + 1;
            }
            k = k + 1;
        }
g.dp = dp_2;
    }

    static int show_min(Graph g, int u, int v) {
        return g.dp[u][v];
    }
    public static void main(String[] args) {
        INF = 1000000000;
        graph = new_graph(5);
        add_edge(graph, 0, 2, 9);
        add_edge(graph, 0, 4, 10);
        add_edge(graph, 1, 3, 5);
        add_edge(graph, 2, 3, 7);
        add_edge(graph, 3, 0, 10);
        add_edge(graph, 3, 1, 2);
        add_edge(graph, 3, 2, 1);
        add_edge(graph, 3, 4, 6);
        add_edge(graph, 4, 1, 3);
        add_edge(graph, 4, 2, 4);
        add_edge(graph, 4, 3, 9);
        floyd_warshall(graph);
        System.out.println(_p(show_min(graph, 1, 4)));
        System.out.println(_p(show_min(graph, 0, 3)));
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
