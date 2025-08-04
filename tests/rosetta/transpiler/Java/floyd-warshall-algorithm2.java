public class Main {
    static int INF;
    static class FWResult {
        int[][] dist;
        int[][] next;
        FWResult(int[][] dist, int[][] next) {
            this.dist = dist;
            this.next = next;
        }
        @Override public String toString() {
            return String.format("{'dist': %s, 'next': %s}", String.valueOf(dist), String.valueOf(next));
        }
    }

    static int n_1;
    static int[][] g;
    static FWResult res;
    static int i_3;

    static FWResult floydWarshall(int[][] graph) {
        int n = graph.length;
        int[][] dist = ((int[][])(new int[][]{}));
        int[][] next = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < n) {
            int[] drow = ((int[])(new int[]{}));
            int[] nrow = ((int[])(new int[]{}));
            int j = 0;
            while (j < n) {
                drow = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(drow), java.util.stream.IntStream.of(graph[i][j])).toArray()));
                if (graph[i][j] < INF && i != j) {
                    nrow = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nrow), java.util.stream.IntStream.of(j)).toArray()));
                } else {
                    nrow = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nrow), java.util.stream.IntStream.of(-1)).toArray()));
                }
                j = j + 1;
            }
            dist = ((int[][])(appendObj(dist, drow)));
            next = ((int[][])(appendObj(next, nrow)));
            i = i + 1;
        }
        int k = 0;
        while (k < n) {
            int i_1 = 0;
            while (i_1 < n) {
                int j_1 = 0;
                while (j_1 < n) {
                    if (dist[i_1][k] < INF && dist[k][j_1] < INF) {
                        int alt = dist[i_1][k] + dist[k][j_1];
                        if (alt < dist[i_1][j_1]) {
dist[i_1][j_1] = alt;
next[i_1][j_1] = next[i_1][k];
                        }
                    }
                    j_1 = j_1 + 1;
                }
                i_1 = i_1 + 1;
            }
            k = k + 1;
        }
        return new FWResult(dist, next);
    }

    static int[] path(int u, int v, int[][] next) {
        if (next[u][v] < 0) {
            return new int[]{};
        }
        int[] p = ((int[])(new int[]{u}));
        int x = u;
        while (x != v) {
            x = next[x][v];
            p = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(p), java.util.stream.IntStream.of(x)).toArray()));
        }
        return p;
    }

    static String pathStr(int[] p) {
        String s = "";
        int i_2 = 0;
        while (i_2 < p.length) {
            s = s + _p(p[i_2] + 1);
            if (i_2 < p.length - 1) {
                s = s + " -> ";
            }
            i_2 = i_2 + 1;
        }
        return s;
    }
    public static void main(String[] args) {
        INF = 1000000;
        n_1 = 4;
        g = ((int[][])(new int[][]{}));
        for (int i = 0; i < n_1; i++) {
            int[] row = ((int[])(new int[]{}));
            for (int j = 0; j < n_1; j++) {
                if (i == j) {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                } else {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(INF)).toArray()));
                }
            }
            g = ((int[][])(appendObj(g, row)));
        }
g[0][2] = -2;
g[2][3] = 2;
g[3][1] = -1;
g[1][0] = 4;
g[1][2] = 3;
        res = floydWarshall(((int[][])(g)));
        System.out.println("pair\tdist\tpath");
        i_3 = 0;
        while (i_3 < n_1) {
            int j_2 = 0;
            while (j_2 < n_1) {
                if (i_3 != j_2) {
                    int[] p_1 = ((int[])(path(i_3, j_2, ((int[][])(res.next)))));
                    System.out.println(_p(i_3 + 1) + " -> " + _p(j_2 + 1) + "\t" + _p(_geti(res.dist[i_3], j_2)) + "\t" + String.valueOf(pathStr(((int[])(p_1)))));
                }
                j_2 = j_2 + 1;
            }
            i_3 = i_3 + 1;
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
