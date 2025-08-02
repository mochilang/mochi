public class Main {

    static void main() {
        int INF = 1000000000;
        int n = 4;
        int[][] dist = new int[][]{};
        int[][] next = new int[][]{};
        int i = 0;
        while (i < n) {
            int[] row = new int[]{};
            int[] nrow = new int[]{};
            int j = 0;
            while (j < n) {
                if (i == j) {
                    row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray();
                } else {
                    row = java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(INF)).toArray();
                }
                nrow = java.util.stream.IntStream.concat(java.util.Arrays.stream(nrow), java.util.stream.IntStream.of(0 - 1)).toArray();
                j = j + 1;
            }
            dist = appendObj(dist, row);
            next = appendObj(next, nrow);
            i = i + 1;
        }
dist[0][2] = -2;
next[0][2] = 2;
dist[2][3] = 2;
next[2][3] = 3;
dist[3][1] = -1;
next[3][1] = 1;
dist[1][0] = 4;
next[1][0] = 0;
dist[1][2] = 3;
next[1][2] = 2;
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
        java.util.function.BiFunction<Integer,Integer,int[]> path = (u, v) -> {
        int ui = u - 1;
        int vi = v - 1;
        if (next[ui][vi] == 0 - 1) {
            return new int[]{};
        }
        int[] p = new int[]{u};
        int cur = ui;
        while (cur != vi) {
            cur = next[cur][vi];
            p = java.util.stream.IntStream.concat(java.util.Arrays.stream(p), java.util.stream.IntStream.of(cur + 1)).toArray();
        }
        return p;
};
        int[][] next_0 = next;
        java.util.function.Function<int[],String> pathStr = (p_1) -> {
        String s = "";
        boolean first = true;
        int idx = 0;
        while (idx < p_1.length) {
            int x = p_1[idx];
            if (!first) {
                s = s + " -> ";
            }
            s = s + _p(x);
            first = false;
            idx = idx + 1;
        }
        return s;
};
        System.out.println("pair\tdist\tpath");
        int a = 0;
        while (a < n) {
            int b = 0;
            while (b < n) {
                if (a != b) {
                    System.out.println(_p(a + 1) + " -> " + _p(b + 1) + "\t" + _p(_geti(dist[a], b)) + "\t" + String.valueOf(pathStr.apply(path.apply(a + 1, b + 1))));
                }
                b = b + 1;
            }
            a = a + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
