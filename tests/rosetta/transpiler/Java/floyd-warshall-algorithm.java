public class Main {

    static void main() {
        int INF = 1000000000;
        int n = 4;
        int[][] dist = new int[][]{};
        int[][][] next = new int[1][][];
        next[0] = new int[][]{};
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
            next[0] = appendObj(next[0], nrow);
            i = i + 1;
        }
dist[0][2] = -2;
next[0][0][2] = 2;
dist[2][3] = 2;
next[0][2][3] = 3;
dist[3][1] = -1;
next[0][3][1] = 1;
dist[1][0] = 4;
next[0][1][0] = 0;
dist[1][2] = 3;
next[0][1][2] = 2;
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
next[0][i_1][j_1] = next[0][i_1][k];
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
        if (next[0][ui][vi] == 0 - 1) {
            return new int[]{};
        }
        int[] p = new int[]{u};
        int cur = ui;
        while (cur != vi) {
            cur = next[0][cur][vi];
            p = java.util.stream.IntStream.concat(java.util.Arrays.stream(p), java.util.stream.IntStream.of(cur + 1)).toArray();
        }
        return p;
};
        java.util.function.Function<int[],String> pathStr = (p_1) -> {
        String s = "";
        boolean first = true;
        int idx = 0;
        while (idx < p_1.length) {
            int x = p_1[idx];
            if (!first) {
                s = s + " -> ";
            }
            s = s + String.valueOf(x);
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
                    System.out.println(String.valueOf(a + 1) + " -> " + String.valueOf(b + 1) + "\t" + String.valueOf(dist[a][b]) + "\t" + String.valueOf(pathStr.apply(path.apply(a + 1, b + 1))));
                }
                b = b + 1;
            }
            a = a + 1;
        }
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
