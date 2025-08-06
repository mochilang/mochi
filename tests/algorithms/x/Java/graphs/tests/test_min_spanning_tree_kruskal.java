public class Main {

    static int[][] sort_edges(int[][] edges) {
        int[][] es = ((int[][])(edges));
        int i = 0;
        while (i < es.length) {
            int j = 0;
            while (j < es.length - i - 1) {
                if (es[j][2] > es[j + 1][2]) {
                    int[] tmp = ((int[])(es[j]));
es[j] = ((int[])(es[j + 1]));
es[j + 1] = ((int[])(tmp));
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return es;
    }

    static int find(int[] parent, int x) {
        int r = x;
        while (parent[r] != r) {
            r = parent[r];
        }
        return r;
    }

    static int[][] kruskal(int n, int[][] edges) {
        int[] parent = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        int[][] sorted = ((int[][])(sort_edges(((int[][])(edges)))));
        int[][] mst = ((int[][])(new int[][]{}));
        int e = 0;
        while (e < sorted.length) {
            if (mst.length == n - 1) {
                break;
            }
            int[] edge = ((int[])(sorted[e]));
            e = e + 1;
            int u = edge[0];
            int v = edge[1];
            int w = edge[2];
            int ru = find(((int[])(parent)), u);
            int rv = find(((int[])(parent)), v);
            if (ru != rv) {
parent[ru] = rv;
                mst = ((int[][])(appendObj(mst, new int[]{u, v, w})));
            }
        }
        return mst;
    }

    static boolean edges_equal(int[][] a, int[][] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < a.length) {
            int[] e1 = ((int[])(a[i_2]));
            int[] e2 = ((int[])(b[i_2]));
            if (e1[0] != e2[0] || e1[1] != e2[1] || e1[2] != e2[2]) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static void main() {
        int num_nodes = 9;
        int[][] edges = ((int[][])(new int[][]{new int[]{0, 1, 4}, new int[]{0, 7, 8}, new int[]{1, 2, 8}, new int[]{7, 8, 7}, new int[]{7, 6, 1}, new int[]{2, 8, 2}, new int[]{8, 6, 6}, new int[]{2, 3, 7}, new int[]{2, 5, 4}, new int[]{6, 5, 2}, new int[]{3, 5, 14}, new int[]{3, 4, 9}, new int[]{5, 4, 10}, new int[]{1, 7, 11}}));
        int[][] expected = ((int[][])(new int[][]{new int[]{7, 6, 1}, new int[]{2, 8, 2}, new int[]{6, 5, 2}, new int[]{0, 1, 4}, new int[]{2, 5, 4}, new int[]{2, 3, 7}, new int[]{0, 7, 8}, new int[]{3, 4, 9}}));
        int[][] result = ((int[][])(kruskal(num_nodes, ((int[][])(edges)))));
        int[][] sorted_result = ((int[][])(sort_edges(((int[][])(result)))));
        int[][] sorted_expected = ((int[][])(sort_edges(((int[][])(expected)))));
        System.out.println(_p(sorted_result));
        if (((Boolean)(edges_equal(((int[][])(sorted_expected)), ((int[][])(sorted_result)))))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
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
