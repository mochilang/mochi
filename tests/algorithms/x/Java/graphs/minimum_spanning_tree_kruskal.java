public class Main {
    static int[][] edges1;
    static int[][] edges2;
    static int[][] edges3;

    static int[][] sort_edges(int[][] edges) {
        int[][] es = ((int[][])(edges));
        int i = 0;
        while (i < es.length) {
            int j = 0;
            while (j < es.length - i - 1) {
                if (es[j][2] > es[j + 1][2]) {
                    int[] temp = ((int[])(es[j]));
es[j] = ((int[])(es[j + 1]));
es[j + 1] = ((int[])(temp));
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return es;
    }

    static int find_parent(int[] parent, int i) {
        if (parent[i] != i) {
parent[i] = find_parent(((int[])(parent)), parent[i]);
        }
        return parent[i];
    }

    static int[][] kruskal(int num_nodes, int[][] edges) {
        int[][] es_1 = ((int[][])(sort_edges(((int[][])(edges)))));
        int[] parent = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < num_nodes) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        int[][] mst = ((int[][])(new int[][]{}));
        int idx = 0;
        while (idx < es_1.length) {
            int[] e = ((int[])(es_1[idx]));
            int pa = find_parent(((int[])(parent)), e[0]);
            int pb = find_parent(((int[])(parent)), e[1]);
            if (pa != pb) {
                mst = ((int[][])(appendObj(mst, e)));
parent[pa] = pb;
            }
            idx = idx + 1;
        }
        return mst;
    }

    static String edges_to_string(int[][] es) {
        String s = "[";
        int i_2 = 0;
        while (i_2 < es.length) {
            int[] e_1 = ((int[])(es[i_2]));
            s = s + "(" + _p(_geti(e_1, 0)) + ", " + _p(_geti(e_1, 1)) + ", " + _p(_geti(e_1, 2)) + ")";
            if (i_2 < es.length - 1) {
                s = s + ", ";
            }
            i_2 = i_2 + 1;
        }
        s = s + "]";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            edges1 = ((int[][])(new int[][]{new int[]{0, 1, 3}, new int[]{1, 2, 5}, new int[]{2, 3, 1}}));
            System.out.println(edges_to_string(((int[][])(kruskal(4, ((int[][])(edges1)))))));
            edges2 = ((int[][])(new int[][]{new int[]{0, 1, 3}, new int[]{1, 2, 5}, new int[]{2, 3, 1}, new int[]{0, 2, 1}, new int[]{0, 3, 2}}));
            System.out.println(edges_to_string(((int[][])(kruskal(4, ((int[][])(edges2)))))));
            edges3 = ((int[][])(new int[][]{new int[]{0, 1, 3}, new int[]{1, 2, 5}, new int[]{2, 3, 1}, new int[]{0, 2, 1}, new int[]{0, 3, 2}, new int[]{2, 1, 1}}));
            System.out.println(edges_to_string(((int[][])(kruskal(4, ((int[][])(edges3)))))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
