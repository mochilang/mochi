public class Main {
    static java.util.Map<Integer,int[]> tree = null;

    static int[] dfs(int start, java.util.Map<Integer,Boolean> visited) {
        int size = 1;
        int cuts = 0;
visited.put(start, true);
        for (int v : ((int[])(tree).get(start))) {
            if (!(Boolean)(visited.containsKey(v))) {
                int[] res = ((int[])(dfs(v, visited)));
                size = size + res[0];
                cuts = cuts + res[1];
            }
        }
        if (Math.floorMod(size, 2) == 0) {
            cuts = cuts + 1;
        }
        return new int[]{size, cuts};
    }

    static int even_tree() {
        java.util.Map<Integer,Boolean> visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] res_1 = ((int[])(dfs(1, visited)));
        return res_1[1] - 1;
    }

    static void main() {
        int[][] edges = ((int[][])(new int[][]{new int[]{2, 1}, new int[]{3, 1}, new int[]{4, 3}, new int[]{5, 2}, new int[]{6, 1}, new int[]{7, 2}, new int[]{8, 6}, new int[]{9, 8}, new int[]{10, 8}}));
        int i = 0;
        while (i < edges.length) {
            int u = edges[i][0];
            int v = edges[i][1];
            if (!(Boolean)(tree.containsKey(u))) {
tree.put(u, ((int[])(new int[]{})));
            }
            if (!(Boolean)(tree.containsKey(v))) {
tree.put(v, ((int[])(new int[]{})));
            }
tree.put(u, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(((int[])(tree).get(u))), java.util.stream.IntStream.of(v)).toArray())));
tree.put(v, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(((int[])(tree).get(v))), java.util.stream.IntStream.of(u)).toArray())));
            i = i + 1;
        }
        System.out.println(_p(even_tree()));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tree = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
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
