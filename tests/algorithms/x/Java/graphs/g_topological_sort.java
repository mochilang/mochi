public class Main {

    static int[] depth_first_search(int u, boolean[] visited, int[][] graph, int[] stack) {
visited[u] = true;
        int i = 0;
        while (i < graph[u].length) {
            int v = graph[u][i];
            if (!(Boolean)visited[v]) {
                stack = ((int[])(depth_first_search(v, ((boolean[])(visited)), ((int[][])(graph)), ((int[])(stack)))));
            }
            i = i + 1;
        }
        stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(u)).toArray()));
        return stack;
    }

    static int[] topological_sort(int[][] graph) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        int i_1 = 0;
        while (i_1 < graph.length) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_1 = i_1 + 1;
        }
        int[] stack = ((int[])(new int[]{}));
        i_1 = 0;
        while (i_1 < graph.length) {
            if (!(Boolean)visited[i_1]) {
                stack = ((int[])(depth_first_search(i_1, ((boolean[])(visited)), ((int[][])(graph)), ((int[])(stack)))));
            }
            i_1 = i_1 + 1;
        }
        return stack;
    }

    static void print_stack(int[] stack, java.util.Map<Integer,String> clothes) {
        int order = 1;
        int[] s = ((int[])(stack));
        while (s.length > 0) {
            int idx = s[s.length - 1];
            s = ((int[])(java.util.Arrays.copyOfRange(s, 0, s.length - 1)));
            System.out.println(_p(order) + " " + ((String)(clothes).get(idx)));
            order = order + 1;
        }
    }

    static String format_list(int[] xs) {
        String res = "[";
        int i_2 = 0;
        while (i_2 < xs.length) {
            res = res + _p(_geti(xs, i_2));
            if (i_2 < xs.length - 1) {
                res = res + ", ";
            }
            i_2 = i_2 + 1;
        }
        res = res + "]";
        return res;
    }

    static void main() {
        java.util.Map<Integer,String> clothes = ((java.util.Map<Integer,String>)(new java.util.LinkedHashMap<Integer, String>(java.util.Map.ofEntries(java.util.Map.entry(0, "underwear"), java.util.Map.entry(1, "pants"), java.util.Map.entry(2, "belt"), java.util.Map.entry(3, "suit"), java.util.Map.entry(4, "shoe"), java.util.Map.entry(5, "socks"), java.util.Map.entry(6, "shirt"), java.util.Map.entry(7, "tie"), java.util.Map.entry(8, "watch")))));
        int[][] graph = ((int[][])(new int[][]{new int[]{1, 4}, new int[]{2, 4}, new int[]{3}, new int[]{}, new int[]{}, new int[]{4}, new int[]{2, 7}, new int[]{3}, new int[]{}}));
        int[] stack_1 = ((int[])(topological_sort(((int[][])(graph)))));
        System.out.println(format_list(((int[])(stack_1))));
        print_stack(((int[])(stack_1)), clothes);
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
