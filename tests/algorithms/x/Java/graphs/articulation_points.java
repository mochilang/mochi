public class Main {

    static int dfs_skip(int[][] graph, boolean[] visited, int skip, int at) {
visited[at] = true;
        int count = 1;
        for (int to : graph[at]) {
            if (to == skip) {
                continue;
            }
            if (visited[to] == false) {
                count = count + dfs_skip(((int[][])(graph)), ((boolean[])(visited)), skip, to);
            }
        }
        return count;
    }

    static int[] articulation_points(int[][] graph) {
        int n = graph.length;
        int[] result = ((int[])(new int[]{}));
        int v = 0;
        while (v < n) {
            boolean[] visited = ((boolean[])(new boolean[]{}));
            int i = 0;
            while (i < n) {
                visited = ((boolean[])(appendBool(visited, false)));
                i = i + 1;
            }
            int start = 0;
            while (start == v && start < n) {
                start = start + 1;
            }
            int reach = dfs_skip(((int[][])(graph)), ((boolean[])(visited)), v, start);
            if (reach < n - 1) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(v)).toArray()));
                System.out.println(v);
            }
            v = v + 1;
        }
        return result;
    }

    static void main() {
        int[][] graph = ((int[][])(new int[][]{new int[]{1, 2}, new int[]{0, 2}, new int[]{0, 1, 3, 5}, new int[]{2, 4}, new int[]{3}, new int[]{2, 6, 8}, new int[]{5, 7}, new int[]{6, 8}, new int[]{5, 7}}));
        articulation_points(((int[][])(graph)));
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
}
