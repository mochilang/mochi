public class Main {
    static int[][] g1;
    static int[][] g2;

    static boolean depth_first_search(int[][] graph, int vertex, boolean[] visited, boolean[] rec_stk) {
visited[vertex] = true;
rec_stk[vertex] = true;
        for (int node : graph[vertex]) {
            if (!(Boolean)visited[node]) {
                if (((Boolean)(depth_first_search(((int[][])(graph)), node, ((boolean[])(visited)), ((boolean[])(rec_stk)))))) {
                    return true;
                }
            } else             if (((Boolean)(rec_stk[node]))) {
                return true;
            }
        }
rec_stk[vertex] = false;
        return false;
    }

    static boolean check_cycle(int[][] graph) {
        int n = graph.length;
        boolean[] visited = ((boolean[])(new boolean[]{}));
        boolean[] rec_stk = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i < n) {
            visited = ((boolean[])(appendBool(visited, false)));
            rec_stk = ((boolean[])(appendBool(rec_stk, false)));
            i = i + 1;
        }
        i = 0;
        while (i < n) {
            if (!(Boolean)visited[i]) {
                if (((Boolean)(depth_first_search(((int[][])(graph)), i, ((boolean[])(visited)), ((boolean[])(rec_stk)))))) {
                    return true;
                }
            }
            i = i + 1;
        }
        return false;
    }

    static void print_bool(boolean b) {
        if (((Boolean)(b))) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g1 = ((int[][])(new int[][]{new int[]{}, new int[]{0, 3}, new int[]{0, 4}, new int[]{5}, new int[]{5}, new int[]{}}));
            print_bool(check_cycle(((int[][])(g1))));
            g2 = ((int[][])(new int[][]{new int[]{1, 2}, new int[]{2}, new int[]{0, 3}, new int[]{3}}));
            print_bool(check_cycle(((int[][])(g2))));
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
