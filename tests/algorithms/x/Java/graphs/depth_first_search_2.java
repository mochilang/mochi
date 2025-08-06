public class Main {
    static class Graph {
        java.util.Map<Integer,int[]> vertex;
        int size;
        Graph(java.util.Map<Integer,int[]> vertex, int size) {
            this.vertex = vertex;
            this.size = size;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'vertex': %s, 'size': %s}", String.valueOf(vertex), String.valueOf(size));
        }
    }

    static Graph g = null;

    static Graph add_edge(Graph g, int from_vertex, int to_vertex) {
        java.util.Map<Integer,int[]> v = g.vertex;
        if (((Boolean)(v.containsKey(from_vertex)))) {
            int[] lst = (int[])(((int[])(v).get(from_vertex)));
            lst = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lst), java.util.stream.IntStream.of(to_vertex)).toArray()));
v.put(from_vertex, ((int[])(lst)));
        } else {
v.put(from_vertex, ((int[])(new int[]{to_vertex})));
        }
g.vertex = v;
        if (from_vertex + 1 > g.size) {
g.size = from_vertex + 1;
        }
        if (to_vertex + 1 > g.size) {
g.size = to_vertex + 1;
        }
        return g;
    }

    static String list_to_string(int[] lst) {
        String res = "";
        int i = 0;
        while (i < lst.length) {
            res = res + _p(_geti(lst, i));
            if (i < lst.length - 1) {
                res = res + " ";
            }
            i = i + 1;
        }
        return res;
    }

    static String list_to_arrow(int[] lst) {
        String res_1 = "";
        int i_1 = 0;
        while (i_1 < lst.length) {
            res_1 = res_1 + _p(_geti(lst, i_1));
            if (i_1 < lst.length - 1) {
                res_1 = res_1 + " -> ";
            }
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static void print_graph(Graph g) {
        System.out.println(_p(g.vertex));
        int i_2 = 0;
        while (i_2 < g.size) {
            int[] edges = ((int[])(new int[]{}));
            if (((Boolean)(g.vertex.containsKey(i_2)))) {
                edges = (int[])(((int[])(g.vertex).get(i_2)));
            }
            String line = _p(i_2) + "  ->  " + String.valueOf(list_to_arrow(((int[])(edges))));
            System.out.println(line);
            i_2 = i_2 + 1;
        }
    }

    static int[] dfs_recursive(Graph g, int start_vertex, boolean[] visited, int[] order) {
visited[start_vertex] = true;
        order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(start_vertex)).toArray()));
        if (((Boolean)(g.vertex.containsKey(start_vertex)))) {
            int[] neighbors = (int[])(((int[])(g.vertex).get(start_vertex)));
            int i_3 = 0;
            while (i_3 < neighbors.length) {
                int nb = neighbors[i_3];
                if (!(Boolean)visited[nb]) {
                    order = ((int[])(dfs_recursive(g, nb, ((boolean[])(visited)), ((int[])(order)))));
                }
                i_3 = i_3 + 1;
            }
        }
        return order;
    }

    static int[] dfs(Graph g) {
        int n = g.size;
        boolean[] visited = ((boolean[])(new boolean[]{}));
        int i_4 = 0;
        while (i_4 < n) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_4 = i_4 + 1;
        }
        int[] order = ((int[])(new int[]{}));
        i_4 = 0;
        while (i_4 < n) {
            if (!(Boolean)visited[i_4]) {
                order = ((int[])(dfs_recursive(g, i_4, ((boolean[])(visited)), ((int[])(order)))));
            }
            i_4 = i_4 + 1;
        }
        return order;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g = new Graph(new java.util.LinkedHashMap<Integer, int[]>(), 0);
            g = add_edge(g, 0, 1);
            g = add_edge(g, 0, 2);
            g = add_edge(g, 1, 2);
            g = add_edge(g, 2, 0);
            g = add_edge(g, 2, 3);
            g = add_edge(g, 3, 3);
            print_graph(g);
            System.out.println("DFS:");
            System.out.println(list_to_string(((int[])(dfs(g)))));
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
