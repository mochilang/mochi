public class Main {

    static int pow2(int exp) {
        int res = 1;
        int i = 0;
        while (i < exp) {
            res = res * 2;
            i = i + 1;
        }
        return res;
    }

    static int[][] create_sparse(int max_node, int[][] parent) {
        int j = 1;
        while (pow2(j) < max_node) {
            int i_1 = 1;
            while (i_1 <= max_node) {
parent[j][i_1] = parent[j - 1][parent[j - 1][i_1]];
                i_1 = i_1 + 1;
            }
            j = j + 1;
        }
        return parent;
    }

    static int lowest_common_ancestor(int u, int v, int[] level, int[][] parent) {
        if (level[u] < level[v]) {
            int temp = u;
            u = v;
            v = temp;
        }
        int i_2 = 18;
        while (i_2 >= 0) {
            if (level[u] - pow2(i_2) >= level[v]) {
                u = parent[i_2][u];
            }
            i_2 = i_2 - 1;
        }
        if (u == v) {
            return u;
        }
        i_2 = 18;
        while (i_2 >= 0) {
            int pu = parent[i_2][u];
            int pv = parent[i_2][v];
            if (pu != 0 && pu != pv) {
                u = pu;
                v = pv;
            }
            i_2 = i_2 - 1;
        }
        return parent[0][u];
    }

    static void breadth_first_search(int[] level, int[][] parent, int max_node, java.util.Map<Integer,int[]> graph, int root) {
level[root] = 0;
        int[] q = ((int[])(new int[]{}));
        q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(root)).toArray()));
        int head = 0;
        while (head < q.length) {
            int u = q[head];
            head = head + 1;
            int[] adj = (int[])(((int[])(graph).get(u)));
            int j_1 = 0;
            while (j_1 < adj.length) {
                int v = adj[j_1];
                if (level[v] == 0 - 1) {
level[v] = level[u] + 1;
parent[0][v] = u;
                    q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(v)).toArray()));
                }
                j_1 = j_1 + 1;
            }
        }
    }

    static void main() {
        int max_node = 13;
        int[][] parent = ((int[][])(new int[][]{}));
        int i_3 = 0;
        while (i_3 < 20) {
            int[] row = ((int[])(new int[]{}));
            int j_2 = 0;
            while (j_2 < max_node + 10) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j_2 = j_2 + 1;
            }
            parent = ((int[][])(appendObj(parent, row)));
            i_3 = i_3 + 1;
        }
        int[] level = ((int[])(new int[]{}));
        i_3 = 0;
        while (i_3 < max_node + 10) {
            level = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(level), java.util.stream.IntStream.of(0 - 1)).toArray()));
            i_3 = i_3 + 1;
        }
        java.util.Map<Integer,int[]> graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
graph.put(1, ((int[])(new int[]{2, 3, 4})));
graph.put(2, ((int[])(new int[]{5})));
graph.put(3, ((int[])(new int[]{6, 7})));
graph.put(4, ((int[])(new int[]{8})));
graph.put(5, ((int[])(new int[]{9, 10})));
graph.put(6, ((int[])(new int[]{11})));
graph.put(7, ((int[])(new int[]{})));
graph.put(8, ((int[])(new int[]{12, 13})));
graph.put(9, ((int[])(new int[]{})));
graph.put(10, ((int[])(new int[]{})));
graph.put(11, ((int[])(new int[]{})));
graph.put(12, ((int[])(new int[]{})));
graph.put(13, ((int[])(new int[]{})));
        breadth_first_search(((int[])(level)), ((int[][])(parent)), max_node, graph, 1);
        parent = ((int[][])(create_sparse(max_node, ((int[][])(parent)))));
        System.out.println("LCA of node 1 and 3 is: " + _p(lowest_common_ancestor(1, 3, ((int[])(level)), ((int[][])(parent)))));
        System.out.println("LCA of node 5 and 6 is: " + _p(lowest_common_ancestor(5, 6, ((int[])(level)), ((int[][])(parent)))));
        System.out.println("LCA of node 7 and 11 is: " + _p(lowest_common_ancestor(7, 11, ((int[])(level)), ((int[][])(parent)))));
        System.out.println("LCA of node 6 and 7 is: " + _p(lowest_common_ancestor(6, 7, ((int[])(level)), ((int[][])(parent)))));
        System.out.println("LCA of node 4 and 12 is: " + _p(lowest_common_ancestor(4, 12, ((int[])(level)), ((int[][])(parent)))));
        System.out.println("LCA of node 8 and 8 is: " + _p(lowest_common_ancestor(8, 8, ((int[])(level)), ((int[][])(parent)))));
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
