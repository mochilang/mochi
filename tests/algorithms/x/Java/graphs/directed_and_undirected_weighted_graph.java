public class Main {
    static class DirectedGraph {
        java.util.Map<Integer,int[][]> graph;
        DirectedGraph(java.util.Map<Integer,int[][]> graph) {
            this.graph = graph;
        }
        DirectedGraph() {}
        @Override public String toString() {
            return String.format("{'graph': %s}", String.valueOf(graph));
        }
    }

    static class Graph {
        java.util.Map<Integer,int[][]> graph;
        Graph(java.util.Map<Integer,int[][]> graph) {
            this.graph = graph;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'graph': %s}", String.valueOf(graph));
        }
    }


    static boolean list_contains_int(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean edge_exists(int[][] edges, int w, int v) {
        int i_1 = 0;
        while (i_1 < edges.length) {
            if (edges[i_1][0] == w && edges[i_1][1] == v) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static int first_key(java.util.Map<Integer,int[][]> m) {
        for (int k : m.keySet()) {
            return k;
        }
        return 0;
    }

    static int rand_range(int low, int high) {
        return (Math.floorMod(_now(), (high - low))) + low;
    }

    static DirectedGraph dg_make_graph() {
        return new DirectedGraph(new java.util.LinkedHashMap<Integer, int[][]>());
    }

    static void dg_add_pair(DirectedGraph g, int u, int v, int w) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            int[][] edges = (int[][])(((int[][])(g.graph).get(u)));
            if (!(Boolean)edge_exists(((int[][])(edges)), w, v)) {
                edges = ((int[][])(appendObj(edges, new int[]{w, v})));
                java.util.Map<Integer,int[][]> m = g.graph;
m.put(u, ((int[][])(edges)));
g.graph = m;
            }
        } else {
            java.util.Map<Integer,int[][]> m0 = g.graph;
m0.put(u, ((int[][])(new int[][]{new int[]{w, v}})));
g.graph = m0;
        }
        if (!(Boolean)(g.graph.containsKey(v))) {
            java.util.Map<Integer,int[][]> m1 = g.graph;
m1.put(v, ((int[][])(new int[][]{})));
g.graph = m1;
        }
    }

    static void dg_remove_pair(DirectedGraph g, int u, int v) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            int[][] edges_1 = (int[][])(((int[][])(g.graph).get(u)));
            int[][] new_edges = ((int[][])(new int[][]{}));
            int i_2 = 0;
            while (i_2 < edges_1.length) {
                if (edges_1[i_2][1] != v) {
                    new_edges = ((int[][])(appendObj(new_edges, edges_1[i_2])));
                }
                i_2 = i_2 + 1;
            }
            java.util.Map<Integer,int[][]> m_1 = g.graph;
m_1.put(u, ((int[][])(new_edges)));
g.graph = m_1;
        }
    }

    static int[] dg_all_nodes(DirectedGraph g) {
        int[] res = ((int[])(new int[]{}));
        for (int k : g.graph.keySet()) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(((Number)(k)).intValue())).toArray()));
        }
        return res;
    }

    static int[] dg_dfs_util(DirectedGraph g, int node, java.util.Map<Integer,Boolean> visited, int[] order, int d) {
visited.put(node, true);
        order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(node)).toArray()));
        if (d != (-1) && node == d) {
            return order;
        }
        int[][] edges_2 = (int[][])(((int[][])(g.graph).get(node)));
        int i_3 = 0;
        while (i_3 < edges_2.length) {
            int neigh = edges_2[i_3][1];
            if (!(Boolean)(visited.containsKey(neigh))) {
                order = ((int[])(dg_dfs_util(g, neigh, visited, ((int[])(order)), d)));
                if (d != (-1) && order[order.length - 1] == d) {
                    return order;
                }
            }
            i_3 = i_3 + 1;
        }
        return order;
    }

    static int[] dg_dfs(DirectedGraph g, int s, int d) {
        if (s == d) {
            return new int[]{};
        }
        int start = s == (-2) ? first_key(g.graph) : s;
        java.util.Map<Integer,Boolean> visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] order = ((int[])(new int[]{}));
        order = ((int[])(dg_dfs_util(g, start, visited, ((int[])(order)), d)));
        return order;
    }

    static int[] dg_bfs(DirectedGraph g, int s) {
        int[] queue = ((int[])(new int[]{}));
        java.util.Map<Integer,Boolean> visited_1 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] order_1 = ((int[])(new int[]{}));
        int start_1 = s == (-2) ? first_key(g.graph) : s;
        queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(start_1)).toArray()));
visited_1.put(start_1, true);
        while (queue.length > 0) {
            int node = queue[0];
            queue = ((int[])(java.util.Arrays.copyOfRange(queue, 1, queue.length)));
            order_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order_1), java.util.stream.IntStream.of(node)).toArray()));
            int[][] edges_3 = (int[][])(((int[][])(g.graph).get(node)));
            int i_4 = 0;
            while (i_4 < edges_3.length) {
                int neigh_1 = edges_3[i_4][1];
                if (!(Boolean)(visited_1.containsKey(neigh_1))) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(neigh_1)).toArray()));
visited_1.put(neigh_1, true);
                }
                i_4 = i_4 + 1;
            }
        }
        return order_1;
    }

    static int dg_in_degree(DirectedGraph g, int u) {
        int count = 0;
        for (int k : g.graph.keySet()) {
            int[][] edges_4 = (int[][])(((int[][])(g.graph).get(k)));
            int i_5 = 0;
            while (i_5 < edges_4.length) {
                if (edges_4[i_5][1] == u) {
                    count = count + 1;
                }
                i_5 = i_5 + 1;
            }
        }
        return count;
    }

    static int dg_out_degree(DirectedGraph g, int u) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            return ((int[][])(g.graph).get(u)).length;
        }
        return 0;
    }

    static int[] dg_topo_util(DirectedGraph g, int node, java.util.Map<Integer,Boolean> visited, int[] stack) {
visited.put(node, true);
        int[][] edges_5 = (int[][])(((int[][])(g.graph).get(node)));
        int i_6 = 0;
        while (i_6 < edges_5.length) {
            int neigh_2 = edges_5[i_6][1];
            if (!(Boolean)(visited.containsKey(neigh_2))) {
                stack = ((int[])(dg_topo_util(g, neigh_2, visited, ((int[])(stack)))));
            }
            i_6 = i_6 + 1;
        }
        stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(node)).toArray()));
        return stack;
    }

    static int[] dg_topological_sort(DirectedGraph g) {
        java.util.Map<Integer,Boolean> visited_2 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] stack = ((int[])(new int[]{}));
        for (int k : g.graph.keySet()) {
            if (!(Boolean)(visited_2.containsKey(k))) {
                stack = ((int[])(dg_topo_util(g, ((Number)(k)).intValue(), visited_2, ((int[])(stack)))));
            }
        }
        int[] res_1 = ((int[])(new int[]{}));
        int i_7 = stack.length - 1;
        while (i_7 >= 0) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(stack[i_7])).toArray()));
            i_7 = i_7 - 1;
        }
        return res_1;
    }

    static int[] dg_cycle_util(DirectedGraph g, int node, java.util.Map<Integer,Boolean> visited, java.util.Map<Integer,Boolean> rec, int[] res) {
visited.put(node, true);
rec.put(node, true);
        int[][] edges_6 = (int[][])(((int[][])(g.graph).get(node)));
        int i_8 = 0;
        while (i_8 < edges_6.length) {
            int neigh_3 = edges_6[i_8][1];
            if (!(Boolean)(visited.containsKey(neigh_3))) {
                res = ((int[])(dg_cycle_util(g, neigh_3, visited, rec, ((int[])(res)))));
            } else             if (((boolean)(rec).getOrDefault(neigh_3, false))) {
                if (!(Boolean)list_contains_int(((int[])(res)), neigh_3)) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(neigh_3)).toArray()));
                }
                if (!(Boolean)list_contains_int(((int[])(res)), node)) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(node)).toArray()));
                }
            }
            i_8 = i_8 + 1;
        }
rec.put(node, false);
        return res;
    }

    static int[] dg_cycle_nodes(DirectedGraph g) {
        java.util.Map<Integer,Boolean> visited_3 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        java.util.Map<Integer,Boolean> rec = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] res_2 = ((int[])(new int[]{}));
        for (int k : g.graph.keySet()) {
            if (!(Boolean)(visited_3.containsKey(k))) {
                res_2 = ((int[])(dg_cycle_util(g, ((Number)(k)).intValue(), visited_3, rec, ((int[])(res_2)))));
            }
        }
        return res_2;
    }

    static boolean dg_has_cycle_util(DirectedGraph g, int node, java.util.Map<Integer,Boolean> visited, java.util.Map<Integer,Boolean> rec) {
visited.put(node, true);
rec.put(node, true);
        int[][] edges_7 = (int[][])(((int[][])(g.graph).get(node)));
        int i_9 = 0;
        while (i_9 < edges_7.length) {
            int neigh_4 = edges_7[i_9][1];
            if (!(Boolean)(visited.containsKey(neigh_4))) {
                if (((Boolean)(dg_has_cycle_util(g, neigh_4, visited, rec)))) {
                    return true;
                }
            } else             if (((boolean)(rec).getOrDefault(neigh_4, false))) {
                return true;
            }
            i_9 = i_9 + 1;
        }
rec.put(node, false);
        return false;
    }

    static boolean dg_has_cycle(DirectedGraph g) {
        java.util.Map<Integer,Boolean> visited_4 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        java.util.Map<Integer,Boolean> rec_1 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        for (int k : g.graph.keySet()) {
            if (!(Boolean)(visited_4.containsKey(k))) {
                if (((Boolean)(dg_has_cycle_util(g, ((Number)(k)).intValue(), visited_4, rec_1)))) {
                    return true;
                }
            }
        }
        return false;
    }

    static void dg_fill_graph_randomly(DirectedGraph g, int c) {
        int count_1 = c;
        if (count_1 == (-1)) {
            count_1 = rand_range(10, 10010);
        }
        int i_10 = 0;
        while (i_10 < count_1) {
            int edge_count = rand_range(1, 103);
            int j = 0;
            while (j < edge_count) {
                int n = rand_range(0, count_1);
                if (n != i_10) {
                    dg_add_pair(g, i_10, n, 1);
                }
                j = j + 1;
            }
            i_10 = i_10 + 1;
        }
    }

    static int dg_dfs_time(DirectedGraph g, int s, int e) {
        int begin = _now();
        dg_dfs(g, s, e);
        int end = _now();
        return end - begin;
    }

    static int dg_bfs_time(DirectedGraph g, int s) {
        int begin_1 = _now();
        dg_bfs(g, s);
        int end_1 = _now();
        return end_1 - begin_1;
    }

    static Graph g_make_graph() {
        return new Graph(new java.util.LinkedHashMap<Integer, int[][]>());
    }

    static void g_add_pair(Graph g, int u, int v, int w) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            int[][] edges_8 = (int[][])(((int[][])(g.graph).get(u)));
            if (!(Boolean)edge_exists(((int[][])(edges_8)), w, v)) {
                edges_8 = ((int[][])(appendObj(edges_8, new int[]{w, v})));
                java.util.Map<Integer,int[][]> m_2 = g.graph;
m_2.put(u, ((int[][])(edges_8)));
g.graph = m_2;
            }
        } else {
            java.util.Map<Integer,int[][]> m0_1 = g.graph;
m0_1.put(u, ((int[][])(new int[][]{new int[]{w, v}})));
g.graph = m0_1;
        }
        if (((Boolean)(g.graph.containsKey(v)))) {
            int[][] edges2 = (int[][])(((int[][])(g.graph).get(v)));
            if (!(Boolean)edge_exists(((int[][])(edges2)), w, u)) {
                edges2 = ((int[][])(appendObj(edges2, new int[]{w, u})));
                java.util.Map<Integer,int[][]> m2 = g.graph;
m2.put(v, ((int[][])(edges2)));
g.graph = m2;
            }
        } else {
            java.util.Map<Integer,int[][]> m3 = g.graph;
m3.put(v, ((int[][])(new int[][]{new int[]{w, u}})));
g.graph = m3;
        }
    }

    static void g_remove_pair(Graph g, int u, int v) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            int[][] edges_9 = (int[][])(((int[][])(g.graph).get(u)));
            int[][] new_edges_1 = ((int[][])(new int[][]{}));
            int i_11 = 0;
            while (i_11 < edges_9.length) {
                if (edges_9[i_11][1] != v) {
                    new_edges_1 = ((int[][])(appendObj(new_edges_1, edges_9[i_11])));
                }
                i_11 = i_11 + 1;
            }
            java.util.Map<Integer,int[][]> m_3 = g.graph;
m_3.put(u, ((int[][])(new_edges_1)));
g.graph = m_3;
        }
        if (((Boolean)(g.graph.containsKey(v)))) {
            int[][] edges2_1 = (int[][])(((int[][])(g.graph).get(v)));
            int[][] new_edges2 = ((int[][])(new int[][]{}));
            int j_1 = 0;
            while (j_1 < edges2_1.length) {
                if (edges2_1[j_1][1] != u) {
                    new_edges2 = ((int[][])(appendObj(new_edges2, edges2_1[j_1])));
                }
                j_1 = j_1 + 1;
            }
            java.util.Map<Integer,int[][]> m2_1 = g.graph;
m2_1.put(v, ((int[][])(new_edges2)));
g.graph = m2_1;
        }
    }

    static int[] g_all_nodes(Graph g) {
        int[] res_3 = ((int[])(new int[]{}));
        for (int k : g.graph.keySet()) {
            res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(((Number)(k)).intValue())).toArray()));
        }
        return res_3;
    }

    static int[] g_dfs_util(Graph g, int node, java.util.Map<Integer,Boolean> visited, int[] order, int d) {
visited.put(node, true);
        order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(node)).toArray()));
        if (d != (-1) && node == d) {
            return order;
        }
        int[][] edges_10 = (int[][])(((int[][])(g.graph).get(node)));
        int i_12 = 0;
        while (i_12 < edges_10.length) {
            int neigh_5 = edges_10[i_12][1];
            if (!(Boolean)(visited.containsKey(neigh_5))) {
                order = ((int[])(g_dfs_util(g, neigh_5, visited, ((int[])(order)), d)));
                if (d != (-1) && order[order.length - 1] == d) {
                    return order;
                }
            }
            i_12 = i_12 + 1;
        }
        return order;
    }

    static int[] g_dfs(Graph g, int s, int d) {
        if (s == d) {
            return new int[]{};
        }
        int start_2 = s == (-2) ? first_key(g.graph) : s;
        java.util.Map<Integer,Boolean> visited_5 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] order_2 = ((int[])(new int[]{}));
        order_2 = ((int[])(g_dfs_util(g, start_2, visited_5, ((int[])(order_2)), d)));
        return order_2;
    }

    static int[] g_bfs(Graph g, int s) {
        int[] queue_1 = ((int[])(new int[]{}));
        java.util.Map<Integer,Boolean> visited_6 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] order_3 = ((int[])(new int[]{}));
        int start_3 = s == (-2) ? first_key(g.graph) : s;
        queue_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.IntStream.of(start_3)).toArray()));
visited_6.put(start_3, true);
        while (queue_1.length > 0) {
            int node_1 = queue_1[0];
            queue_1 = ((int[])(java.util.Arrays.copyOfRange(queue_1, 1, queue_1.length)));
            order_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order_3), java.util.stream.IntStream.of(node_1)).toArray()));
            int[][] edges_11 = (int[][])(((int[][])(g.graph).get(node_1)));
            int i_13 = 0;
            while (i_13 < edges_11.length) {
                int neigh_6 = edges_11[i_13][1];
                if (!(Boolean)(visited_6.containsKey(neigh_6))) {
                    queue_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.IntStream.of(neigh_6)).toArray()));
visited_6.put(neigh_6, true);
                }
                i_13 = i_13 + 1;
            }
        }
        return order_3;
    }

    static int g_degree(Graph g, int u) {
        if (((Boolean)(g.graph.containsKey(u)))) {
            return ((int[][])(g.graph).get(u)).length;
        }
        return 0;
    }

    static int[] g_cycle_util(Graph g, int node, java.util.Map<Integer,Boolean> visited, int parent, int[] res) {
visited.put(node, true);
        int[][] edges_12 = (int[][])(((int[][])(g.graph).get(node)));
        int i_14 = 0;
        while (i_14 < edges_12.length) {
            int neigh_7 = edges_12[i_14][1];
            if (!(Boolean)(visited.containsKey(neigh_7))) {
                res = ((int[])(g_cycle_util(g, neigh_7, visited, node, ((int[])(res)))));
            } else             if (neigh_7 != parent) {
                if (!(Boolean)list_contains_int(((int[])(res)), neigh_7)) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(neigh_7)).toArray()));
                }
                if (!(Boolean)list_contains_int(((int[])(res)), node)) {
                    res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(node)).toArray()));
                }
            }
            i_14 = i_14 + 1;
        }
        return res;
    }

    static int[] g_cycle_nodes(Graph g) {
        java.util.Map<Integer,Boolean> visited_7 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] res_4 = ((int[])(new int[]{}));
        for (int k : g.graph.keySet()) {
            if (!(Boolean)(visited_7.containsKey(k))) {
                res_4 = ((int[])(g_cycle_util(g, ((Number)(k)).intValue(), visited_7, -1, ((int[])(res_4)))));
            }
        }
        return res_4;
    }

    static boolean g_has_cycle_util(Graph g, int node, java.util.Map<Integer,Boolean> visited, int parent) {
visited.put(node, true);
        int[][] edges_13 = (int[][])(((int[][])(g.graph).get(node)));
        int i_15 = 0;
        while (i_15 < edges_13.length) {
            int neigh_8 = edges_13[i_15][1];
            if (!(Boolean)(visited.containsKey(neigh_8))) {
                if (((Boolean)(g_has_cycle_util(g, neigh_8, visited, node)))) {
                    return true;
                }
            } else             if (neigh_8 != parent) {
                return true;
            }
            i_15 = i_15 + 1;
        }
        return false;
    }

    static boolean g_has_cycle(Graph g) {
        java.util.Map<Integer,Boolean> visited_8 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        for (int k : g.graph.keySet()) {
            if (!(Boolean)(visited_8.containsKey(k))) {
                if (((Boolean)(g_has_cycle_util(g, ((Number)(k)).intValue(), visited_8, -1)))) {
                    return true;
                }
            }
        }
        return false;
    }

    static void g_fill_graph_randomly(Graph g, int c) {
        int count_2 = c;
        if (count_2 == (-1)) {
            count_2 = rand_range(10, 10010);
        }
        int i_16 = 0;
        while (i_16 < count_2) {
            int edge_count_1 = rand_range(1, 103);
            int j_2 = 0;
            while (j_2 < edge_count_1) {
                int n_1 = rand_range(0, count_2);
                if (n_1 != i_16) {
                    g_add_pair(g, i_16, n_1, 1);
                }
                j_2 = j_2 + 1;
            }
            i_16 = i_16 + 1;
        }
    }

    static int g_dfs_time(Graph g, int s, int e) {
        int begin_2 = _now();
        g_dfs(g, s, e);
        int end_2 = _now();
        return end_2 - begin_2;
    }

    static int g_bfs_time(Graph g, int s) {
        int begin_3 = _now();
        g_bfs(g, s);
        int end_3 = _now();
        return end_3 - begin_3;
    }

    static void main() {
        DirectedGraph dg = dg_make_graph();
        dg_add_pair(dg, 0, 1, 5);
        dg_add_pair(dg, 0, 2, 3);
        dg_add_pair(dg, 1, 3, 2);
        dg_add_pair(dg, 2, 3, 4);
        System.out.println(_p(dg_dfs(dg, -2, -1)));
        System.out.println(_p(dg_bfs(dg, -2)));
        System.out.println(_p(dg_in_degree(dg, 3)));
        System.out.println(_p(dg_out_degree(dg, 0)));
        System.out.println(_p(dg_topological_sort(dg)));
        System.out.println(_p(dg_has_cycle(dg)));
        Graph ug = g_make_graph();
        g_add_pair(ug, 0, 1, 1);
        g_add_pair(ug, 1, 2, 1);
        g_add_pair(ug, 2, 0, 1);
        System.out.println(_p(g_dfs(ug, -2, -1)));
        System.out.println(_p(g_bfs(ug, -2)));
        System.out.println(_p(g_degree(ug, 1)));
        System.out.println(_p(g_has_cycle(ug)));
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
