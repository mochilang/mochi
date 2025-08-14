public class Main {
    static class DirectedGraph {
        java.util.Map<Long,long[][]> graph;
        DirectedGraph(java.util.Map<Long,long[][]> graph) {
            this.graph = graph;
        }
        DirectedGraph() {}
        @Override public String toString() {
            return String.format("{'graph': %s}", String.valueOf(graph));
        }
    }

    static class Graph {
        java.util.Map<Long,long[][]> graph;
        Graph(java.util.Map<Long,long[][]> graph) {
            this.graph = graph;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'graph': %s}", String.valueOf(graph));
        }
    }


    static boolean list_contains_int(long[] xs, long x) {
        long i = 0L;
        while ((long)(i) < (long)(xs.length)) {
            if ((long)(xs[(int)((long)(i))]) == (long)(x)) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static boolean edge_exists(long[][] edges, long w, long v) {
        long i_1 = 0L;
        while ((long)(i_1) < (long)(edges.length)) {
            if ((long)(edges[(int)((long)(i_1))][(int)((long)(0))]) == (long)(w) && (long)(edges[(int)((long)(i_1))][(int)((long)(1))]) == (long)(v)) {
                return true;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return false;
    }

    static long first_key(java.util.Map<Long,long[][]> m) {
        for (long k : m.keySet()) {
            return k;
        }
        return 0;
    }

    static long rand_range(long low, long high) {
        return (long)((Math.floorMod(_now(), ((long)(high) - (long)(low))))) + (long)(low);
    }

    static DirectedGraph dg_make_graph() {
        return new DirectedGraph(new java.util.LinkedHashMap<Long, long[][]>());
    }

    static void dg_add_pair(DirectedGraph g, long u, long v, long w) {
        if (g.graph.containsKey(u)) {
            long[][] edges = (long[][])(((long[][])(g.graph).get(u)));
            if (!(Boolean)edge_exists(((long[][])(edges)), (long)(w), (long)(v))) {
                edges = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges), java.util.stream.Stream.of(new long[][]{new long[]{w, v}})).toArray(long[][]::new)));
                java.util.Map<Long,long[][]> m = g.graph;
m.put(u, ((long[][])(edges)));
g.graph = m;
            }
        } else {
            java.util.Map<Long,long[][]> m0 = g.graph;
m0.put(u, ((long[][])(new long[][]{new long[]{w, v}})));
g.graph = m0;
        }
        if (!(g.graph.containsKey(v))) {
            java.util.Map<Long,long[][]> m1_1 = g.graph;
m1_1.put(v, ((long[][])(new long[][]{})));
g.graph = m1_1;
        }
    }

    static void dg_remove_pair(DirectedGraph g, long u, long v) {
        if (g.graph.containsKey(u)) {
            long[][] edges_1 = (long[][])(((long[][])(g.graph).get(u)));
            long[][] new_edges = ((long[][])(new long[][]{}));
            long i_2 = 0L;
            while ((long)(i_2) < (long)(edges_1.length)) {
                if ((long)(edges_1[(int)((long)(i_2))][(int)((long)(1))]) != (long)(v)) {
                    new_edges = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_edges), java.util.stream.Stream.of(new long[][]{edges_1[(int)((long)(i_2))]})).toArray(long[][]::new)));
                }
                i_2 = (long)((long)(i_2) + 1L);
            }
            java.util.Map<Long,long[][]> m_1 = g.graph;
m_1.put(u, ((long[][])(new_edges)));
g.graph = m_1;
        }
    }

    static long[] dg_all_nodes(DirectedGraph g) {
        long[] res = ((long[])(new long[]{}));
        for (long k : g.graph.keySet()) {
            res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(k))).toArray()));
        }
        return res;
    }

    static long[] dg_dfs_util(DirectedGraph g, long node, java.util.Map<Long,Boolean> visited, long[] order, long d) {
visited.put(node, true);
        order = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order), java.util.stream.LongStream.of((long)(node))).toArray()));
        if ((long)(d) != (long)((-1)) && (long)(node) == (long)(d)) {
            return order;
        }
        long[][] edges_3 = (long[][])(((long[][])(g.graph).get(node)));
        long i_4 = 0L;
        while ((long)(i_4) < (long)(edges_3.length)) {
            long neigh_1 = (long)(edges_3[(int)((long)(i_4))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_1))) {
                order = ((long[])(dg_dfs_util(g, (long)(neigh_1), visited, ((long[])(order)), (long)(d))));
                if ((long)(d) != (long)((-1)) && (long)(order[(int)((long)((long)(order.length) - 1L))]) == (long)(d)) {
                    return order;
                }
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        return order;
    }

    static long[] dg_dfs(DirectedGraph g, long s, long d) {
        if ((long)(s) == (long)(d)) {
            return new long[]{};
        }
        long start_1 = (long)((long)(s) == (long)((-2)) ? first_key(g.graph) : s);
        java.util.Map<Long,Boolean> visited_1 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] order_1 = ((long[])(new long[]{}));
        order_1 = ((long[])(dg_dfs_util(g, (long)(start_1), visited_1, ((long[])(order_1)), (long)(d))));
        return order_1;
    }

    static long[] dg_bfs(DirectedGraph g, long s) {
        long[] queue = ((long[])(new long[]{}));
        java.util.Map<Long,Boolean> visited_3 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] order_3 = ((long[])(new long[]{}));
        long start_3 = (long)((long)(s) == (long)((-2)) ? first_key(g.graph) : s);
        queue = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue), java.util.stream.LongStream.of((long)(start_3))).toArray()));
visited_3.put(start_3, true);
        while ((long)(queue.length) > 0L) {
            long node_1 = (long)(queue[(int)((long)(0))]);
            queue = ((long[])(java.util.Arrays.copyOfRange(queue, (int)((long)(1)), (int)((long)(queue.length)))));
            order_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_3), java.util.stream.LongStream.of((long)(node_1))).toArray()));
            long[][] edges_5 = (long[][])(((long[][])(g.graph).get(node_1)));
            long i_6 = 0L;
            while ((long)(i_6) < (long)(edges_5.length)) {
                long neigh_3 = (long)(edges_5[(int)((long)(i_6))][(int)((long)(1))]);
                if (!(visited_3.containsKey(neigh_3))) {
                    queue = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue), java.util.stream.LongStream.of((long)(neigh_3))).toArray()));
visited_3.put(neigh_3, true);
                }
                i_6 = (long)((long)(i_6) + 1L);
            }
        }
        return order_3;
    }

    static long dg_in_degree(DirectedGraph g, long u) {
        long count = 0L;
        for (long k : g.graph.keySet()) {
            long[][] edges_7 = (long[][])(((long[][])(g.graph).get(k)));
            long i_8 = 0L;
            while ((long)(i_8) < (long)(edges_7.length)) {
                if ((long)(edges_7[(int)((long)(i_8))][(int)((long)(1))]) == (long)(u)) {
                    count = (long)((long)(count) + 1L);
                }
                i_8 = (long)((long)(i_8) + 1L);
            }
        }
        return count;
    }

    static long dg_out_degree(DirectedGraph g, long u) {
        if (g.graph.containsKey(u)) {
            return ((long[][])(g.graph).get(u)).length;
        }
        return 0;
    }

    static long[] dg_topo_util(DirectedGraph g, long node, java.util.Map<Long,Boolean> visited, long[] stack) {
visited.put(node, true);
        long[][] edges_9 = (long[][])(((long[][])(g.graph).get(node)));
        long i_10 = 0L;
        while ((long)(i_10) < (long)(edges_9.length)) {
            long neigh_5 = (long)(edges_9[(int)((long)(i_10))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_5))) {
                stack = ((long[])(dg_topo_util(g, (long)(neigh_5), visited, ((long[])(stack)))));
            }
            i_10 = (long)((long)(i_10) + 1L);
        }
        stack = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(stack), java.util.stream.LongStream.of((long)(node))).toArray()));
        return stack;
    }

    static long[] dg_topological_sort(DirectedGraph g) {
        java.util.Map<Long,Boolean> visited_4 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] stack_1 = ((long[])(new long[]{}));
        for (long k : g.graph.keySet()) {
            if (!(visited_4.containsKey(k))) {
                stack_1 = ((long[])(dg_topo_util(g, (long)(k), visited_4, ((long[])(stack_1)))));
            }
        }
        long[] res_2 = ((long[])(new long[]{}));
        long i_12 = (long)((long)(stack_1.length) - 1L);
        while ((long)(i_12) >= 0L) {
            res_2 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_2), java.util.stream.LongStream.of((long)(stack_1[(int)((long)(i_12))]))).toArray()));
            i_12 = (long)((long)(i_12) - 1L);
        }
        return res_2;
    }

    static long[] dg_cycle_util(DirectedGraph g, long node, java.util.Map<Long,Boolean> visited, java.util.Map<Long,Boolean> rec, long[] res) {
visited.put(node, true);
rec.put(node, true);
        long[][] edges_11 = (long[][])(((long[][])(g.graph).get(node)));
        long i_14 = 0L;
        while ((long)(i_14) < (long)(edges_11.length)) {
            long neigh_7 = (long)(edges_11[(int)((long)(i_14))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_7))) {
                res = ((long[])(dg_cycle_util(g, (long)(neigh_7), visited, rec, ((long[])(res)))));
            } else             if (((boolean)(rec).getOrDefault(neigh_7, false))) {
                if (!(Boolean)list_contains_int(((long[])(res)), (long)(neigh_7))) {
                    res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(neigh_7))).toArray()));
                }
                if (!(Boolean)list_contains_int(((long[])(res)), (long)(node))) {
                    res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(node))).toArray()));
                }
            }
            i_14 = (long)((long)(i_14) + 1L);
        }
rec.put(node, false);
        return res;
    }

    static long[] dg_cycle_nodes(DirectedGraph g) {
        java.util.Map<Long,Boolean> visited_5 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        java.util.Map<Long,Boolean> rec_1 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] res_4 = ((long[])(new long[]{}));
        for (long k : g.graph.keySet()) {
            if (!(visited_5.containsKey(k))) {
                res_4 = ((long[])(dg_cycle_util(g, (long)(k), visited_5, rec_1, ((long[])(res_4)))));
            }
        }
        return res_4;
    }

    static boolean dg_has_cycle_util(DirectedGraph g, long node, java.util.Map<Long,Boolean> visited, java.util.Map<Long,Boolean> rec) {
visited.put(node, true);
rec.put(node, true);
        long[][] edges_13 = (long[][])(((long[][])(g.graph).get(node)));
        long i_16 = 0L;
        while ((long)(i_16) < (long)(edges_13.length)) {
            long neigh_9 = (long)(edges_13[(int)((long)(i_16))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_9))) {
                if (dg_has_cycle_util(g, (long)(neigh_9), visited, rec)) {
                    return true;
                }
            } else             if (((boolean)(rec).getOrDefault(neigh_9, false))) {
                return true;
            }
            i_16 = (long)((long)(i_16) + 1L);
        }
rec.put(node, false);
        return false;
    }

    static boolean dg_has_cycle(DirectedGraph g) {
        java.util.Map<Long,Boolean> visited_6 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        java.util.Map<Long,Boolean> rec_3 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        for (long k : g.graph.keySet()) {
            if (!(visited_6.containsKey(k))) {
                if (dg_has_cycle_util(g, (long)(k), visited_6, rec_3)) {
                    return true;
                }
            }
        }
        return false;
    }

    static void dg_fill_graph_randomly(DirectedGraph g, long c) {
        long count_1 = (long)(c);
        if ((long)(count_1) == (long)((-1))) {
            count_1 = (long)(rand_range(10L, 10010L));
        }
        long i_18 = 0L;
        while ((long)(i_18) < (long)(count_1)) {
            long edge_count_1 = (long)(rand_range(1L, 103L));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(edge_count_1)) {
                long n_1 = (long)(rand_range(0L, (long)(count_1)));
                if ((long)(n_1) != (long)(i_18)) {
                    dg_add_pair(g, (long)(i_18), (long)(n_1), 1L);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_18 = (long)((long)(i_18) + 1L);
        }
    }

    static long dg_dfs_time(DirectedGraph g, long s, long e) {
        long begin = (long)(_now());
        dg_dfs(g, (long)(s), (long)(e));
        long end_1 = (long)(_now());
        return (long)(end_1) - (long)(begin);
    }

    static long dg_bfs_time(DirectedGraph g, long s) {
        long begin_1 = (long)(_now());
        dg_bfs(g, (long)(s));
        long end_3 = (long)(_now());
        return (long)(end_3) - (long)(begin_1);
    }

    static Graph g_make_graph() {
        return new Graph(new java.util.LinkedHashMap<Long, long[][]>());
    }

    static void g_add_pair(Graph g, long u, long v, long w) {
        if (g.graph.containsKey(u)) {
            long[][] edges_14 = (long[][])(((long[][])(g.graph).get(u)));
            if (!(Boolean)edge_exists(((long[][])(edges_14)), (long)(w), (long)(v))) {
                edges_14 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges_14), java.util.stream.Stream.of(new long[][]{new long[]{w, v}})).toArray(long[][]::new)));
                java.util.Map<Long,long[][]> m_2 = g.graph;
m_2.put(u, ((long[][])(edges_14)));
g.graph = m_2;
            }
        } else {
            java.util.Map<Long,long[][]> m0_1 = g.graph;
m0_1.put(u, ((long[][])(new long[][]{new long[]{w, v}})));
g.graph = m0_1;
        }
        if (g.graph.containsKey(v)) {
            long[][] edges2_1 = (long[][])(((long[][])(g.graph).get(v)));
            if (!(Boolean)edge_exists(((long[][])(edges2_1)), (long)(w), (long)(u))) {
                edges2_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(edges2_1), java.util.stream.Stream.of(new long[][]{new long[]{w, u}})).toArray(long[][]::new)));
                java.util.Map<Long,long[][]> m2_1 = g.graph;
m2_1.put(v, ((long[][])(edges2_1)));
g.graph = m2_1;
            }
        } else {
            java.util.Map<Long,long[][]> m3_1 = g.graph;
m3_1.put(v, ((long[][])(new long[][]{new long[]{w, u}})));
g.graph = m3_1;
        }
    }

    static void g_remove_pair(Graph g, long u, long v) {
        if (g.graph.containsKey(u)) {
            long[][] edges_15 = (long[][])(((long[][])(g.graph).get(u)));
            long[][] new_edges_1 = ((long[][])(new long[][]{}));
            long i_19 = 0L;
            while ((long)(i_19) < (long)(edges_15.length)) {
                if ((long)(edges_15[(int)((long)(i_19))][(int)((long)(1))]) != (long)(v)) {
                    new_edges_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_edges_1), java.util.stream.Stream.of(new long[][]{edges_15[(int)((long)(i_19))]})).toArray(long[][]::new)));
                }
                i_19 = (long)((long)(i_19) + 1L);
            }
            java.util.Map<Long,long[][]> m_3 = g.graph;
m_3.put(u, ((long[][])(new_edges_1)));
g.graph = m_3;
        }
        if (g.graph.containsKey(v)) {
            long[][] edges2_3 = (long[][])(((long[][])(g.graph).get(v)));
            long[][] new_edges2_1 = ((long[][])(new long[][]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(edges2_3.length)) {
                if ((long)(edges2_3[(int)((long)(j_3))][(int)((long)(1))]) != (long)(u)) {
                    new_edges2_1 = ((long[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_edges2_1), java.util.stream.Stream.of(new long[][]{edges2_3[(int)((long)(j_3))]})).toArray(long[][]::new)));
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            java.util.Map<Long,long[][]> m2_3 = g.graph;
m2_3.put(v, ((long[][])(new_edges2_1)));
g.graph = m2_3;
        }
    }

    static long[] g_all_nodes(Graph g) {
        long[] res_5 = ((long[])(new long[]{}));
        for (long k : g.graph.keySet()) {
            res_5 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_5), java.util.stream.LongStream.of((long)(k))).toArray()));
        }
        return res_5;
    }

    static long[] g_dfs_util(Graph g, long node, java.util.Map<Long,Boolean> visited, long[] order, long d) {
visited.put(node, true);
        order = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order), java.util.stream.LongStream.of((long)(node))).toArray()));
        if ((long)(d) != (long)((-1)) && (long)(node) == (long)(d)) {
            return order;
        }
        long[][] edges_17 = (long[][])(((long[][])(g.graph).get(node)));
        long i_21 = 0L;
        while ((long)(i_21) < (long)(edges_17.length)) {
            long neigh_11 = (long)(edges_17[(int)((long)(i_21))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_11))) {
                order = ((long[])(g_dfs_util(g, (long)(neigh_11), visited, ((long[])(order)), (long)(d))));
                if ((long)(d) != (long)((-1)) && (long)(order[(int)((long)((long)(order.length) - 1L))]) == (long)(d)) {
                    return order;
                }
            }
            i_21 = (long)((long)(i_21) + 1L);
        }
        return order;
    }

    static long[] g_dfs(Graph g, long s, long d) {
        if ((long)(s) == (long)(d)) {
            return new long[]{};
        }
        long start_5 = (long)((long)(s) == (long)((-2)) ? first_key(g.graph) : s);
        java.util.Map<Long,Boolean> visited_8 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] order_5 = ((long[])(new long[]{}));
        order_5 = ((long[])(g_dfs_util(g, (long)(start_5), visited_8, ((long[])(order_5)), (long)(d))));
        return order_5;
    }

    static long[] g_bfs(Graph g, long s) {
        long[] queue_1 = ((long[])(new long[]{}));
        java.util.Map<Long,Boolean> visited_10 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] order_7 = ((long[])(new long[]{}));
        long start_7 = (long)((long)(s) == (long)((-2)) ? first_key(g.graph) : s);
        queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(start_7))).toArray()));
visited_10.put(start_7, true);
        while ((long)(queue_1.length) > 0L) {
            long node_3 = (long)(queue_1[(int)((long)(0))]);
            queue_1 = ((long[])(java.util.Arrays.copyOfRange(queue_1, (int)((long)(1)), (int)((long)(queue_1.length)))));
            order_7 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_7), java.util.stream.LongStream.of((long)(node_3))).toArray()));
            long[][] edges_19 = (long[][])(((long[][])(g.graph).get(node_3)));
            long i_23 = 0L;
            while ((long)(i_23) < (long)(edges_19.length)) {
                long neigh_13 = (long)(edges_19[(int)((long)(i_23))][(int)((long)(1))]);
                if (!(visited_10.containsKey(neigh_13))) {
                    queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(neigh_13))).toArray()));
visited_10.put(neigh_13, true);
                }
                i_23 = (long)((long)(i_23) + 1L);
            }
        }
        return order_7;
    }

    static long g_degree(Graph g, long u) {
        if (g.graph.containsKey(u)) {
            return ((long[][])(g.graph).get(u)).length;
        }
        return 0;
    }

    static long[] g_cycle_util(Graph g, long node, java.util.Map<Long,Boolean> visited, long parent, long[] res) {
visited.put(node, true);
        long[][] edges_21 = (long[][])(((long[][])(g.graph).get(node)));
        long i_25 = 0L;
        while ((long)(i_25) < (long)(edges_21.length)) {
            long neigh_15 = (long)(edges_21[(int)((long)(i_25))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_15))) {
                res = ((long[])(g_cycle_util(g, (long)(neigh_15), visited, (long)(node), ((long[])(res)))));
            } else             if ((long)(neigh_15) != (long)(parent)) {
                if (!(Boolean)list_contains_int(((long[])(res)), (long)(neigh_15))) {
                    res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(neigh_15))).toArray()));
                }
                if (!(Boolean)list_contains_int(((long[])(res)), (long)(node))) {
                    res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of((long)(node))).toArray()));
                }
            }
            i_25 = (long)((long)(i_25) + 1L);
        }
        return res;
    }

    static long[] g_cycle_nodes(Graph g) {
        java.util.Map<Long,Boolean> visited_11 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        long[] res_7 = ((long[])(new long[]{}));
        for (long k : g.graph.keySet()) {
            if (!(visited_11.containsKey(k))) {
                res_7 = ((long[])(g_cycle_util(g, (long)(k), visited_11, (long)(-1), ((long[])(res_7)))));
            }
        }
        return res_7;
    }

    static boolean g_has_cycle_util(Graph g, long node, java.util.Map<Long,Boolean> visited, long parent) {
visited.put(node, true);
        long[][] edges_23 = (long[][])(((long[][])(g.graph).get(node)));
        long i_27 = 0L;
        while ((long)(i_27) < (long)(edges_23.length)) {
            long neigh_17 = (long)(edges_23[(int)((long)(i_27))][(int)((long)(1))]);
            if (!(visited.containsKey(neigh_17))) {
                if (g_has_cycle_util(g, (long)(neigh_17), visited, (long)(node))) {
                    return true;
                }
            } else             if ((long)(neigh_17) != (long)(parent)) {
                return true;
            }
            i_27 = (long)((long)(i_27) + 1L);
        }
        return false;
    }

    static boolean g_has_cycle(Graph g) {
        java.util.Map<Long,Boolean> visited_12 = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        for (long k : g.graph.keySet()) {
            if (!(visited_12.containsKey(k))) {
                if (g_has_cycle_util(g, (long)(k), visited_12, (long)(-1))) {
                    return true;
                }
            }
        }
        return false;
    }

    static void g_fill_graph_randomly(Graph g, long c) {
        long count_2 = (long)(c);
        if ((long)(count_2) == (long)((-1))) {
            count_2 = (long)(rand_range(10L, 10010L));
        }
        long i_29 = 0L;
        while ((long)(i_29) < (long)(count_2)) {
            long edge_count_3 = (long)(rand_range(1L, 103L));
            long j_5 = 0L;
            while ((long)(j_5) < (long)(edge_count_3)) {
                long n_3 = (long)(rand_range(0L, (long)(count_2)));
                if ((long)(n_3) != (long)(i_29)) {
                    g_add_pair(g, (long)(i_29), (long)(n_3), 1L);
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            i_29 = (long)((long)(i_29) + 1L);
        }
    }

    static long g_dfs_time(Graph g, long s, long e) {
        long begin_2 = (long)(_now());
        g_dfs(g, (long)(s), (long)(e));
        long end_5 = (long)(_now());
        return (long)(end_5) - (long)(begin_2);
    }

    static long g_bfs_time(Graph g, long s) {
        long begin_3 = (long)(_now());
        g_bfs(g, (long)(s));
        long end_7 = (long)(_now());
        return (long)(end_7) - (long)(begin_3);
    }

    static void main() {
        DirectedGraph dg = dg_make_graph();
        dg_add_pair(dg, 0L, 1L, 5L);
        dg_add_pair(dg, 0L, 2L, 3L);
        dg_add_pair(dg, 1L, 3L, 2L);
        dg_add_pair(dg, 2L, 3L, 4L);
        System.out.println(_p(dg_dfs(dg, (long)(-2), (long)(-1))));
        System.out.println(_p(dg_bfs(dg, (long)(-2))));
        System.out.println(_p(dg_in_degree(dg, 3L)));
        System.out.println(_p(dg_out_degree(dg, 0L)));
        System.out.println(_p(dg_topological_sort(dg)));
        System.out.println(_p(dg_has_cycle(dg)));
        Graph ug_1 = g_make_graph();
        g_add_pair(ug_1, 0L, 1L, 1L);
        g_add_pair(ug_1, 1L, 2L, 1L);
        g_add_pair(ug_1, 2L, 0L, 1L);
        System.out.println(_p(g_dfs(ug_1, (long)(-2), (long)(-1))));
        System.out.println(_p(g_bfs(ug_1, (long)(-2))));
        System.out.println(_p(g_degree(ug_1, 1L)));
        System.out.println(_p(g_has_cycle(ug_1)));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
