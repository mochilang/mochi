public class Main {
    static class Edge {
        long u;
        long v;
        long w;
        Edge(long u, long v, long w) {
            this.u = u;
            this.v = v;
            this.w = w;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s, 'w': %s}", String.valueOf(u), String.valueOf(v), String.valueOf(w));
        }
    }

    static class Graph {
        Edge[] edges;
        long num_nodes;
        Graph(Edge[] edges, long num_nodes) {
            this.edges = edges;
            this.num_nodes = num_nodes;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'edges': %s, 'num_nodes': %s}", String.valueOf(edges), String.valueOf(num_nodes));
        }
    }

    static class DS {
        long[] parent;
        long[] rank;
        DS(long[] parent, long[] rank) {
            this.parent = parent;
            this.rank = rank;
        }
        DS() {}
        @Override public String toString() {
            return String.format("{'parent': %s, 'rank': %s}", String.valueOf(parent), String.valueOf(rank));
        }
    }

    static class FindResult {
        DS ds;
        long root;
        FindResult(DS ds, long root) {
            this.ds = ds;
            this.root = root;
        }
        FindResult() {}
        @Override public String toString() {
            return String.format("{'ds': %s, 'root': %s}", String.valueOf(ds), String.valueOf(root));
        }
    }


    static Graph new_graph() {
        return new Graph(new Edge[]{}, 0);
    }

    static Graph add_edge(Graph g, long u, long v, long w) {
        Edge[] es = ((Edge[])(g.edges));
        es = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(es), java.util.stream.Stream.of(new Edge(u, v, w))).toArray(Edge[]::new)));
        long n_1 = (long)(g.num_nodes);
        if ((long)(u) > (long)(n_1)) {
            n_1 = (long)(u);
        }
        if ((long)(v) > (long)(n_1)) {
            n_1 = (long)(v);
        }
        return new Graph(es, n_1);
    }

    static DS make_ds(long n) {
        long[] parent = ((long[])(new long[]{}));
        long[] rank_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) <= (long)(n)) {
            parent = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(parent), java.util.stream.LongStream.of((long)(i_1))).toArray()));
            rank_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(rank_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new DS(parent, rank_1);
    }

    static FindResult find_set(DS ds, long x) {
        if ((long)(ds.parent[(int)((long)(x))]) == (long)(x)) {
            return new FindResult(ds, x);
        }
        FindResult res_1 = find_set(ds, (long)(ds.parent[(int)((long)(x))]));
        long[] p_1 = ((long[])(res_1.ds.parent));
p_1[(int)((long)(x))] = (long)(res_1.root);
        return new FindResult(new DS(p_1, res_1.ds.rank), res_1.root);
    }

    static DS union_set(DS ds, long x, long y) {
        FindResult fx = find_set(ds, (long)(x));
        DS ds1_1 = fx.ds;
        long x_root_1 = (long)(fx.root);
        FindResult fy_1 = find_set(ds1_1, (long)(y));
        DS ds2_1 = fy_1.ds;
        long y_root_1 = (long)(fy_1.root);
        if ((long)(x_root_1) == (long)(y_root_1)) {
            return ds2_1;
        }
        long[] p_3 = ((long[])(ds2_1.parent));
        long[] r_1 = ((long[])(ds2_1.rank));
        if ((long)(r_1[(int)((long)(x_root_1))]) > (long)(r_1[(int)((long)(y_root_1))])) {
p_3[(int)((long)(y_root_1))] = (long)(x_root_1);
        } else {
p_3[(int)((long)(x_root_1))] = (long)(y_root_1);
            if ((long)(r_1[(int)((long)(x_root_1))]) == (long)(r_1[(int)((long)(y_root_1))])) {
r_1[(int)((long)(y_root_1))] = (long)((long)(r_1[(int)((long)(y_root_1))]) + 1L);
            }
        }
        return new DS(p_3, r_1);
    }

    static Edge[] sort_edges(Edge[] edges) {
        Edge[] arr = ((Edge[])(edges));
        long i_3 = 1L;
        while ((long)(i_3) < (long)(arr.length)) {
            Edge key_1 = arr[(int)((long)(i_3))];
            long j_1 = (long)((long)(i_3) - 1L);
            while ((long)(j_1) >= 0L) {
                Edge temp_1 = arr[(int)((long)(j_1))];
                if ((long)(temp_1.w) > (long)(key_1.w) || ((long)(temp_1.w) == (long)(key_1.w) && ((long)(temp_1.u) > (long)(key_1.u) || ((long)(temp_1.u) == (long)(key_1.u) && (long)(temp_1.v) > (long)(key_1.v))))) {
arr[(int)((long)((long)(j_1) + 1L))] = temp_1;
                    j_1 = (long)((long)(j_1) - 1L);
                } else {
                    break;
                }
            }
arr[(int)((long)((long)(j_1) + 1L))] = key_1;
            i_3 = (long)((long)(i_3) + 1L);
        }
        return arr;
    }

    static Graph kruskal(Graph g) {
        Edge[] edges = ((Edge[])(sort_edges(((Edge[])(g.edges)))));
        DS ds_1 = make_ds((long)(g.num_nodes));
        Edge[] mst_edges_1 = ((Edge[])(new Edge[]{}));
        long i_5 = 0L;
        long added_1 = 0L;
        while ((long)(added_1) < (long)((long)(g.num_nodes) - 1L) && (long)(i_5) < (long)(edges.length)) {
            Edge e_1 = edges[(int)((long)(i_5))];
            i_5 = (long)((long)(i_5) + 1L);
            FindResult fu_1 = find_set(ds_1, (long)(e_1.u));
            ds_1 = fu_1.ds;
            long ru_1 = (long)(fu_1.root);
            FindResult fv_1 = find_set(ds_1, (long)(e_1.v));
            ds_1 = fv_1.ds;
            long rv_1 = (long)(fv_1.root);
            if ((long)(ru_1) != (long)(rv_1)) {
                mst_edges_1 = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_edges_1), java.util.stream.Stream.of(e_1)).toArray(Edge[]::new)));
                added_1 = (long)((long)(added_1) + 1L);
                ds_1 = union_set(ds_1, (long)(ru_1), (long)(rv_1));
            }
        }
        return new Graph(mst_edges_1, g.num_nodes);
    }

    static void print_mst(Graph g) {
        Edge[] es_1 = ((Edge[])(sort_edges(((Edge[])(g.edges)))));
        for (Edge e : es_1) {
            System.out.println(_p(e.u) + "-" + _p(e.v) + ":" + _p(e.w));
        }
    }

    static void main() {
        Graph g = new_graph();
        g = add_edge(g, 1L, 2L, 1L);
        g = add_edge(g, 2L, 3L, 2L);
        g = add_edge(g, 3L, 4L, 1L);
        g = add_edge(g, 3L, 5L, 100L);
        g = add_edge(g, 4L, 5L, 5L);
        Graph mst_1 = kruskal(g);
        print_mst(mst_1);
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
