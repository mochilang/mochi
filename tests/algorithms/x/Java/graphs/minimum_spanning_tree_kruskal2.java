public class Main {
    static class Edge {
        int u;
        int v;
        int w;
        Edge(int u, int v, int w) {
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
        int num_nodes;
        Graph(Edge[] edges, int num_nodes) {
            this.edges = edges;
            this.num_nodes = num_nodes;
        }
        Graph() {}
        @Override public String toString() {
            return String.format("{'edges': %s, 'num_nodes': %s}", String.valueOf(edges), String.valueOf(num_nodes));
        }
    }

    static class DS {
        int[] parent;
        int[] rank;
        DS(int[] parent, int[] rank) {
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
        int root;
        FindResult(DS ds, int root) {
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

    static Graph add_edge(Graph g, int u, int v, int w) {
        Edge[] es = ((Edge[])(g.edges));
        es = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(es), java.util.stream.Stream.of(new Edge(u, v, w))).toArray(Edge[]::new)));
        int n = g.num_nodes;
        if (u > n) {
            n = u;
        }
        if (v > n) {
            n = v;
        }
        return new Graph(es, n);
    }

    static DS make_ds(int n) {
        int[] parent = ((int[])(new int[]{}));
        int[] rank = ((int[])(new int[]{}));
        int i = 0;
        while (i <= n) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(i)).toArray()));
            rank = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rank), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        return new DS(parent, rank);
    }

    static FindResult find_set(DS ds, int x) {
        if (ds.parent[x] == x) {
            return new FindResult(ds, x);
        }
        FindResult res = find_set(ds, ds.parent[x]);
        int[] p = ((int[])(res.ds.parent));
p[x] = res.root;
        return new FindResult(new DS(p, res.ds.rank), res.root);
    }

    static DS union_set(DS ds, int x, int y) {
        FindResult fx = find_set(ds, x);
        DS ds1 = fx.ds;
        int x_root = fx.root;
        FindResult fy = find_set(ds1, y);
        DS ds2 = fy.ds;
        int y_root = fy.root;
        if (x_root == y_root) {
            return ds2;
        }
        int[] p_1 = ((int[])(ds2.parent));
        int[] r = ((int[])(ds2.rank));
        if (r[x_root] > r[y_root]) {
p_1[y_root] = x_root;
        } else {
p_1[x_root] = y_root;
            if (r[x_root] == r[y_root]) {
r[y_root] = r[y_root] + 1;
            }
        }
        return new DS(p_1, r);
    }

    static Edge[] sort_edges(Edge[] edges) {
        Edge[] arr = ((Edge[])(edges));
        int i_1 = 1;
        while (i_1 < arr.length) {
            Edge key = arr[i_1];
            int j = i_1 - 1;
            while (j >= 0) {
                Edge temp = arr[j];
                if (temp.w > key.w || (temp.w == key.w && (temp.u > key.u || (temp.u == key.u && temp.v > key.v)))) {
arr[j + 1] = temp;
                    j = j - 1;
                } else {
                    break;
                }
            }
arr[j + 1] = key;
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static Graph kruskal(Graph g) {
        Edge[] edges = ((Edge[])(sort_edges(((Edge[])(g.edges)))));
        DS ds = make_ds(g.num_nodes);
        Edge[] mst_edges = ((Edge[])(new Edge[]{}));
        int i_2 = 0;
        int added = 0;
        while (added < g.num_nodes - 1 && i_2 < edges.length) {
            Edge e = edges[i_2];
            i_2 = i_2 + 1;
            FindResult fu = find_set(ds, e.u);
            ds = fu.ds;
            int ru = fu.root;
            FindResult fv = find_set(ds, e.v);
            ds = fv.ds;
            int rv = fv.root;
            if (ru != rv) {
                mst_edges = ((Edge[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_edges), java.util.stream.Stream.of(e)).toArray(Edge[]::new)));
                added = added + 1;
                ds = union_set(ds, ru, rv);
            }
        }
        return new Graph(mst_edges, g.num_nodes);
    }

    static void print_mst(Graph g) {
        Edge[] es_1 = ((Edge[])(sort_edges(((Edge[])(g.edges)))));
        for (Edge e : es_1) {
            System.out.println(_p(e.u) + "-" + _p(e.v) + ":" + _p(e.w));
        }
    }

    static void main() {
        Graph g = new_graph();
        g = add_edge(g, 1, 2, 1);
        g = add_edge(g, 2, 3, 2);
        g = add_edge(g, 3, 4, 1);
        g = add_edge(g, 3, 5, 100);
        g = add_edge(g, 4, 5, 5);
        Graph mst = kruskal(g);
        print_mst(mst);
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
        return String.valueOf(v);
    }
}
