public class Main {
    static int INF;
    static class Dinic {
        int n;
        int[] lvl;
        int[] ptr;
        int[] q;
        int[][][] adj;
        Dinic(int n, int[] lvl, int[] ptr, int[] q, int[][][] adj) {
            this.n = n;
            this.lvl = lvl;
            this.ptr = ptr;
            this.q = q;
            this.adj = adj;
        }
        Dinic() {}
        @Override public String toString() {
            return String.format("{'n': %s, 'lvl': %s, 'ptr': %s, 'q': %s, 'adj': %s}", String.valueOf(n), String.valueOf(lvl), String.valueOf(ptr), String.valueOf(q), String.valueOf(adj));
        }
    }

    static Dinic graph = null;
    static int source;
    static int sink;
    static int v_1 = 0;

    static int pow2(int k) {
        int res = 1;
        int i = 0;
        while (i < k) {
            res = res * 2;
            i = i + 1;
        }
        return res;
    }

    static int min2(int a, int b) {
        if (a < b) {
            return a;
        }
        return b;
    }

    static Dinic new_dinic(int n) {
        int[] lvl = ((int[])(new int[]{}));
        int[] ptr = ((int[])(new int[]{}));
        int[] q = ((int[])(new int[]{}));
        int[][][] adj = ((int[][][])(new int[][][]{}));
        int i_1 = 0;
        while (i_1 < n) {
            lvl = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lvl), java.util.stream.IntStream.of(0)).toArray()));
            ptr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ptr), java.util.stream.IntStream.of(0)).toArray()));
            q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(0)).toArray()));
            int[][] edges = ((int[][])(new int[][]{}));
            adj = ((int[][][])(appendObj(adj, edges)));
            i_1 = i_1 + 1;
        }
        return new Dinic(n, lvl, ptr, q, adj);
    }

    static void add_edge(Dinic g, int a, int b, int c, int rcap) {
        int[][][] adj_1 = ((int[][][])(g.adj));
        int[][] list_a = ((int[][])(adj_1[a]));
        int[][] list_b = ((int[][])(adj_1[b]));
        int[] e1 = ((int[])(new int[]{b, list_b.length, c, 0}));
        int[] e2 = ((int[])(new int[]{a, list_a.length, rcap, 0}));
        list_a = ((int[][])(appendObj(list_a, e1)));
        list_b = ((int[][])(appendObj(list_b, e2)));
adj_1[a] = ((int[][])(list_a));
adj_1[b] = ((int[][])(list_b));
g.adj = adj_1;
    }

    static int dfs(Dinic g, int v, int sink, int flow) {
        if (v == sink || flow == 0) {
            return flow;
        }
        int[] ptr_1 = ((int[])(g.ptr));
        int i_2 = ptr_1[v];
        int[][][] adj_all = ((int[][][])(g.adj));
        int[][] adj_v = ((int[][])(adj_all[v]));
        while (i_2 < adj_v.length) {
            int[] e = ((int[])(adj_v[i_2]));
            int to = e[0];
            if (g.lvl[to] == g.lvl[v] + 1) {
                int avail = e[2] - e[3];
                int pushed = dfs(g, to, sink, min2(flow, avail));
                if (pushed > 0) {
e[3] = e[3] + pushed;
adj_v[i_2] = ((int[])(e));
                    int[][] adj_to = ((int[][])(adj_all[to]));
                    int[] back = ((int[])(adj_to[e[1]]));
back[3] = back[3] - pushed;
adj_to[e[1]] = ((int[])(back));
adj_all[to] = ((int[][])(adj_to));
adj_all[v] = ((int[][])(adj_v));
g.adj = adj_all;
                    return pushed;
                }
            }
            i_2 = i_2 + 1;
ptr_1[v] = i_2;
        }
g.ptr = ptr_1;
adj_all[v] = ((int[][])(adj_v));
g.adj = adj_all;
        return 0;
    }

    static int max_flow(Dinic g, int source, int sink) {
        int flow = 0;
        int l = 0;
        while (l < 31) {
            int threshold = pow2(30 - l);
            while (true) {
                int[] lvl_1 = ((int[])(new int[]{}));
                int[] ptr_2 = ((int[])(new int[]{}));
                int i_3 = 0;
                while (i_3 < g.n) {
                    lvl_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(lvl_1), java.util.stream.IntStream.of(0)).toArray()));
                    ptr_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ptr_2), java.util.stream.IntStream.of(0)).toArray()));
                    i_3 = i_3 + 1;
                }
g.lvl = lvl_1;
g.ptr = ptr_2;
                int qi = 0;
                int qe = 1;
lvl_1[source] = 1;
g.lvl = lvl_1;
                int[] q_1 = ((int[])(g.q));
q_1[0] = source;
                while (qi < qe && g.lvl[sink] == 0) {
                    int v = q_1[qi];
                    qi = qi + 1;
                    int[][] edges_1 = ((int[][])(g.adj[v]));
                    int j = 0;
                    while (j < edges_1.length) {
                        int[] e_1 = ((int[])(edges_1[j]));
                        int to_1 = e_1[0];
                        int residual = e_1[2] - e_1[3];
                        int[] lvl_inner = ((int[])(g.lvl));
                        if (lvl_inner[to_1] == 0 && residual >= threshold) {
q_1[qe] = to_1;
                            qe = qe + 1;
lvl_inner[to_1] = lvl_inner[v] + 1;
g.lvl = lvl_inner;
                        }
                        j = j + 1;
                    }
                }
                int p = dfs(g, source, sink, INF);
                while (p > 0) {
                    flow = flow + p;
                    p = dfs(g, source, sink, INF);
                }
                if (g.lvl[sink] == 0) {
                    break;
                }
            }
            l = l + 1;
        }
        return flow;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
            graph = new_dinic(10);
            source = 0;
            sink = 9;
            v_1 = 1;
            while (v_1 < 5) {
                add_edge(graph, source, v_1, 1, 0);
                v_1 = v_1 + 1;
            }
            v_1 = 5;
            while (v_1 < 9) {
                add_edge(graph, v_1, sink, 1, 0);
                v_1 = v_1 + 1;
            }
            v_1 = 1;
            while (v_1 < 5) {
                add_edge(graph, v_1, v_1 + 4, 1, 0);
                v_1 = v_1 + 1;
            }
            System.out.println(_p(max_flow(graph, source, sink)));
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
