public class Main {
    static java.util.Map<Integer,int[]> g_dfs;
    static java.util.Map<Integer,int[]> g_bfs;
    static java.util.Map<Integer,int[][]> g_weighted;
    static java.util.Map<Integer,int[]> g_topo;
    static int[][] matrix;
    static java.util.Map<Integer,int[][]> g_prim;
    static int[][] edges_kruskal;
    static java.util.Map<Integer,int[]> g_iso;
    static int[] iso;

    static void dfs(java.util.Map<Integer,int[]> g, int s) {
        java.util.Map<Integer,Boolean> visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] stack = ((int[])(new int[]{}));
visited.put(s, true);
        stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(s)).toArray()));
        System.out.println(s);
        while (stack.length > 0) {
            int u = stack[stack.length - 1];
            boolean found = false;
            for (int v : ((int[])(g).get(u))) {
                if (!(Boolean)(visited.containsKey(v))) {
visited.put(v, true);
                    stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(v)).toArray()));
                    System.out.println(v);
                    found = true;
                    break;
                }
            }
            if (!found) {
                stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 1)));
            }
        }
    }

    static void bfs(java.util.Map<Integer,int[]> g, int s) {
        java.util.Map<Integer,Boolean> visited_1 = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        int[] q = ((int[])(new int[]{}));
visited_1.put(s, true);
        q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(s)).toArray()));
        System.out.println(s);
        while (q.length > 0) {
            int u_1 = q[0];
            q = ((int[])(java.util.Arrays.copyOfRange(q, 1, q.length)));
            for (int v : ((int[])(g).get(u_1))) {
                if (!(Boolean)(visited_1.containsKey(v))) {
visited_1.put(v, true);
                    q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(v)).toArray()));
                    System.out.println(v);
                }
            }
        }
    }

    static int[] sort_ints(int[] a) {
        int[] arr = ((int[])(a));
        int i = 0;
        while (i < arr.length) {
            int j = 0;
            while (j < arr.length - i - 1) {
                if (arr[j] > arr[j + 1]) {
                    int tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static void dijkstra(java.util.Map<Integer,int[][]> g, int s) {
        java.util.Map<Integer,Integer> dist = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
dist.put(s, 0);
        java.util.Map<Integer,Integer> path = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
path.put(s, 0);
        int[] known = ((int[])(new int[]{}));
        int[] keys = ((int[])(new int[]{s}));
        while (known.length < keys.length) {
            int mini = 100000;
            int u_2 = -1;
            int i_1 = 0;
            while (i_1 < keys.length) {
                int k = keys[i_1];
                int d = (int)(((int)(dist).getOrDefault(k, 0)));
                if (!(Boolean)(java.util.Arrays.stream(known).anyMatch((x) -> ((Number)(x)).intValue() == k)) && d < mini) {
                    mini = d;
                    u_2 = k;
                }
                i_1 = i_1 + 1;
            }
            known = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(known), java.util.stream.IntStream.of(u_2)).toArray()));
            for (int[] e : ((int[][])(g).get(u_2))) {
                int v = e[0];
                int w = e[1];
                if (!(Boolean)(java.util.Arrays.stream(keys).anyMatch((x) -> ((Number)(x)).intValue() == v))) {
                    keys = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(keys), java.util.stream.IntStream.of(v)).toArray()));
                }
                int alt = (int)(((int)(dist).getOrDefault(u_2, 0))) + w;
                int cur = dist.containsKey(v) ? ((int)(dist).getOrDefault(v, 0)) : 100000;
                if (!(Boolean)(java.util.Arrays.stream(known).anyMatch((x) -> ((Number)(x)).intValue() == v)) && alt < cur) {
dist.put(v, alt);
path.put(v, u_2);
                }
            }
        }
        int[] ordered = ((int[])(sort_ints(((int[])(keys)))));
        int idx = 0;
        while (idx < ordered.length) {
            int k_1 = ordered[idx];
            if (k_1 != s) {
                System.out.println(((int)(dist).getOrDefault(k_1, 0)));
            }
            idx = idx + 1;
        }
    }

    static void topo(java.util.Map<Integer,int[]> g, int n) {
        int[] ind = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 <= n) {
            ind = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ind), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        int node = 1;
        while (node <= n) {
            for (int v : ((int[])(g).get(node))) {
ind[v] = ind[v] + 1;
            }
            node = node + 1;
        }
        int[] q_1 = ((int[])(new int[]{}));
        int j_1 = 1;
        while (j_1 <= n) {
            if (ind[j_1] == 0) {
                q_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q_1), java.util.stream.IntStream.of(j_1)).toArray()));
            }
            j_1 = j_1 + 1;
        }
        while (q_1.length > 0) {
            int v_1 = q_1[0];
            q_1 = ((int[])(java.util.Arrays.copyOfRange(q_1, 1, q_1.length)));
            System.out.println(v_1);
            for (int w : ((int[])(g).get(v_1))) {
ind[w] = ind[w] - 1;
                if (ind[w] == 0) {
                    q_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q_1), java.util.stream.IntStream.of(w)).toArray()));
                }
            }
        }
    }

    static void floyd(int[][] a) {
        int n = a.length;
        int[][] dist_1 = ((int[][])(new int[][]{}));
        int i_3 = 0;
        while (i_3 < n) {
            int[] row = ((int[])(new int[]{}));
            int j_2 = 0;
            while (j_2 < n) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(a[i_3][j_2])).toArray()));
                j_2 = j_2 + 1;
            }
            dist_1 = ((int[][])(appendObj(dist_1, row)));
            i_3 = i_3 + 1;
        }
        int k_2 = 0;
        while (k_2 < n) {
            int ii = 0;
            while (ii < n) {
                int jj = 0;
                while (jj < n) {
                    if (dist_1[ii][jj] > dist_1[ii][k_2] + dist_1[k_2][jj]) {
dist_1[ii][jj] = dist_1[ii][k_2] + dist_1[k_2][jj];
                    }
                    jj = jj + 1;
                }
                ii = ii + 1;
            }
            k_2 = k_2 + 1;
        }
        System.out.println(java.util.Arrays.deepToString(dist_1));
    }

    static int prim(java.util.Map<Integer,int[][]> g, int s, int n) {
        java.util.Map<Integer,Integer> dist_2 = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
dist_2.put(s, 0);
        int[] known_1 = ((int[])(new int[]{}));
        int[] keys_1 = ((int[])(new int[]{s}));
        int total = 0;
        while (known_1.length < n) {
            int mini_1 = 100000;
            int u_3 = -1;
            int i_4 = 0;
            while (i_4 < keys_1.length) {
                int k_3 = keys_1[i_4];
                int d_1 = (int)(((int)(dist_2).getOrDefault(k_3, 0)));
                if (!(Boolean)(java.util.Arrays.stream(known_1).anyMatch((x) -> ((Number)(x)).intValue() == k_3)) && d_1 < mini_1) {
                    mini_1 = d_1;
                    u_3 = k_3;
                }
                i_4 = i_4 + 1;
            }
            known_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(known_1), java.util.stream.IntStream.of(u_3)).toArray()));
            total = total + mini_1;
            for (int[] e : ((int[][])(g).get(u_3))) {
                int v_2 = e[0];
                int w_1 = e[1];
                if (!(Boolean)(java.util.Arrays.stream(keys_1).anyMatch((x) -> ((Number)(x)).intValue() == v_2))) {
                    keys_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(keys_1), java.util.stream.IntStream.of(v_2)).toArray()));
                }
                int cur_1 = dist_2.containsKey(v_2) ? ((int)(dist_2).getOrDefault(v_2, 0)) : 100000;
                if (!(Boolean)(java.util.Arrays.stream(known_1).anyMatch((x) -> ((Number)(x)).intValue() == v_2)) && w_1 < cur_1) {
dist_2.put(v_2, w_1);
                }
            }
        }
        return total;
    }

    static int[][] sort_edges(int[][] edges) {
        int[][] es = ((int[][])(edges));
        int i_5 = 0;
        while (i_5 < es.length) {
            int j_3 = 0;
            while (j_3 < es.length - i_5 - 1) {
                if (es[j_3][2] > es[j_3 + 1][2]) {
                    int[] tmp_1 = ((int[])(es[j_3]));
es[j_3] = ((int[])(es[j_3 + 1]));
es[j_3 + 1] = ((int[])(tmp_1));
                }
                j_3 = j_3 + 1;
            }
            i_5 = i_5 + 1;
        }
        return es;
    }

    static int find_parent(int[] parent, int x) {
        int r = x;
        while (parent[r] != r) {
            r = parent[r];
        }
        return r;
    }

    static void union_parent(int[] parent, int a, int b) {
parent[a] = b;
    }

    static int kruskal(int[][] edges, int n) {
        int[][] es_1 = ((int[][])(sort_edges(((int[][])(edges)))));
        int[] parent = ((int[])(new int[]{}));
        int i_6 = 0;
        while (i_6 <= n) {
            parent = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parent), java.util.stream.IntStream.of(i_6)).toArray()));
            i_6 = i_6 + 1;
        }
        int total_1 = 0;
        int count = 0;
        int idx_1 = 0;
        while (count < n - 1 && idx_1 < es_1.length) {
            int[] e = ((int[])(es_1[idx_1]));
            idx_1 = idx_1 + 1;
            int u_4 = e[0];
            int v_3 = e[1];
            int w_2 = e[2];
            int ru = find_parent(((int[])(parent)), u_4);
            int rv = find_parent(((int[])(parent)), v_3);
            if (ru != rv) {
                union_parent(((int[])(parent)), ru, rv);
                total_1 = total_1 + w_2;
                count = count + 1;
            }
        }
        return total_1;
    }

    static int[] find_isolated_nodes(java.util.Map<Integer,int[]> g, int[] nodes) {
        int[] isolated = ((int[])(new int[]{}));
        for (int node : nodes) {
            if (((int[])(g).get(node)).length == 0) {
                isolated = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(isolated), java.util.stream.IntStream.of(node)).toArray()));
            }
        }
        return isolated;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            g_dfs = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{4, 5}))), java.util.Map.entry(3, new int[]{}), java.util.Map.entry(4, new int[]{}), java.util.Map.entry(5, new int[]{})))));
            g_bfs = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{4, 5}))), java.util.Map.entry(3, ((int[])(new int[]{6, 7}))), java.util.Map.entry(4, new int[]{}), java.util.Map.entry(5, ((int[])(new int[]{8}))), java.util.Map.entry(6, new int[]{}), java.util.Map.entry(7, new int[]{}), java.util.Map.entry(8, new int[]{})))));
            g_weighted = ((java.util.Map<Integer,int[][]>)(new java.util.LinkedHashMap<Integer, int[][]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[][])(new int[][]{new int[]{2, 7}, new int[]{3, 9}, new int[]{6, 14}}))), java.util.Map.entry(2, ((int[][])(new int[][]{new int[]{1, 7}, new int[]{3, 10}, new int[]{4, 15}}))), java.util.Map.entry(3, ((int[][])(new int[][]{new int[]{1, 9}, new int[]{2, 10}, new int[]{4, 11}, new int[]{6, 2}}))), java.util.Map.entry(4, ((int[][])(new int[][]{new int[]{2, 15}, new int[]{3, 11}, new int[]{5, 6}}))), java.util.Map.entry(5, ((int[][])(new int[][]{new int[]{4, 6}, new int[]{6, 9}}))), java.util.Map.entry(6, ((int[][])(new int[][]{new int[]{1, 14}, new int[]{3, 2}, new int[]{5, 9}})))))));
            g_topo = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{4}))), java.util.Map.entry(3, ((int[])(new int[]{4}))), java.util.Map.entry(4, new int[]{})))));
            matrix = ((int[][])(new int[][]{new int[]{0, 5, 9, 100000}, new int[]{100000, 0, 2, 8}, new int[]{100000, 100000, 0, 7}, new int[]{4, 100000, 100000, 0}}));
            g_prim = ((java.util.Map<Integer,int[][]>)(new java.util.LinkedHashMap<Integer, int[][]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[][])(new int[][]{new int[]{2, 1}, new int[]{3, 3}}))), java.util.Map.entry(2, ((int[][])(new int[][]{new int[]{1, 1}, new int[]{3, 1}, new int[]{4, 6}}))), java.util.Map.entry(3, ((int[][])(new int[][]{new int[]{1, 3}, new int[]{2, 1}, new int[]{4, 2}}))), java.util.Map.entry(4, ((int[][])(new int[][]{new int[]{2, 6}, new int[]{3, 2}})))))));
            edges_kruskal = ((int[][])(new int[][]{new int[]{1, 2, 1}, new int[]{2, 3, 2}, new int[]{1, 3, 2}, new int[]{3, 4, 1}}));
            g_iso = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3}))), java.util.Map.entry(3, ((int[])(new int[]{1, 2}))), java.util.Map.entry(4, new int[]{})))));
            dfs(g_dfs, 1);
            bfs(g_bfs, 1);
            dijkstra(g_weighted, 1);
            topo(g_topo, 4);
            floyd(((int[][])(matrix)));
            System.out.println(prim(g_prim, 1, 4));
            System.out.println(kruskal(((int[][])(edges_kruskal)), 4));
            iso = ((int[])(find_isolated_nodes(g_iso, ((int[])(new int[]{1, 2, 3, 4})))));
            System.out.println(java.util.Arrays.toString(iso));
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
}
