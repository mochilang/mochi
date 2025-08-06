public class Main {
    static int INF;

    static java.util.Map<Integer,int[][]> connect(java.util.Map<Integer,int[][]> graph, int a, int b, int w) {
        int u = a - 1;
        int v = b - 1;
        java.util.Map<Integer,int[][]> g = graph;
g.put(u, ((int[][])(appendObj(((int[][])(g).get(u)), new int[]{v, w}))));
g.put(v, ((int[][])(appendObj(((int[][])(g).get(v)), new int[]{u, w}))));
        return g;
    }

    static boolean in_list(int[] arr, int x) {
        int i = 0;
        while (i < arr.length) {
            if (arr[i] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int[][] prim(java.util.Map<Integer,int[][]> graph, int s, int n) {
        java.util.Map<Integer,Integer> dist = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        java.util.Map<Integer,Integer> parent = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
dist.put(s, 0);
parent.put(s, -1);
        int[] known = ((int[])(new int[]{}));
        int[] keys = ((int[])(new int[]{s}));
        while (known.length < n) {
            int mini = INF;
            int u_1 = -1;
            int i_1 = 0;
            while (i_1 < keys.length) {
                int k = keys[i_1];
                int d = (int)(((int)(dist).getOrDefault(k, 0)));
                if (!(Boolean)(in_list(((int[])(known)), k)) && d < mini) {
                    mini = d;
                    u_1 = k;
                }
                i_1 = i_1 + 1;
            }
            known = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(known), java.util.stream.IntStream.of(u_1)).toArray()));
            for (int[] e : ((int[][])(graph).get(u_1))) {
                int v_1 = e[0];
                int w = e[1];
                if (!(Boolean)(in_list(((int[])(keys)), v_1))) {
                    keys = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(keys), java.util.stream.IntStream.of(v_1)).toArray()));
                }
                int cur = dist.containsKey(v_1) ? ((int)(dist).getOrDefault(v_1, 0)) : INF;
                if (!(Boolean)(in_list(((int[])(known)), v_1)) && w < cur) {
dist.put(v_1, w);
parent.put(v_1, u_1);
                }
            }
        }
        int[][] edges = ((int[][])(new int[][]{}));
        int j = 0;
        while (j < keys.length) {
            int v_2 = keys[j];
            if (v_2 != s) {
                edges = ((int[][])(appendObj(edges, new int[]{v_2 + 1, (int)(((int)(parent).getOrDefault(v_2, 0))) + 1})));
            }
            j = j + 1;
        }
        return edges;
    }

    static int[] sort_heap(int[] h, java.util.Map<Integer,Integer> dist) {
        int[] a = ((int[])(h));
        int i_2 = 0;
        while (i_2 < a.length) {
            int j_1 = 0;
            while (j_1 < a.length - i_2 - 1) {
                int dj = dist.containsKey(a[j_1]) ? ((int)(dist).getOrDefault(a[j_1], 0)) : INF;
                int dj1 = dist.containsKey(a[j_1 + 1]) ? ((int)(dist).getOrDefault(a[j_1 + 1], 0)) : INF;
                if (dj > dj1) {
                    int t = a[j_1];
a[j_1] = a[j_1 + 1];
a[j_1 + 1] = t;
                }
                j_1 = j_1 + 1;
            }
            i_2 = i_2 + 1;
        }
        return a;
    }

    static int[][] prim_heap(java.util.Map<Integer,int[][]> graph, int s, int n) {
        java.util.Map<Integer,Integer> dist_1 = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        java.util.Map<Integer,Integer> parent_1 = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
dist_1.put(s, 0);
parent_1.put(s, -1);
        int[] h = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < n) {
            h = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(h), java.util.stream.IntStream.of(i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        h = ((int[])(sort_heap(((int[])(h)), dist_1)));
        int[] known_1 = ((int[])(new int[]{}));
        while (h.length > 0) {
            int u_2 = h[0];
            h = ((int[])(java.util.Arrays.copyOfRange(h, 1, h.length)));
            known_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(known_1), java.util.stream.IntStream.of(u_2)).toArray()));
            for (int[] e : ((int[][])(graph).get(u_2))) {
                int v_3 = e[0];
                int w_1 = e[1];
                int cur_1 = dist_1.containsKey(v_3) ? ((int)(dist_1).getOrDefault(v_3, 0)) : INF;
                if (!(Boolean)(in_list(((int[])(known_1)), v_3)) && w_1 < cur_1) {
dist_1.put(v_3, w_1);
parent_1.put(v_3, u_2);
                }
            }
            h = ((int[])(sort_heap(((int[])(h)), dist_1)));
        }
        int[][] edges_1 = ((int[][])(new int[][]{}));
        int j_2 = 0;
        while (j_2 < n) {
            if (j_2 != s) {
                edges_1 = ((int[][])(appendObj(edges_1, new int[]{j_2 + 1, (int)(((int)(parent_1).getOrDefault(j_2, 0))) + 1})));
            }
            j_2 = j_2 + 1;
        }
        return edges_1;
    }

    static void print_edges(int[][] edges) {
        int i_4 = 0;
        while (i_4 < edges.length) {
            int[] e = ((int[])(edges[i_4]));
            System.out.println("(" + _p(_geti(e, 0)) + ", " + _p(_geti(e, 1)) + ")");
            i_4 = i_4 + 1;
        }
    }

    static void test_vector() {
        int x = 5;
        java.util.Map<Integer,int[][]> G = ((java.util.Map<Integer,int[][]>)(new java.util.LinkedHashMap<Integer, int[][]>()));
        int i_5 = 0;
        while (i_5 < x) {
G.put(i_5, ((int[][])(new int[][]{})));
            i_5 = i_5 + 1;
        }
        G = connect(G, 1, 2, 15);
        G = connect(G, 1, 3, 12);
        G = connect(G, 2, 4, 13);
        G = connect(G, 2, 5, 5);
        G = connect(G, 3, 2, 6);
        G = connect(G, 3, 4, 6);
        int[][] mst = ((int[][])(prim(G, 0, x)));
        print_edges(((int[][])(mst)));
        int[][] mst_heap = ((int[][])(prim_heap(G, 0, x)));
        print_edges(((int[][])(mst_heap)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000;
            test_vector();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
