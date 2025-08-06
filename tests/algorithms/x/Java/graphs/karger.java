public class Main {
    static int seed = 0;
    static class Pair {
        String a;
        String b;
        Pair(String a, String b) {
            this.a = a;
            this.b = b;
        }
        Pair() {}
        @Override public String toString() {
            return String.format("{'a': '%s', 'b': '%s'}", String.valueOf(a), String.valueOf(b));
        }
    }

    static java.util.Map<String,String[]> TEST_GRAPH;
    static Pair[] result;

    static int rand_int(int n) {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return Math.floorMod(seed, n);
    }

    static boolean contains(String[] list, String value) {
        int i = 0;
        while (i < list.length) {
            if ((list[i].equals(value))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static String[] remove_all(String[] list, String value) {
        String[] res = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < list.length) {
            if (!(list[i_1].equals(value))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(list[i_1])).toArray(String[]::new)));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static Pair[] partition_graph(java.util.Map<String,String[]> graph) {
        java.util.Map<String,String[]> contracted = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (var node : new java.util.ArrayList<>(graph.keySet())) {
contracted.put(node, ((String[])(new String[]{node})));
        }
        java.util.Map<String,String[]> graph_copy = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (var node : new java.util.ArrayList<>(graph.keySet())) {
            String[] lst = ((String[])(new String[]{}));
            String[] neigh = (String[])(((String[])(graph).get(node)));
            int i_2 = 0;
            while (i_2 < neigh.length) {
                lst = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lst), java.util.stream.Stream.of(neigh[i_2])).toArray(String[]::new)));
                i_2 = i_2 + 1;
            }
graph_copy.put(node, ((String[])(lst)));
        }
        Object nodes = new java.util.ArrayList<>(graph_copy.keySet());
        while (String.valueOf(nodes).length() > 2) {
            Object u = nodes[rand_int(String.valueOf(nodes).length())];
            String[] u_neighbors = (String[])(((String[])(graph_copy).get(u)));
            String v = u_neighbors[rand_int(u_neighbors.length)];
            String uv = (String)(u) + v;
            String[] uv_neighbors = ((String[])(new String[]{}));
            int i_3 = 0;
            while (i_3 < ((String[])(graph_copy).get(u)).length) {
                String n = ((String[])(graph_copy).get(u))[i_3];
                if (n != ((Number)(u)).intValue() && !(n.equals(v)) && contains(((String[])(uv_neighbors)), n) == false) {
                    uv_neighbors = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(uv_neighbors), java.util.stream.Stream.of(n)).toArray(String[]::new)));
                }
                i_3 = i_3 + 1;
            }
            i_3 = 0;
            while (i_3 < ((String[])(graph_copy).get(v)).length) {
                String n_1 = ((String[])(graph_copy).get(v))[i_3];
                if (n_1 != ((Number)(u)).intValue() && !(n_1.equals(v)) && contains(((String[])(uv_neighbors)), n_1) == false) {
                    uv_neighbors = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(uv_neighbors), java.util.stream.Stream.of(n_1)).toArray(String[]::new)));
                }
                i_3 = i_3 + 1;
            }
graph_copy.put(uv, ((String[])(uv_neighbors)));
            int k = 0;
            while (k < uv_neighbors.length) {
                String nb = uv_neighbors[k];
graph_copy.put(nb, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph_copy).get(nb))), java.util.stream.Stream.of(uv)).toArray(String[]::new))));
graph_copy.put(nb, ((String[])(remove_all((String[])(((String[])(graph_copy).get(nb))), (String)(u)))));
graph_copy.put(nb, ((String[])(remove_all((String[])(((String[])(graph_copy).get(nb))), v))));
                k = k + 1;
            }
            String[] group = ((String[])(new String[]{}));
            i_3 = 0;
            while (i_3 < ((String[])(contracted).get(u)).length) {
                group = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(group), java.util.stream.Stream.of(((String[])(contracted).get(u))[i_3])).toArray(String[]::new)));
                i_3 = i_3 + 1;
            }
            i_3 = 0;
            while (i_3 < ((String[])(contracted).get(v)).length) {
                String val = ((String[])(contracted).get(v))[i_3];
                if (contains(((String[])(group)), val) == false) {
                    group = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(group), java.util.stream.Stream.of(val)).toArray(String[]::new)));
                }
                i_3 = i_3 + 1;
            }
contracted.put(uv, ((String[])(group)));
            nodes = remove_all(((String[])(nodes)), (String)(u));
            nodes = remove_all(((String[])(nodes)), v);
            nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(uv)).toArray(String[]::new);
        }
        String[][] groups = ((String[][])(new String[][]{}));
        int j = 0;
        while (j < String.valueOf(nodes).length()) {
            Object n_2 = nodes[j];
            groups = ((String[][])(appendObj(groups, ((String[])(contracted).get(n_2)))));
            j = j + 1;
        }
        String[] groupA = ((String[])(groups[0]));
        String[] groupB = ((String[])(groups[1]));
        Pair[] cut = ((Pair[])(new Pair[]{}));
        j = 0;
        while (j < groupA.length) {
            String node = groupA[j];
            String[] neigh_1 = (String[])(((String[])(graph).get(node)));
            int l = 0;
            while (l < neigh_1.length) {
                String nb_1 = neigh_1[l];
                if (((Boolean)(contains(((String[])(groupB)), nb_1)))) {
                    cut = ((Pair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cut), java.util.stream.Stream.of(new Pair(node, nb_1))).toArray(Pair[]::new)));
                }
                l = l + 1;
            }
            j = j + 1;
        }
        return cut;
    }

    static String cut_to_string(Pair[] cut) {
        String s = "{";
        int i_4 = 0;
        while (i_4 < cut.length) {
            Pair p = cut[i_4];
            s = s + "(" + p.a + ", " + p.b + ")";
            if (i_4 < cut.length - 1) {
                s = s + ", ";
            }
            i_4 = i_4 + 1;
        }
        s = s + "}";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
            TEST_GRAPH = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("1", ((String[])(new String[]{"2", "3", "4", "5"}))), java.util.Map.entry("2", ((String[])(new String[]{"1", "3", "4", "5"}))), java.util.Map.entry("3", ((String[])(new String[]{"1", "2", "4", "5", "10"}))), java.util.Map.entry("4", ((String[])(new String[]{"1", "2", "3", "5", "6"}))), java.util.Map.entry("5", ((String[])(new String[]{"1", "2", "3", "4", "7"}))), java.util.Map.entry("6", ((String[])(new String[]{"7", "8", "9", "10", "4"}))), java.util.Map.entry("7", ((String[])(new String[]{"6", "8", "9", "10", "5"}))), java.util.Map.entry("8", ((String[])(new String[]{"6", "7", "9", "10"}))), java.util.Map.entry("9", ((String[])(new String[]{"6", "7", "8", "10"}))), java.util.Map.entry("10", ((String[])(new String[]{"6", "7", "8", "9", "3"})))))));
            result = ((Pair[])(partition_graph(TEST_GRAPH)));
            System.out.println(cut_to_string(((Pair[])(result))));
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
