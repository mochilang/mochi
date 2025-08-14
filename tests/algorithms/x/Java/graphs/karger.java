public class Main {
    static long seed = 1L;
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

    static long rand_int(long n) {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return Math.floorMod(seed, n);
    }

    static boolean contains(String[] list, String value) {
        long i = 0L;
        while ((long)(i) < (long)(list.length)) {
            if ((list[(int)((long)(i))].equals(value))) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static String[] remove_all(String[] list, String value) {
        String[] res = ((String[])(new String[]{}));
        long i_2 = 0L;
        while ((long)(i_2) < (long)(list.length)) {
            if (!(list[(int)((long)(i_2))].equals(value))) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(list[(int)((long)(i_2))])).toArray(String[]::new)));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        return res;
    }

    static Pair[] partition_graph(java.util.Map<String,String[]> graph) {
        java.util.Map<String,String[]> contracted = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (var node : new java.util.ArrayList<>(graph.keySet())) {
contracted.put(node, ((String[])(new String[]{node})));
        }
        java.util.Map<String,String[]> graph_copy_1 = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>()));
        for (var node : new java.util.ArrayList<>(graph.keySet())) {
            String[] lst_1 = ((String[])(new String[]{}));
            String[] neigh_1 = (String[])(((String[])(graph).get(node)));
            long i_4 = 0L;
            while ((long)(i_4) < (long)(neigh_1.length)) {
                lst_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(lst_1), java.util.stream.Stream.of(neigh_1[(int)((long)(i_4))])).toArray(String[]::new)));
                i_4 = (long)((long)(i_4) + 1L);
            }
graph_copy_1.put(node, ((String[])(lst_1)));
        }
        java.util.List nodes_1 = new java.util.ArrayList<>(graph_copy_1.keySet());
        while ((long)(String.valueOf(nodes_1).length()) > 2L) {
            Object u_1 = nodes_1[rand_int((long)(String.valueOf(nodes_1).length()))];
            String[] u_neighbors_1 = (String[])(((String[])(graph_copy_1).get(u_1)));
            String v_1 = u_neighbors_1[(int)((long)(rand_int((long)(u_neighbors_1.length))))];
            String uv_1 = (String)(u_1) + v_1;
            String[] uv_neighbors_1 = ((String[])(new String[]{}));
            long i_6 = 0L;
            while ((long)(i_6) < (long)(((String[])(graph_copy_1).get(u_1)).length)) {
                String n_2 = ((String[])(graph_copy_1).get(u_1))[(int)((long)(i_6))];
                if ((long)(n_2) != ((Number)(u_1)).intValue() && !(n_2.equals(v_1)) && (contains(((String[])(uv_neighbors_1)), n_2) == false)) {
                    uv_neighbors_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(uv_neighbors_1), java.util.stream.Stream.of(n_2)).toArray(String[]::new)));
                }
                i_6 = (long)((long)(i_6) + 1L);
            }
            i_6 = 0L;
            while ((long)(i_6) < (long)(((String[])(graph_copy_1).get(v_1)).length)) {
                String n_3 = ((String[])(graph_copy_1).get(v_1))[(int)((long)(i_6))];
                if ((long)(n_3) != ((Number)(u_1)).intValue() && !(n_3.equals(v_1)) && (contains(((String[])(uv_neighbors_1)), n_3) == false)) {
                    uv_neighbors_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(uv_neighbors_1), java.util.stream.Stream.of(n_3)).toArray(String[]::new)));
                }
                i_6 = (long)((long)(i_6) + 1L);
            }
graph_copy_1.put(uv_1, ((String[])(uv_neighbors_1)));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(uv_neighbors_1.length)) {
                String nb_1 = uv_neighbors_1[(int)((long)(k_1))];
graph_copy_1.put(nb_1, ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((String[])(graph_copy_1).get(nb_1))), java.util.stream.Stream.of(uv_1)).toArray(String[]::new))));
graph_copy_1.put(nb_1, ((String[])(remove_all((String[])(((String[])(graph_copy_1).get(nb_1))), (String)(u_1)))));
graph_copy_1.put(nb_1, ((String[])(remove_all((String[])(((String[])(graph_copy_1).get(nb_1))), v_1))));
                k_1 = (long)((long)(k_1) + 1L);
            }
            String[] group_1 = ((String[])(new String[]{}));
            i_6 = 0L;
            while ((long)(i_6) < (long)(((String[])(contracted).get(u_1)).length)) {
                group_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(group_1), java.util.stream.Stream.of(((String[])(contracted).get(u_1))[(int)((long)(i_6))])).toArray(String[]::new)));
                i_6 = (long)((long)(i_6) + 1L);
            }
            i_6 = 0L;
            while ((long)(i_6) < (long)(((String[])(contracted).get(v_1)).length)) {
                String val_1 = ((String[])(contracted).get(v_1))[(int)((long)(i_6))];
                if ((contains(((String[])(group_1)), val_1) == false)) {
                    group_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(group_1), java.util.stream.Stream.of(val_1)).toArray(String[]::new)));
                }
                i_6 = (long)((long)(i_6) + 1L);
            }
contracted.put(uv_1, ((String[])(group_1)));
            nodes_1 = remove_all(((String[])(nodes_1)), (String)(u_1));
            nodes_1 = remove_all(((String[])(nodes_1)), v_1);
            nodes_1 = java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_1), java.util.stream.Stream.of(uv_1)).toArray(String[]::new);
        }
        String[][] groups_1 = ((String[][])(new String[][]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(String.valueOf(nodes_1).length())) {
            Object n_5 = nodes_1[j_1];
            groups_1 = ((String[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(groups_1), java.util.stream.Stream.of(new String[][]{((String[])(contracted).get(n_5))})).toArray(String[][]::new)));
            j_1 = (long)((long)(j_1) + 1L);
        }
        String[] groupA_1 = ((String[])(groups_1[(int)((long)(0))]));
        String[] groupB_1 = ((String[])(groups_1[(int)((long)(1))]));
        Pair[] cut_1 = ((Pair[])(new Pair[]{}));
        j_1 = 0L;
        while ((long)(j_1) < (long)(groupA_1.length)) {
            String node_1 = groupA_1[(int)((long)(j_1))];
            String[] neigh_3 = (String[])(((String[])(graph).get(node_1)));
            long l_1 = 0L;
            while ((long)(l_1) < (long)(neigh_3.length)) {
                String nb_3 = neigh_3[(int)((long)(l_1))];
                if (contains(((String[])(groupB_1)), nb_3)) {
                    cut_1 = ((Pair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(cut_1), java.util.stream.Stream.of(new Pair(node_1, nb_3))).toArray(Pair[]::new)));
                }
                l_1 = (long)((long)(l_1) + 1L);
            }
            j_1 = (long)((long)(j_1) + 1L);
        }
        return cut_1;
    }

    static String cut_to_string(Pair[] cut) {
        String s = "{";
        long i_8 = 0L;
        while ((long)(i_8) < (long)(cut.length)) {
            Pair p_1 = cut[(int)((long)(i_8))];
            s = s + "(" + p_1.a + ", " + p_1.b + ")";
            if ((long)(i_8) < (long)((long)(cut.length) - 1L)) {
                s = s + ", ";
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        s = s + "}";
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
}
