public class Main {
    static java.util.Map<String,String[]> G;
    static String[] result;

    static boolean contains(String[] lst, String v) {
        long i = 0L;
        while ((long)(i) < (long)(lst.length)) {
            if ((lst[(int)((long)(i))].equals(v))) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static String[] depth_first_search(java.util.Map<String,String[]> graph, String start) {
        String[] explored = ((String[])(new String[]{}));
        String[] stack_1 = ((String[])(new String[]{}));
        stack_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(start)).toArray(String[]::new)));
        explored = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(explored), java.util.stream.Stream.of(start)).toArray(String[]::new)));
        while ((long)(stack_1.length) > 0L) {
            long idx_1 = (long)((long)(stack_1.length) - 1L);
            String v_1 = stack_1[(int)((long)(idx_1))];
            stack_1 = ((String[])(java.util.Arrays.copyOfRange(stack_1, (int)((long)(0)), (int)((long)(idx_1)))));
            String[] neighbors_1 = (String[])(((String[])(graph).get(v_1)));
            long i_2 = (long)((long)(neighbors_1.length) - 1L);
            while ((long)(i_2) >= 0L) {
                String adj_1 = neighbors_1[(int)((long)(i_2))];
                if (!(Boolean)contains(((String[])(explored)), adj_1)) {
                    explored = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(explored), java.util.stream.Stream.of(adj_1)).toArray(String[]::new)));
                    stack_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack_1), java.util.stream.Stream.of(adj_1)).toArray(String[]::new)));
                }
                i_2 = (long)((long)(i_2) - 1L);
            }
        }
        return explored;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            G = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("A", ((String[])(new String[]{"B", "C", "D"}))), java.util.Map.entry("B", ((String[])(new String[]{"A", "D", "E"}))), java.util.Map.entry("C", ((String[])(new String[]{"A", "F"}))), java.util.Map.entry("D", ((String[])(new String[]{"B", "D"}))), java.util.Map.entry("E", ((String[])(new String[]{"B", "F"}))), java.util.Map.entry("F", ((String[])(new String[]{"C", "E", "G"}))), java.util.Map.entry("G", ((String[])(new String[]{"F"})))))));
            result = ((String[])(depth_first_search(G, "A")));
            System.out.println(java.util.Arrays.toString(result));
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
