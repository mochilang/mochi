public class Main {
    static java.util.Map<String,String[]> G;

    static String join(String[] xs) {
        String s = "";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            s = s + xs[(int)((long)(i_1))];
            i_1 = (long)((long)(i_1) + 1L);
        }
        return s;
    }

    static String[] breadth_first_search(java.util.Map<String,String[]> graph, String start) {
        java.util.Map<String,Boolean> explored = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
explored.put(start, true);
        String[] result_1 = ((String[])(new String[]{start}));
        String[] queue_1 = ((String[])(new String[]{start}));
        while ((long)(queue_1.length) > 0L) {
            String v_1 = queue_1[(int)((long)(0))];
            queue_1 = ((String[])(java.util.Arrays.copyOfRange(queue_1, (int)((long)(1)), (int)((long)(queue_1.length)))));
            String[] children_1 = (String[])(((String[])(graph).get(v_1)));
            long i_3 = 0L;
            while ((long)(i_3) < (long)(children_1.length)) {
                String w_1 = children_1[(int)((long)(i_3))];
                if (!(explored.containsKey(w_1))) {
explored.put(w_1, true);
                    result_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(w_1)).toArray(String[]::new)));
                    queue_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(w_1)).toArray(String[]::new)));
                }
                i_3 = (long)((long)(i_3) + 1L);
            }
        }
        return result_1;
    }

    static String[] breadth_first_search_with_deque(java.util.Map<String,String[]> graph, String start) {
        java.util.Map<String,Boolean> visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
visited.put(start, true);
        String[] result_3 = ((String[])(new String[]{start}));
        String[] queue_3 = ((String[])(new String[]{start}));
        long head_1 = 0L;
        while ((long)(head_1) < (long)(queue_3.length)) {
            String v_3 = queue_3[(int)((long)(head_1))];
            head_1 = (long)((long)(head_1) + 1L);
            String[] children_3 = (String[])(((String[])(graph).get(v_3)));
            long i_5 = 0L;
            while ((long)(i_5) < (long)(children_3.length)) {
                String child_1 = children_3[(int)((long)(i_5))];
                if (!(visited.containsKey(child_1))) {
visited.put(child_1, true);
                    result_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_3), java.util.stream.Stream.of(child_1)).toArray(String[]::new)));
                    queue_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_3), java.util.stream.Stream.of(child_1)).toArray(String[]::new)));
                }
                i_5 = (long)((long)(i_5) + 1L);
            }
        }
        return result_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            G = ((java.util.Map<String,String[]>)(new java.util.LinkedHashMap<String, String[]>(java.util.Map.ofEntries(java.util.Map.entry("A", ((String[])(new String[]{"B", "C"}))), java.util.Map.entry("B", ((String[])(new String[]{"A", "D", "E"}))), java.util.Map.entry("C", ((String[])(new String[]{"A", "F"}))), java.util.Map.entry("D", ((String[])(new String[]{"B"}))), java.util.Map.entry("E", ((String[])(new String[]{"B", "F"}))), java.util.Map.entry("F", ((String[])(new String[]{"C", "E"})))))));
            System.out.println(join(((String[])(breadth_first_search(G, "A")))));
            System.out.println(join(((String[])(breadth_first_search_with_deque(G, "A")))));
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
