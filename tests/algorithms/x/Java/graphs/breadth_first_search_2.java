public class Main {
    static java.util.Map<String,String[]> G;

    static String join(String[] xs) {
        String s = "";
        int i = 0;
        while (i < xs.length) {
            s = s + xs[i];
            i = i + 1;
        }
        return s;
    }

    static String[] breadth_first_search(java.util.Map<String,String[]> graph, String start) {
        java.util.Map<String,Boolean> explored = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
explored.put(start, true);
        String[] result = ((String[])(new String[]{start}));
        String[] queue = ((String[])(new String[]{start}));
        while (queue.length > 0) {
            String v = queue[0];
            queue = ((String[])(java.util.Arrays.copyOfRange(queue, 1, queue.length)));
            String[] children = (String[])(((String[])(graph).get(v)));
            int i_1 = 0;
            while (i_1 < children.length) {
                String w = children[i_1];
                if (!(Boolean)(explored.containsKey(w))) {
explored.put(w, true);
                    result = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(w)).toArray(String[]::new)));
                    queue = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(w)).toArray(String[]::new)));
                }
                i_1 = i_1 + 1;
            }
        }
        return result;
    }

    static String[] breadth_first_search_with_deque(java.util.Map<String,String[]> graph, String start) {
        java.util.Map<String,Boolean> visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
visited.put(start, true);
        String[] result_1 = ((String[])(new String[]{start}));
        String[] queue_1 = ((String[])(new String[]{start}));
        int head = 0;
        while (head < queue_1.length) {
            String v_1 = queue_1[head];
            head = head + 1;
            String[] children_1 = (String[])(((String[])(graph).get(v_1)));
            int i_2 = 0;
            while (i_2 < children_1.length) {
                String child = children_1[i_2];
                if (!(Boolean)(visited.containsKey(child))) {
visited.put(child, true);
                    result_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(child)).toArray(String[]::new)));
                    queue_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(child)).toArray(String[]::new)));
                }
                i_2 = i_2 + 1;
            }
        }
        return result_1;
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
