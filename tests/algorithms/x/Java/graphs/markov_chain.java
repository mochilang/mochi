public class Main {
    static class Transition {
        String src;
        String dst;
        double prob;
        Transition(String src, String dst, double prob) {
            this.src = src;
            this.dst = dst;
            this.prob = prob;
        }
        Transition() {}
        @Override public String toString() {
            return String.format("{'src': '%s', 'dst': '%s', 'prob': %s}", String.valueOf(src), String.valueOf(dst), String.valueOf(prob));
        }
    }

    static long seed = 1L;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static double random() {
        return (double)(((double)(1.0) * (double)(rand()))) / (double)(2147483648.0);
    }

    static String[] get_nodes(Transition[] trans) {
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (Transition t : trans) {
seen.put(t.src, true);
seen.put(t.dst, true);
        }
        String[] nodes_1 = ((String[])(new String[]{}));
        for (var k : new java.util.ArrayList<>(seen.keySet())) {
            nodes_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_1), java.util.stream.Stream.of(k)).toArray(String[]::new)));
        }
        return nodes_1;
    }

    static String transition(String current, Transition[] trans) {
        double current_probability = (double)(0.0);
        double random_value_1 = (double)(random());
        for (Transition t : trans) {
            if ((t.src.equals(current))) {
                current_probability = (double)((double)(current_probability) + (double)(t.prob));
                if ((double)(current_probability) > (double)(random_value_1)) {
                    return t.dst;
                }
            }
        }
        return "";
    }

    static java.util.Map<String,Long> get_transitions(String start, Transition[] trans, long steps) {
        java.util.Map<String,Long> visited = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        for (String node : get_nodes(((Transition[])(trans)))) {
            long one_1 = 1L;
visited.put(node, (long)(one_1));
        }
        String node_2 = start;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(steps)) {
            node_2 = String.valueOf(transition(node_2, ((Transition[])(trans))));
            long count_1 = (long)(((long)(visited).getOrDefault(node_2, 0L)));
            count_1 = (long)((long)(count_1) + 1L);
visited.put(node_2, (long)(count_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return visited;
    }

    static void main() {
        Transition[] transitions = ((Transition[])(new Transition[]{new Transition("a", "a", 0.9), new Transition("a", "b", 0.075), new Transition("a", "c", 0.025), new Transition("b", "a", 0.15), new Transition("b", "b", 0.8), new Transition("b", "c", 0.05), new Transition("c", "a", 0.25), new Transition("c", "b", 0.25), new Transition("c", "c", 0.5)}));
        java.util.Map<String,Long> result_1 = get_transitions("a", ((Transition[])(transitions)), 5000L);
        System.out.println(_p(((long)(result_1).getOrDefault("a", 0L))) + " " + _p(((long)(result_1).getOrDefault("b", 0L))) + " " + _p(((long)(result_1).getOrDefault("c", 0L))));
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
