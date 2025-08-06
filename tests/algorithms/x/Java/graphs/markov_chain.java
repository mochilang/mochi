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

    static int seed = 0;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static String[] get_nodes(Transition[] trans) {
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (Transition t : trans) {
seen.put(t.src, true);
seen.put(t.dst, true);
        }
        String[] nodes = ((String[])(new String[]{}));
        for (var k : new java.util.ArrayList<>(seen.keySet())) {
            nodes = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(k)).toArray(String[]::new)));
        }
        return nodes;
    }

    static String transition(String current, Transition[] trans) {
        double current_probability = 0.0;
        double random_value = random();
        for (Transition t : trans) {
            if ((t.src.equals(current))) {
                current_probability = current_probability + t.prob;
                if (current_probability > random_value) {
                    return t.dst;
                }
            }
        }
        return "";
    }

    static java.util.Map<String,Integer> get_transitions(String start, Transition[] trans, int steps) {
        java.util.Map<String,Integer> visited = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        for (String node : get_nodes(((Transition[])(trans)))) {
            int one = 1;
visited.put(node, one);
        }
        String node = start;
        int i = 0;
        while (i < steps) {
            node = String.valueOf(transition(node, ((Transition[])(trans))));
            int count = (int)(((int)(visited).getOrDefault(node, 0)));
            count = count + 1;
visited.put(node, count);
            i = i + 1;
        }
        return visited;
    }

    static void main() {
        Transition[] transitions = ((Transition[])(new Transition[]{new Transition("a", "a", 0.9), new Transition("a", "b", 0.075), new Transition("a", "c", 0.025), new Transition("b", "a", 0.15), new Transition("b", "b", 0.8), new Transition("b", "c", 0.05), new Transition("c", "a", 0.25), new Transition("c", "b", 0.25), new Transition("c", "c", 0.5)}));
        java.util.Map<String,Integer> result = get_transitions("a", ((Transition[])(transitions)), 5000);
        System.out.println(_p(((int)(result).getOrDefault("a", 0))) + " " + _p(((int)(result).getOrDefault("b", 0))) + " " + _p(((int)(result).getOrDefault("c", 0))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
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
