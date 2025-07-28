public class Main {
    static int INF = 1000000000;
    static java.util.Map<String,java.util.Map<String,Integer>> graph = ((java.util.Map<String,java.util.Map<String,Integer>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Integer>>()));

    static void addEdge(String u, String v, int w) {
        if (!(Boolean)(graph.containsKey(u))) {
graph.put(u, new java.util.LinkedHashMap<String, Integer>());
        }
((java.util.Map<String,Integer>)(graph).get(u)).put(v, w);
        if (!(Boolean)(graph.containsKey(v))) {
graph.put(v, new java.util.LinkedHashMap<String, Integer>());
        }
    }

    static String[] removeAt(String[] xs, int idx) {
        String[] out = new String[]{};
        int i = 0;
        for (String x : xs) {
            if (i != idx) {
                out = java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.stream.Stream.of(x)).toArray(String[]::new);
            }
            i = i + 1;
        }
        return out;
    }

    static java.util.Map<String,Object> dijkstra(String source) {
        java.util.Map<String,Integer> dist = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        java.util.Map<String,String> prev = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        for (String v : graph.keySet()) {
dist.put(v, INF);
prev.put(v, "");
        }
dist.put(source, 0);
        String[] q = new String[]{};
        for (String v : graph.keySet()) {
            q = java.util.stream.Stream.concat(java.util.Arrays.stream(q), java.util.stream.Stream.of(v)).toArray(String[]::new);
        }
        while (q.length > 0) {
            int bestIdx = 0;
            String u = q[0];
            int i = 1;
            while (i < q.length) {
                String v = q[i];
                if ((int)(((int)(dist).getOrDefault(v, 0))) < (int)(((int)(dist).getOrDefault(u, 0)))) {
                    u = v;
                    bestIdx = i;
                }
                i = i + 1;
            }
            q = removeAt(q, bestIdx);
            for (String v : ((java.util.Map<String,Integer>)(graph).get(u)).keySet()) {
                int alt = (int)(((int)(dist).getOrDefault(u, 0))) + (int)(((int)(((java.util.Map<String,Integer>)(graph).get(u))).getOrDefault(v, 0)));
                if (alt < (int)(((int)(dist).getOrDefault(v, 0)))) {
dist.put(v, alt);
prev.put(v, u);
                }
            }
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("dist", dist), java.util.Map.entry("prev", prev)));
    }

    static String path(java.util.Map<String,String> prev, String v) {
        String s = v;
        String cur = v;
        while (!(((String)(prev).get(cur)).equals(""))) {
            cur = ((String)(prev).get(cur));
            s = cur + s;
        }
        return s;
    }

    static void main() {
        addEdge("a", "b", 7);
        addEdge("a", "c", 9);
        addEdge("a", "f", 14);
        addEdge("b", "c", 10);
        addEdge("b", "d", 15);
        addEdge("c", "d", 11);
        addEdge("c", "f", 2);
        addEdge("d", "e", 6);
        addEdge("e", "f", 9);
        java.util.Map<String,Object> res = dijkstra("a");
        java.util.Map<String,Integer> dist = ((java.util.Map<String,Integer>)(((java.util.Map<String,Integer>) (res.get("dist")))));
        java.util.Map<String,String> prev = ((java.util.Map<String,String>)(((java.util.Map<String,String>) (res.get("prev")))));
        System.out.println("Distance to e: " + String.valueOf(((int)(dist).getOrDefault("e", 0))) + ", Path: " + String.valueOf(path(prev, "e")));
        System.out.println("Distance to f: " + String.valueOf(((int)(dist).getOrDefault("f", 0))) + ", Path: " + String.valueOf(path(prev, "f")));
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
}
