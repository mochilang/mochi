public class Main {
    static java.util.Map<Integer,java.util.Map<String,Object>> nodes = ((java.util.Map<Integer,java.util.Map<String,Object>>)(new java.util.LinkedHashMap<Integer, java.util.Map<String,Object>>()));
    static int head = 0 - 1;
    static int tail = 0 - 1;
    static String out = "From tail:";
    static int id = tail;

    static String listString() {
        if (head == 0 - 1) {
            return "<nil>";
        }
        String r = "[" + (String)(((Object)(((java.util.Map<String,Object>)(nodes).get(head))).get("value")));
        int id = (int)(((int)(((java.util.Map<String,Object>)(nodes).get(head))).getOrDefault("next", 0)));
        while (id != 0 - 1) {
            r = r + " " + (String)(((Object)(((java.util.Map<String,Object>)(nodes).get(id))).get("value")));
            id = (int)(((int)(((java.util.Map<String,Object>)(nodes).get(id))).getOrDefault("next", 0)));
        }
        r = r + "]";
        return r;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(listString());
nodes.put(0, new java.util.LinkedHashMap<String, Object>() {{ put("value", "A"); put("next", 0 - 1); put("prev", 0 - 1); }});
            head = 0;
            tail = 0;
nodes.put(1, new java.util.LinkedHashMap<String, Object>() {{ put("value", "B"); put("next", 0 - 1); put("prev", 0); }});
((java.util.Map<String,Object>)(nodes).get(0)).put("next", 1);
            tail = 1;
            System.out.println(listString());
nodes.put(2, new java.util.LinkedHashMap<String, Object>() {{ put("value", "C"); put("next", 1); put("prev", 0); }});
((java.util.Map<String,Object>)(nodes).get(1)).put("prev", 2);
((java.util.Map<String,Object>)(nodes).get(0)).put("next", 2);
            System.out.println(listString());
            while (id != 0 - 1) {
                out = out + " " + (String)(((Object)(((java.util.Map<String,Object>)(nodes).get(id))).get("value")));
                id = (int)(((int)(((java.util.Map<String,Object>)(nodes).get(id))).getOrDefault("prev", 0)));
            }
            System.out.println(out);
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
