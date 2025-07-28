public class Main {

    static java.util.Map<String,Object> newList() {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("nodes", new java.util.LinkedHashMap<String, Object>()), java.util.Map.entry("head", 0), java.util.Map.entry("tail", 0), java.util.Map.entry("nextID", 1)));
    }

    static java.util.Map<String,Object> newNode(java.util.Map<String,Object> l, Object v) {
        int id = (int)(((int)(l).getOrDefault("nextID", 0)));
l.put("nextID", id + 1);
        java.util.Map<Integer,java.util.Map<String,Object>> nodes = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
        java.util.Map<String,Object> n = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("id", id), java.util.Map.entry("value", v), java.util.Map.entry("next", 0), java.util.Map.entry("prev", 0)))));
nodes.put(id, n);
l.put("nodes", nodes);
        return n;
    }

    static java.util.Map<String,Object> pushFront(java.util.Map<String,Object> l, Object v) {
        java.util.Map<String,Object> n = newNode(l, v);
n.put("next", (Object)(((Object)(l).get("head"))));
        if ((((int)(l).getOrDefault("head", 0))) != 0) {
            java.util.Map<Integer,java.util.Map<String,Object>> nodes = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
            java.util.Map<String,Object> h = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(((int)(l).getOrDefault("head", 0)))));
h.put("prev", (Object)(((int) (n.get("id")))));
nodes.put(((int)(h).getOrDefault("id", 0)), h);
l.put("nodes", nodes);
        } else {
l.put("tail", (Object)(((int) (n.get("id")))));
        }
l.put("head", (Object)(((int) (n.get("id")))));
        java.util.Map<Integer,java.util.Map<String,Object>> nodes2 = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
nodes2.put(((Number)(((int) (n.get("id"))))).intValue(), n);
l.put("nodes", nodes2);
        return n;
    }

    static java.util.Map<String,Object> pushBack(java.util.Map<String,Object> l, Object v) {
        java.util.Map<String,Object> n = newNode(l, v);
n.put("prev", (Object)(((Object)(l).get("tail"))));
        if ((((int)(l).getOrDefault("tail", 0))) != 0) {
            java.util.Map<Integer,java.util.Map<String,Object>> nodes = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
            java.util.Map<String,Object> t = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(((int)(l).getOrDefault("tail", 0)))));
t.put("next", (Object)(((int) (n.get("id")))));
nodes.put(((int)(t).getOrDefault("id", 0)), t);
l.put("nodes", nodes);
        } else {
l.put("head", (Object)(((int) (n.get("id")))));
        }
l.put("tail", (Object)(((int) (n.get("id")))));
        java.util.Map<Integer,java.util.Map<String,Object>> nodes2 = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
nodes2.put(((Number)(((int) (n.get("id"))))).intValue(), n);
l.put("nodes", nodes2);
        return n;
    }

    static java.util.Map<String,Object> insertBefore(java.util.Map<String,Object> l, int refID, Object v) {
        if (refID == 0) {
            return pushFront(l, v);
        }
        java.util.Map<Integer,java.util.Map<String,Object>> nodes = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
        java.util.Map<String,Object> ref = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(refID)));
        java.util.Map<String,Object> n = newNode(l, v);
n.put("prev", (Object)(((Object)(ref).get("prev"))));
n.put("next", (Object)(((Object)(ref).get("id"))));
        if ((((int)(ref).getOrDefault("prev", 0))) != 0) {
            java.util.Map<String,Object> p = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(((int)(ref).getOrDefault("prev", 0)))));
p.put("next", (Object)(((int) (n.get("id")))));
nodes.put(((int)(p).getOrDefault("id", 0)), p);
        } else {
l.put("head", (Object)(((int) (n.get("id")))));
        }
ref.put("prev", (Object)(((int) (n.get("id")))));
nodes.put(refID, ref);
nodes.put(((Number)(((int) (n.get("id"))))).intValue(), n);
l.put("nodes", nodes);
        return n;
    }

    static java.util.Map<String,Object> insertAfter(java.util.Map<String,Object> l, int refID, Object v) {
        if (refID == 0) {
            return pushBack(l, v);
        }
        java.util.Map<Integer,java.util.Map<String,Object>> nodes = (java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map<Integer,java.util.Map<String,Object>>)(l).get("nodes")));
        java.util.Map<String,Object> ref = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(refID)));
        java.util.Map<String,Object> n = newNode(l, v);
n.put("next", (Object)(((Object)(ref).get("next"))));
n.put("prev", (Object)(((Object)(ref).get("id"))));
        if ((((int)(ref).getOrDefault("next", 0))) != 0) {
            java.util.Map<String,Object> nx = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(((int)(ref).getOrDefault("next", 0)))));
nx.put("prev", (Object)(((int) (n.get("id")))));
nodes.put(((int)(nx).getOrDefault("id", 0)), nx);
        } else {
l.put("tail", (Object)(((int) (n.get("id")))));
        }
ref.put("next", (Object)(((int) (n.get("id")))));
nodes.put(refID, ref);
nodes.put(((Number)(((int) (n.get("id"))))).intValue(), n);
l.put("nodes", nodes);
        return n;
    }

    static void main() {
        java.util.Map<String,Object> l = newList();
        java.util.Map<String,Object> e4 = pushBack(l, 4);
        java.util.Map<String,Object> e1 = pushFront(l, 1);
        insertBefore(l, (int)(((int)(e4).getOrDefault("id", 0))), 3);
        insertAfter(l, (int)(((int)(e1).getOrDefault("id", 0))), "two");
        int id = ((Number)(((int) (l.get("head"))))).intValue();
        java.util.Map<Integer,java.util.Map<String,Object>> nodes = ((java.util.Map<Integer,java.util.Map<String,Object>>)(((java.util.Map) (l.get("nodes")))));
        while (id != 0) {
            java.util.Map<String,Object> node = (java.util.Map<String,Object>)(((java.util.Map<String,Object>)(nodes).get(id)));
            System.out.println(String.valueOf(((Object)(node).get("value"))));
            id = (int)(((int)(node).getOrDefault("next", 0)));
        }
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
