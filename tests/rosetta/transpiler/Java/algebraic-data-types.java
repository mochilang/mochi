public class Main {
    static Object tr = null;
    static int i = 1;

    static java.util.Map<String,Object> node(String cl, Object le, int aa, Object ri) {
        return new java.util.LinkedHashMap<String, Object>() {{ put("cl", cl); put("le", le); put("aa", aa); put("ri", ri); }};
    }

    static String treeString(Object t) {
        if ((t == null)) {
            return "E";
        }
        java.util.Map<String,Object> m = ((java.util.Map<String,Object>)(t));
        return String.valueOf(String.valueOf(String.valueOf(String.valueOf(String.valueOf("T(" + (String)(((Object)m.get("cl")))) + ", " + treeString((Object)(((Object)m.get("le"))))) + ", ") + String.valueOf(((Object)m.get("aa")))) + ", " + treeString((Object)(((Object)m.get("ri"))))) + ")";
    }

    static Object balance(Object t) {
        if ((t == null)) {
            return t;
        }
        java.util.Map<String,Object> m = ((java.util.Map<String,Object>)(t));
        if (!(((Object)m.get("cl")).equals("B"))) {
            return t;
        }
        Object le = (Object)(((Object)m.get("le")));
        Object ri = (Object)(((Object)m.get("ri")));
        if (!(le == null)) {
            java.util.Map<String,Object> leMap = ((java.util.Map<String,Object>)(le));
            if ((((Object)leMap.get("cl")).equals("R"))) {
                Object lele = (Object)(((Object)leMap.get("le")));
                if (!(lele == null)) {
                    java.util.Map<String,Object> leleMap = ((java.util.Map<String,Object>)(lele));
                    if ((((Object)leleMap.get("cl")).equals("R"))) {
                        return node("R", node("B", (Object)(((Object)leleMap.get("le"))), (int)(((Object)leleMap.get("aa"))), (Object)(((Object)leleMap.get("ri")))), (int)(((Object)leMap.get("aa"))), node("B", (Object)(((Object)leMap.get("ri"))), (int)(((Object)m.get("aa"))), ri));
                    }
                }
                Object leri = (Object)(((Object)leMap.get("ri")));
                if (!(leri == null)) {
                    java.util.Map<String,Object> leriMap = ((java.util.Map<String,Object>)(leri));
                    if ((((Object)leriMap.get("cl")).equals("R"))) {
                        return node("R", node("B", (Object)(((Object)leMap.get("le"))), (int)(((Object)leMap.get("aa"))), (Object)(((Object)leriMap.get("le")))), (int)(((Object)leriMap.get("aa"))), node("B", (Object)(((Object)leriMap.get("ri"))), (int)(((Object)m.get("aa"))), ri));
                    }
                }
            }
        }
        if (!(ri == null)) {
            java.util.Map<String,Object> riMap = ((java.util.Map<String,Object>)(ri));
            if ((((Object)riMap.get("cl")).equals("R"))) {
                Object rile = (Object)(((Object)riMap.get("le")));
                if (!(rile == null)) {
                    java.util.Map<String,Object> rileMap = ((java.util.Map<String,Object>)(rile));
                    if ((((Object)rileMap.get("cl")).equals("R"))) {
                        return node("R", node("B", (Object)(((Object)m.get("le"))), (int)(((Object)m.get("aa"))), (Object)(((Object)rileMap.get("le")))), (int)(((Object)rileMap.get("aa"))), node("B", (Object)(((Object)rileMap.get("ri"))), (int)(((Object)riMap.get("aa"))), (Object)(((Object)riMap.get("ri")))));
                    }
                }
                Object riri = (Object)(((Object)riMap.get("ri")));
                if (!(riri == null)) {
                    java.util.Map<String,Object> ririMap = ((java.util.Map<String,Object>)(riri));
                    if ((((Object)ririMap.get("cl")).equals("R"))) {
                        return node("R", node("B", (Object)(((Object)m.get("le"))), (int)(((Object)m.get("aa"))), (Object)(((Object)riMap.get("le")))), (int)(((Object)riMap.get("aa"))), node("B", (Object)(((Object)ririMap.get("le"))), (int)(((Object)ririMap.get("aa"))), (Object)(((Object)ririMap.get("ri")))));
                    }
                }
            }
        }
        return t;
    }

    static Object ins(Object tr, int x) {
        if ((tr == null)) {
            return node("R", null, x, null);
        }
        if (x < ((Number)(((Object)((java.util.Map)tr).get("aa")))).intValue()) {
            return balance(node((String)(((Object)((java.util.Map)tr).get("cl"))), ins((Object)(((Object)((java.util.Map)tr).get("le"))), x), (int)(((Object)((java.util.Map)tr).get("aa"))), (Object)(((Object)((java.util.Map)tr).get("ri")))));
        }
        if (x > ((Number)(((Object)((java.util.Map)tr).get("aa")))).intValue()) {
            return balance(node((String)(((Object)((java.util.Map)tr).get("cl"))), (Object)(((Object)((java.util.Map)tr).get("le"))), (int)(((Object)((java.util.Map)tr).get("aa"))), ins((Object)(((Object)((java.util.Map)tr).get("ri"))), x)));
        }
        return tr;
    }

    static Object insert(Object tr, int x) {
        Object t = ins(tr, x);
        if ((t == null)) {
            return null;
        }
        java.util.Map<String,Object> m = ((java.util.Map<String,Object>)(t));
        return node("B", (Object)(((Object)m.get("le"))), (int)(((Object)m.get("aa"))), (Object)(((Object)m.get("ri"))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i <= 16) {
                tr = insert(tr, i);
                i = i + 1;
            }
            System.out.println(treeString(tr));
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
