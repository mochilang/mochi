public class Main {

    static java.util.Map<String,Object> Node(int data) {
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("Data", data), java.util.Map.entry("Balance", 0), java.util.Map.entry("Link", new Object[]{null, null})));
    }

    static Object getLink(java.util.Map<String,Object> n, int dir) {
        return (((Object[])n.get("Link")))[dir];
    }

    static void setLink(java.util.Map<String,Object> n, int dir, Object v) {
        Object[] links = (Object[])(((Object[])n.get("Link")));
links[dir] = v;
n.put("Link", links);
    }

    static int opp(int dir) {
        return 1 - dir;
    }

    static java.util.Map<String,Object> single(java.util.Map<String,Object> root, int dir) {
        Object tmp = getLink(root, opp(dir));
        setLink(root, opp(dir), getLink(tmp, dir));
        setLink(tmp, dir, root);
        return tmp;
    }

    static java.util.Map<String,Object> double_(java.util.Map<String,Object> root, int dir) {
        Object tmp = getLink(getLink(root, opp(dir)), dir);
        setLink(getLink(root, opp(dir)), dir, getLink(tmp, opp(dir)));
        setLink(tmp, opp(dir), getLink(root, opp(dir)));
        setLink(root, opp(dir), tmp);
        tmp = getLink(root, opp(dir));
        setLink(root, opp(dir), getLink(tmp, dir));
        setLink(tmp, dir, root);
        return tmp;
    }

    static void adjustBalance(java.util.Map<String,Object> root, int dir, int bal) {
        java.util.Map<String,Object> n = ((java.util.Map<String,Object>)(getLink(root, dir)));
        java.util.Map<String,Object> nn = ((java.util.Map<String,Object>)(getLink(n, opp(dir))));
        if (((Number)(((Object)nn.get("Balance")))).intValue() == 0) {
root.put("Balance", 0);
n.put("Balance", 0);
        } else         if (((Number)(((Object)nn.get("Balance")))).intValue() == bal) {
root.put("Balance", -bal);
n.put("Balance", 0);
        } else {
root.put("Balance", 0);
n.put("Balance", bal);
        }
nn.put("Balance", 0);
    }

    static java.util.Map<String,Object> insertBalance(java.util.Map<String,Object> root, int dir) {
        java.util.Map<String,Object> n = ((java.util.Map<String,Object>)(getLink(root, dir)));
        int bal = 2 * dir - 1;
        if (((Number)(((Object)n.get("Balance")))).intValue() == bal) {
root.put("Balance", 0);
n.put("Balance", 0);
            return single(root, opp(dir));
        }
        adjustBalance(root, dir, bal);
        return double_(root, opp(dir));
    }

    static java.util.Map<String,Object> insertR(Object root, int data) {
        if ((root == null)) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", Node(data)), java.util.Map.entry("done", false)));
        }
        java.util.Map<String,Object> node = ((java.util.Map<String,Object>)(root));
        int dir = 0;
        if ((((int)node.getOrDefault("Data", 0))) < data) {
            dir = 1;
        }
        java.util.Map<String,Object> r = insertR(getLink(node, dir), data);
        setLink(node, dir, (Object)(((Object)r.get("node"))));
        if (((Object)r.get("done"))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", true)));
        }
node.put("Balance", (((int)node.getOrDefault("Balance", 0))) + (2 * dir - 1));
        if (((Number)(((Object)node.get("Balance")))).intValue() == 0) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", true)));
        }
        if (((Number)(((Object)node.get("Balance")))).intValue() == 1 || (boolean)(((Object)node.get("Balance"))) == (-1)) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", false)));
        }
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", insertBalance(node, dir)), java.util.Map.entry("done", true)));
    }

    static Object Insert(Object tree, int data) {
        java.util.Map<String,Object> r = insertR(tree, data);
        return ((java.util.Map<String,Object>)r.get("node"));
    }

    static java.util.Map<String,Object> removeBalance(java.util.Map<String,Object> root, int dir) {
        java.util.Map<String,Object> n = ((java.util.Map<String,Object>)(getLink(root, opp(dir))));
        int bal = 2 * dir - 1;
        if (((Number)(((Object)n.get("Balance")))).intValue() == (-bal)) {
root.put("Balance", 0);
n.put("Balance", 0);
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", single(root, dir)), java.util.Map.entry("done", false)));
        }
        if (((Number)(((Object)n.get("Balance")))).intValue() == bal) {
            adjustBalance(root, opp(dir), (-bal));
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", double_(root, dir)), java.util.Map.entry("done", false)));
        }
root.put("Balance", -bal);
n.put("Balance", bal);
        return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", single(root, dir)), java.util.Map.entry("done", true)));
    }

    static java.util.Map<String,Object> removeR(Object root, int data) {
        if ((root == null)) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", null), java.util.Map.entry("done", false)));
        }
        java.util.Map<String,Object> node = ((java.util.Map<String,Object>)(root));
        if ((((int)node.getOrDefault("Data", 0))) == data) {
            if ((getLink(node, 0) == null)) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", getLink(node, 1)), java.util.Map.entry("done", false)));
            }
            if ((getLink(node, 1) == null)) {
                return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", getLink(node, 0)), java.util.Map.entry("done", false)));
            }
            Object heir = getLink(node, 0);
            while (!(getLink(heir, 1) == null)) {
                heir = getLink(heir, 1);
            }
node.put("Data", ((Object)((java.util.Map)heir).get("Data")));
            data = (int)(((int)((java.util.Map)heir).getOrDefault("Data", 0)));
        }
        int dir = 0;
        if ((((int)node.getOrDefault("Data", 0))) < data) {
            dir = 1;
        }
        java.util.Map<String,Object> r = removeR(getLink(node, dir), data);
        setLink(node, dir, (Object)(((java.util.Map<String,Object>)r.get("node"))));
        if (((boolean)r.getOrDefault("done", false))) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", true)));
        }
node.put("Balance", (((int)node.getOrDefault("Balance", 0))) + 1 - 2 * dir);
        if (((Number)(((Object)node.get("Balance")))).intValue() == 1 || (boolean)(((Object)node.get("Balance"))) == (-1)) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", true)));
        }
        if (((Number)(((Object)node.get("Balance")))).intValue() == 0) {
            return new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("node", node), java.util.Map.entry("done", false)));
        }
        return removeBalance(node, dir);
    }

    static Object Remove(Object tree, int data) {
        java.util.Map<String,Object> r = removeR(tree, data);
        return ((java.util.Map<String,Object>)r.get("node"));
    }

    static String indentStr(int n) {
        String s = "";
        int i = 0;
        while (i < n) {
            s = String.valueOf(s + " ");
            i = i + 1;
        }
        return s;
    }

    static void dumpNode(Object node, int indent, boolean comma) {
        String sp = String.valueOf(indentStr(indent));
        if ((node == null)) {
            String line = String.valueOf(sp + "null");
            if (comma) {
                line = String.valueOf(line + ",");
            }
            System.out.println(line);
        } else {
            System.out.println(sp + "{");
            System.out.println(String.valueOf(String.valueOf(String.valueOf(indentStr(indent + 3)) + "\"Data\": ") + String.valueOf(((Object)((java.util.Map)node).get("Data")))) + ",");
            System.out.println(String.valueOf(String.valueOf(String.valueOf(indentStr(indent + 3)) + "\"Balance\": ") + String.valueOf(((Object)((java.util.Map)node).get("Balance")))) + ",");
            System.out.println(String.valueOf(indentStr(indent + 3)) + "\"Link\": [");
            dumpNode(getLink(node, 0), indent + 6, true);
            dumpNode(getLink(node, 1), indent + 6, false);
            System.out.println(String.valueOf(indentStr(indent + 3)) + "]");
            String end = String.valueOf(sp + "}");
            if (comma) {
                end = String.valueOf(end + ",");
            }
            System.out.println(end);
        }
    }

    static void dump(Object node, int indent) {
        dumpNode(node, indent, false);
    }

    static void main() {
        Object tree = null;
        System.out.println("Empty tree:");
        dump(tree, 0);
        System.out.println("");
        System.out.println("Insert test:");
        tree = Insert(tree, 3);
        tree = Insert(tree, 1);
        tree = Insert(tree, 4);
        tree = Insert(tree, 1);
        tree = Insert(tree, 5);
        dump(tree, 0);
        System.out.println("");
        System.out.println("Remove test:");
        tree = Remove(tree, 3);
        tree = Remove(tree, 1);
        java.util.Map<String,Object> t = ((java.util.Map<String,Object>)(tree));
t.put("Balance", 0);
        tree = t;
        dump(tree, 0);
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
