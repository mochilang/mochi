public class Main {

    static java.util.Map<String,Integer> removeKey(java.util.Map<String,Integer> m, String k) {
        java.util.Map<String,Integer> out = new java.util.LinkedHashMap<String, Integer>();
        for (String key : m.keySet()) {
            if (!(key.equals(k))) {
out.put(key, ((int)m.getOrDefault(key, 0)));
            }
        }
        return out;
    }

    static void main() {
        java.util.Map<String,Integer> x = null;
        x = new java.util.LinkedHashMap<String, Integer>();
x.put("foo", 3);
        int y1 = (int)(((int)x.getOrDefault("bar", 0)));
        Object ok = x.containsKey("bar");
        System.out.println(y1);
        System.out.println(ok);
        x = removeKey(x, "foo");
        x = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("foo", 2), java.util.Map.entry("bar", 42), java.util.Map.entry("baz", -1)));
        System.out.println(String.valueOf(String.valueOf(((int)x.getOrDefault("foo", 0))) + " " + (int)(((int)x.getOrDefault("bar", 0)))) + " " + (int)(((int)x.getOrDefault("baz", 0))));
    }
    public static void main(String[] args) {
        main();
    }
}
