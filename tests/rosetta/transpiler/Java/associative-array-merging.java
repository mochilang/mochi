public class Main {

    static java.util.Map<String,Object> merge(java.util.Map<String,Object> base, java.util.Map<String,Object> update) {
        java.util.Map<String,Object> result = new java.util.LinkedHashMap<String, Object>();
        for (String k : base.keySet()) {
result.put(k, ((Object)base.get(k)));
        }
        for (String k : update.keySet()) {
result.put(k, ((Object)update.get(k)));
        }
        return result;
    }

    static void main() {
        java.util.Map<String,Object> base = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("name", "Rocket Skates"), java.util.Map.entry("price", 12.75), java.util.Map.entry("color", "yellow")));
        java.util.Map<String,Object> update = new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("price", 15.25), java.util.Map.entry("color", "red"), java.util.Map.entry("year", 1974)));
        java.util.Map<String,Object> result = merge(base, update);
        System.out.println(result);
    }
    public static void main(String[] args) {
        main();
    }
}
