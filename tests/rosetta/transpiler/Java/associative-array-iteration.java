public class Main {

    static void main() {
        java.util.Map<String,Integer> m = new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("hello", 13), java.util.Map.entry("world", 31), java.util.Map.entry("!", 71)));
        for (var k : new java.util.ArrayList<>(m.keySet())) {
            System.out.println(String.valueOf(String.valueOf("key = " + String.valueOf(k)) + ", value = ") + String.valueOf(((int)m.getOrDefault(k, 0))));
        }
        for (var k : new java.util.ArrayList<>(m.keySet())) {
            System.out.println("key = " + String.valueOf(k));
        }
        for (var k : new java.util.ArrayList<>(m.keySet())) {
            System.out.println("value = " + String.valueOf(((int)m.getOrDefault(k, 0))));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
