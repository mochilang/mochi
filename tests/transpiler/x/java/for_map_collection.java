public class Main {
    static java.util.Map<String, Integer> m = java.util.Map.of("a", 1, "b", 2);

    public static void main(String[] args) {
        for (var k : m.keySet()) {
            System.out.println(k);
        }
    }
}
