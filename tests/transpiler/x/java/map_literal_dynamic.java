public class Main {
    static int x = 3;
    static int y = 4;
    static java.util.Map<String, Integer> m = java.util.Map.of("a", x, "b", y);
    public static void main(String[] args) {
        System.out.println(m["a"] + m["b"]);
    }
}
