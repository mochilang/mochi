public class Main {
    static java.util.Map<String, Integer> data = java.util.Map.of("outer", java.util.Map.of("inner", 1));
    public static void main(String[] args) {
        System.out.println(data["outer"]["inner"]);
    }
}
