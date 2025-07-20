public class Main {
    static int x = 2;
    static String label = x == 1 ? "one" : x == 2 ? "two" : x == 3 ? "three" : "unknown";

    public static void main(String[] args) {
        System.out.println(label);
    }
}
