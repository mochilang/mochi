public class Main {
    static java.util.Map m = new java.util.LinkedHashMap<>() {{ put(1, "a"); put(2, "b"); }};

    public static void main(String[] args) {
        System.out.println(m.containsKey(1));
        System.out.println(m.containsKey(3));
    }
}
