public class Main {
    static java.util.Map m = new java.util.LinkedHashMap<>() {{ put("a", 1); put("b", 2); put("c", 3); }};

    public static void main(String[] args) {
        System.out.println(values(m));
    }
}
