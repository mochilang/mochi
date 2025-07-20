public class Main {
    static java.util.Map m = new java.util.LinkedHashMap<>() {{ put(a, 1); put(b, 2); }};

    public static void main(String[] args) {
        json(m);
    }
}
