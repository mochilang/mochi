public class Main {
    static int x = 3;
    static int y = 4;
    static java.util.Map m = new java.util.LinkedHashMap<>() {{ put("a", x); put("b", y); }};

    public static void main(String[] args) {
        System.out.println(m.get("a") + " " + m.get("b"));
    }
}
