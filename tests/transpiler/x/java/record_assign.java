public class Main {
    static java.util.Map c = new java.util.LinkedHashMap<String, Integer>() {{ put("n", 0); }};

    static void inc(java.util.Map c) {
        c = new java.util.LinkedHashMap(c);
c.put("n", ((Integer) (c.get("n"))) + 1);
    }
    public static void main(String[] args) {
        inc(c);
        System.out.println(((Integer) (c.get("n"))));
    }
}
