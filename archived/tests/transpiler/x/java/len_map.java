public class Main {

    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    static String boolStr(Object v) {
        if (v instanceof Boolean b) return b ? "True" : "False";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        System.out.println(boolStr(new java.util.LinkedHashMap<String, Integer>() {{ put("a", 1); put("b", 2); }}.size()));
    }
}
