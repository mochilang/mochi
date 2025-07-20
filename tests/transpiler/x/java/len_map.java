public class Main {

    public static void main(String[] args) {
        System.out.println(new java.util.LinkedHashMap<>() {{ put("a", 1); put("b", 2); }}.size());
    }
}
