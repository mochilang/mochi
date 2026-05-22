public class Main {

    static String q(Object v) {
        if (v instanceof String) return "'" + v.toString() + "'";
        return String.valueOf(v);
    }

    public static void main(String[] args) {
        System.out.println((java.util.Arrays.stream(new int[]{1, 2, 3}).average().orElse(0)));
    }
}
