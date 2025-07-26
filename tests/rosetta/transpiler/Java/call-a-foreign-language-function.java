public class Main {

    static String strdup(String s) {
        return s + "";
    }

    static void main() {
        String go1 = "hello C";
        String c2 = String.valueOf(strdup(go1));
        System.out.println(c2);
    }
    public static void main(String[] args) {
        main();
    }
}
