public class Main {

    static String quibble(String[] items) {
        int n = items.length;
        if (n == 0) {
            return "{}";
        } else         if (n == 1) {
            return "{" + items[0] + "}";
        } else         if (n == 2) {
            return "{" + items[0] + " and " + items[1] + "}";
        } else {
            String prefix = "";
            for (int i = 0; i < n - 1; i++) {
                if (i == n - 1) {
                    break;
                }
                if (i > 0) {
                    prefix = prefix + ", ";
                }
                prefix = prefix + items[i];
            }
            return "{" + prefix + " and " + items[n - 1] + "}";
        }
    }

    static void main() {
        System.out.println(quibble(new String[]{}));
        System.out.println(quibble(new String[]{"ABC"}));
        System.out.println(quibble(new String[]{"ABC", "DEF"}));
        System.out.println(quibble(new String[]{"ABC", "DEF", "G", "H"}));
    }
    public static void main(String[] args) {
        main();
    }
}
