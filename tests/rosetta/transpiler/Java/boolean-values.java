public class Main {

    static boolean parseBool(String s) {
        String l = s.toLowerCase();
        if ((l.equals("1")) || (l.equals("t")) || (l.equals("true")) || (l.equals("yes")) || (l.equals("y"))) {
            return true;
        }
        return false;
    }

    static void main() {
        boolean n = true;
        System.out.println(n ? "True" : "False");
        System.out.println("bool");
        n = !n;
        System.out.println(n ? "True" : "False");
        int x = 5;
        int y = 8;
        System.out.println("x == y:" + " " + String.valueOf(x == y ? 1 : 0));
        System.out.println("x < y:" + " " + String.valueOf(x < y ? 1 : 0));
        System.out.println("\nConvert String into Boolean Data type\n");
        String str1 = "japan";
        System.out.println("Before :" + " " + "string");
        boolean bolStr = parseBool(str1);
        System.out.println("After :" + " " + "bool");
    }
    public static void main(String[] args) {
        main();
    }
}
