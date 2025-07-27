public class Main {

    static String toOct(int n) {
        if (n == 0) {
            return "0";
        }
        String digits = "01234567";
        String out = "";
        int v = n;
        while (v > 0) {
            int d = Math.floorMod(v, 8);
            out = digits.substring(d, d + 1) + out;
            v = v / 8;
        }
        return out;
    }

    static void main() {
        for (int i = 0; i < 16; i++) {
            System.out.println(toOct(i));
        }
    }
    public static void main(String[] args) {
        main();
    }
}
