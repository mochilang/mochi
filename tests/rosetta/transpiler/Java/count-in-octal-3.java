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
        double i = 0.0;
        while (true) {
            System.out.println(toOct(((Number)(i)).intValue()));
            if (i == 3.0) {
                i = 9007199254740992.0 - 4.0;
                System.out.println("...");
            }
            double next = i + 1.0;
            if (next == i) {
                break;
            }
            i = next;
        }
    }
    public static void main(String[] args) {
        main();
    }
}
