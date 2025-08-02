public class Main {

    static int factorial(int n) {
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }

    static boolean isPrime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        int d = 3;
        while (d * d <= n) {
            if (Math.floorMod(n, d) == 0) {
                return false;
            }
            d = d + 2;
        }
        return true;
    }

    static String padLeft(String s, int w) {
        String out = s;
        while (_runeLen(out) < w) {
            out = " " + out;
        }
        return out;
    }

    static void main() {
        int n = 0;
        int count = 0;
        while (count < 10) {
            n = n + 1;
            int f = factorial(n);
            if (((Boolean)(isPrime(f - 1)))) {
                count = count + 1;
                System.out.println(String.valueOf(padLeft((String)(_p(count)), 2)) + ": " + String.valueOf(padLeft((String)(_p(n)), 2)) + "! - 1 = " + (String)(_p(f - 1)));
            }
            if (count < 10 && ((Boolean)(isPrime(f + 1)))) {
                count = count + 1;
                System.out.println(String.valueOf(padLeft((String)(_p(count)), 2)) + ": " + String.valueOf(padLeft((String)(_p(n)), 2)) + "! + 1 = " + (String)(_p(f + 1)));
            }
        }
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
